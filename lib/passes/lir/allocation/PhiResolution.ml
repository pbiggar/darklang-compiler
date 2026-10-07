(* PhiResolution.fs - Lower phi edges to allocation-aware parallel moves. *)
[@@@warning "-4"]
open AllocationModel
open RegisterFacts
open FloatAllocation
open SpillOperands
(*
   Float Move Generation (used by both phi resolution and param copies)
   Generate float move instructions using allocation-based register mapping.
   Uses the float allocation result instead of modulo-based mapping.
*)
let generateFloatMoveInstrsWithAllocation moves (floatAllocation:fAllocationResult) =
 let location = function LIR.FPhysical reg -> FPhysReg reg | LIR.FVirtual id -> (match tryFloatAllocation floatAllocation id with Some allocated -> allocated | None -> Crash.crash ("Float phi move vreg "^string_of_int id^" has no allocation")) in
 let locatedMoves=List.map (fun (dest,src) -> let d=location dest in let s=location src in d,s) moves in
 let sourceLocation = function FRematerialized _ -> None | (FPhysReg _ | FStackSlot _) as source -> Some source in
 let loadInto target = function FPhysReg src -> [LIR.FMov (target,LIR.FPhysical src)] | FStackSlot slot -> [LIR.FSpillLoad (target,slot)] | FRematerialized value -> [LIR.FLoad (target,value)] in
 let moveTo dest src = match dest,src with
 | FPhysReg reg,source -> loadInto (LIR.FPhysical reg) source
 | FStackSlot slot,FPhysReg reg -> [LIR.FSpillStore (slot,LIR.FPhysical reg)]
 | FStackSlot slot,FStackSlot sourceSlot -> [LIR.FSpillLoad (floatAllocation.spillScratchLeft,sourceSlot);LIR.FSpillStore (slot,floatAllocation.spillScratchLeft)]
 | FStackSlot slot,FRematerialized value -> [LIR.FLoad (floatAllocation.spillScratchLeft,value);LIR.FSpillStore (slot,floatAllocation.spillScratchLeft)]
 | FRematerialized _,_ -> Crash.crash "A rematerialized Float cannot be a phi destination" in
 ParallelMoves.resolve locatedMoves sourceLocation |> List.concat_map (function
 | ParallelMoves.SaveToTemp source -> loadInto floatAllocation.spillScratchRight source
 | ParallelMoves.Move (dest,src) -> moveTo dest src
 | ParallelMoves.MoveFromTemp dest -> (match dest,floatAllocation.spillScratchRight with
  | FPhysReg reg,scratch -> [LIR.FMov (LIR.FPhysical reg,scratch)]
  | FStackSlot slot,scratch -> [LIR.FSpillStore (slot,scratch)]
  | FRematerialized _,_ -> Crash.crash "A rematerialized Float cannot be a phi destination"))
(*
   Resolve phi nodes by inserting parallel moves at predecessor block exits.
   This function:
   1. Finds all phi nodes in each block
   2. Drops phis whose destination is never used
   3. For each predecessor, collects all (dest, src) pairs for moves
   4. Uses ParallelMoves.resolve to sequence the moves properly (handling cycles)
   5. Inserts the moves at the end of each predecessor (before terminator)
   6. Removes phi nodes from blocks
   Get the allocation for a virtual register (register or stack slot)
   Helper to convert a LIR.Operand to allocated version
   Collect all int phi info: for each phi, get (dest_reg, src_operand, pred_label)
   This gives us: List of (dest, sources, valueType)
   Collect all float phi info: (dest FReg, source FRegs with labels)
   Group int phis by predecessor index: List<(dest_allocation, src_operand)> per block index
   Keep the full Allocation type to handle both register and stack destinations
   Group float phis by predecessor index: List<(dest_freg, src_freg)> per block index
   Float registers don't go through allocation - FVirtual maps directly to D regs in CodeGen
   Generate move instructions for phi resolution using parallel move resolution
   across both register and stack destinations (handles reg<->stack cycles).
   Add moves to predecessor blocks
   Add int phi moves
   Add float phi moves
   IMPORTANT: For tail call blocks, the phi resolution is ALREADY handled by:
   1. FArgMoves: puts new values in D0-D7
   2. TailCall: jumps back to function entry
   3. Param copy at entry: copies D0-D7 to phi destination registers
   So we should SKIP phi resolution for tail call backedges - it's redundant and incorrect.
   For non-tail-call predecessors, append moves at the end as usual.
   Add phi moves at end of predecessor block
   Remove phi and fphi nodes from all blocks
*)
let resolvePhiNodes blockIndex blocks (allocation:allocationResult) floatAllocation =
 let neededDomain=allocation.AllocationModel.domain in
 let n=Array.length neededDomain.ids in let wordCount=neededDomain.wordCount in
 let phiSources=Array.init n (fun _ -> Bitset.empty wordCount) in
 Array.iter (fun (block:LIR.basicBlock) -> List.iter (function
 | LIR.Phi (LIR.Virtual destId,sources,_) -> (match tryIndexOf neededDomain destId with
  | Some destIdx -> List.iter (function LIR.Reg (LIR.Virtual srcId),_ -> (match tryIndexOf neededDomain srcId with Some srcIdx -> Bitset.addIndexInPlace srcIdx phiSources.(destIdx) | None -> ()) | _ -> ()) sources
  | None -> ()) | _ -> ()) block.LIR.instrs) blocks;
 let collectNonPhiUses blocks =
  let uses=Bitset.empty wordCount in
  Array.iter (fun (block:LIR.basicBlock) ->
   List.iter (function LIR.Phi _ | LIR.FPhi _ -> () | instr -> List.iter (fun id -> vregBitsAddInPlace neededDomain id uses) (getUsedVRegs instr)) block.LIR.instrs;
   List.iter (fun id -> vregBitsAddInPlace neededDomain id uses) (getTerminatorUsedVRegs block.LIR.terminator)) blocks;
  uses in
 let collectPhysicalPhiSources blocks =
  let uses=Bitset.empty wordCount in
  Array.iter (fun (block:LIR.basicBlock) -> List.iter (function
   | LIR.Phi (LIR.Physical _,sources,_) -> List.iter (function LIR.Reg (LIR.Virtual srcId),_ -> vregBitsAddInPlace neededDomain srcId uses | _ -> ()) sources
   | _ -> ()) block.LIR.instrs) blocks;
  uses in
 let computeNeededVRegs blocks =
  let rootUses=collectNonPhiUses blocks in
  Bitset.unionInPlace rootUses (collectPhysicalPhiSources blocks);
  let rec expand needed = function
   | [] -> needed
   | vIdx::rest -> let sources=phiSources.(vIdx) in let newSources=Bitset.diff sources needed in
    if Bitset.isEmpty newSources then expand needed rest else (
     Bitset.unionInPlace needed newSources;
     let worklist=Bitset.indicesToList newSources @ rest in expand needed worklist) in
  expand rootUses (Bitset.indicesToList rootUses) in
 let neededVRegs=computeNeededVRegs blocks in
 let phiDestNeeded = function LIR.Virtual id -> vregBitsContains neededDomain neededVRegs id | LIR.Physical _ -> true in
 let getDestAllocation = function LIR.Virtual id -> (match tryAllocation allocation id with Some alloc -> alloc | None -> Crash.crash ("RegisterAllocation: Virtual register "^string_of_int id^" not found in allocation")) | LIR.Physical p -> PhysReg p in
 let operandToAllocated = function
 | LIR.Reg (LIR.Virtual id) as op -> (match tryAllocation allocation id with Some (PhysReg r) -> LIR.Reg (LIR.Physical r) | Some (StackSlot offset) -> LIR.StackSlot offset | None -> op)
 | LIR.Reg (LIR.Physical p) -> LIR.Reg (LIR.Physical p) | op -> op in
 let intPhiInfo=Array.to_list blocks |> List.concat_map (fun (block:LIR.basicBlock) -> List.filter_map (function LIR.Phi (dest,sources,valueType) when phiDestNeeded dest -> Some (dest,sources,valueType) | _ -> None) block.LIR.instrs) in
 let floatPhiInfo=Array.to_list blocks |> List.concat_map (fun (block:LIR.basicBlock) -> List.filter_map (function LIR.FPhi (dest,sources) -> Some (dest,sources) | _ -> None) block.LIR.instrs) in
 let predecessorIntMoves=Array.init (Array.length blocks) (fun _ -> []) in
 List.iter (fun (dest,sources,_valueType) ->
  let destAlloc=getDestAllocation dest in
  List.iter (fun (src,predLabel) -> match tryBlockIndex blockIndex predLabel with Some predIdx -> let srcAllocated=operandToAllocated src in predecessorIntMoves.(predIdx)<-(destAlloc,srcAllocated)::predecessorIntMoves.(predIdx) | None -> ()) sources) intPhiInfo;
 let predecessorFloatMoves=Array.init (Array.length blocks) (fun _ -> []) in
 List.iter (fun (dest,sources) -> List.iter (fun (src,predLabel) -> match tryBlockIndex blockIndex predLabel with Some predIdx -> predecessorFloatMoves.(predIdx)<-(dest,src)::predecessorFloatMoves.(predIdx) | None -> ()) sources) floatPhiInfo;
 let generateIntMoveInstrs moves =
  let getSrcAllocation = function LIR.Reg (LIR.Physical p) -> Some (PhysReg p) | LIR.StackSlot offset -> Some (StackSlot offset) | _ -> None in
  let actions=ParallelMoves.resolve moves getSrcAllocation in
  let saveToTemp = function PhysReg r -> [LIR.Mov (LIR.Physical LIR.X16,LIR.Reg (LIR.Physical r))] | StackSlot offset -> [LIR.Mov (LIR.Physical LIR.X16,LIR.StackSlot offset)] in
  let moveFromTemp = function PhysReg r -> [LIR.Mov (LIR.Physical r,LIR.Reg (LIR.Physical LIR.X16))] | StackSlot offset -> [LIR.Store (offset,LIR.Physical LIR.X16)] in
  let moveToDest dest src = match dest with PhysReg r -> [LIR.Mov (LIR.Physical r,src)] | StackSlot offset -> (match src with LIR.Reg (LIR.Physical r) -> [LIR.Store (offset,LIR.Physical r)] | _ -> [LIR.Mov (LIR.Physical LIR.X16,src);LIR.Store (offset,LIR.Physical LIR.X16)]) in
  List.concat_map (function ParallelMoves.SaveToTemp loc -> saveToTemp loc | ParallelMoves.Move (dest,src) -> moveToDest dest src | ParallelMoves.MoveFromTemp dest -> moveFromTemp dest) actions in
 let updatedBlocks=Array.copy blocks in
 for predIdx=0 to Array.length updatedBlocks-1 do
  let moves=predecessorIntMoves.(predIdx) in
  if moves<>[] then (let predBlock=updatedBlocks.(predIdx) in let moveInstrs=generateIntMoveInstrs moves in updatedBlocks.(predIdx)<-{predBlock with LIR.instrs=predBlock.LIR.instrs @ moveInstrs})
 done;
 for predIdx=0 to Array.length updatedBlocks-1 do
  let moves=predecessorFloatMoves.(predIdx) in
  if moves<>[] then (let predBlock=updatedBlocks.(predIdx) in let moveInstrs=generateFloatMoveInstrsWithAllocation moves floatAllocation in updatedBlocks.(predIdx)<-{predBlock with LIR.instrs=predBlock.LIR.instrs @ moveInstrs})
 done;
 Array.map (fun (block:LIR.basicBlock) -> let filteredInstrs=List.filter (function LIR.Phi _ | LIR.FPhi _ -> false | _ -> true) block.LIR.instrs in {block with LIR.instrs=filteredInstrs}) updatedBlocks
