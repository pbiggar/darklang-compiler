(* RegisterAllocation.ml - Orchestrate integer and floating allocation and phi elimination. *)
[@@@warning "-4"]
open AllocationModel
open RegisterFacts
open RegisterLiveness
open RegisterPolicy
open RegisterInterference
open RegisterCoalescing
open RegisterColoring
open SpillOperands
open ApplyBlockAllocation
open PhiResolution
module F=FloatAllocation
module A=ARM64CalleeClobbers
module RegMap=Map.Make(struct type t=LIR.physReg let compare=Stdlib.compare end)
module RegSet=Set.Make(struct type t=LIR.physReg let compare=Stdlib.compare end)
module FRegMap=Map.Make(struct type t=LIR.physFPReg let compare=Stdlib.compare end)
module FRegSet=Set.Make(struct type t=LIR.physFPReg let compare=Stdlib.compare end)
let physRegName reg=List.assoc reg [LIR.X0,"X0";LIR.X1,"X1";LIR.X2,"X2";LIR.X3,"X3";LIR.X4,"X4";LIR.X5,"X5";LIR.X6,"X6";LIR.X7,"X7";LIR.X8,"X8";LIR.X9,"X9";LIR.X10,"X10";LIR.X11,"X11";LIR.X12,"X12";LIR.X13,"X13";LIR.X14,"X14";LIR.X15,"X15";LIR.X16,"X16";LIR.X17,"X17";LIR.X19,"X19";LIR.X20,"X20";LIR.X21,"X21";LIR.X22,"X22";LIR.X23,"X23";LIR.X24,"X24";LIR.X25,"X25";LIR.X26,"X26";LIR.X27,"X27";LIR.X29,"X29";LIR.X30,"X30";LIR.SP,"SP"]
let physFPRegName reg="D"^string_of_int (F.physFPRegToInt reg)
(*
   Main Entry Point
   Parameter registers per ARM64 calling convention (X0-X7 for ints, D0-D7 for floats)
*)
let parameterRegs=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7]
let floatParamRegs=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7]
let appendTiming phase elapsedMs timings=timings @ [{phase;elapsedMs}]
let bestCostGap costs=match List.sort Stdlib.compare costs with best::second::_ -> Int32.to_int (Int32.sub (Int32.of_int second) (Int32.of_int best)) | _ -> Crash.crash "Call-aware allocation requires at least two caller registers"
(*
   Allocate registers for a function
*)
let timePhase clock phase timings action=match clock with None -> let result=action () in result,timings | Some now -> let start=now () in let result=action () in let elapsedMs=now ()-.start in result,appendTiming phase elapsedMs timings
let distinct values=List.fold_left (fun acc value -> if List.mem value acc then acc else acc @ [value]) [] values
let minBy key values=match values with [] -> invalid_arg "The input sequence was empty. (Parameter 'list')" | first::rest -> List.fold_left (fun best value -> if Stdlib.compare (key value) (key best)<0 then value else best) first rest
(*
   Assign the colors that actually span calls to preserved registers. Coloring
   still determines interference; this permutation only chooses which physical
   register represents each color, so it cannot create a new conflict.
*)
let chooseRegistersForCalls arch calleeWrites blocks classifiedBlocks domain floatDomain liveness floatLiveness (allocation:allocationResult) =
 let snapshots idx block=computeSaveRegsPreparation domain floatDomain block classifiedBlocks.(idx).instrFacts liveness.(idx).liveOut floatLiveness.(idx).liveOut in
 let liveRegs live=List.filter_map (fun index -> match allocation.allocations.(index) with Some (PhysReg reg) -> Some reg | _ -> None) (Bitset.indicesToList live) in
 let callLiveRegs=Array.to_list (Array.mapi (fun idx block -> List.concat_map (fun (liveInts,_) -> liveRegs liveInts) (snapshots idx block)) blocks) |> List.concat in
 let callCounts=List.fold_left (fun acc reg -> RegMap.add reg (1+Option.value (RegMap.find_opt reg acc) ~default:0) acc) RegMap.empty callLiveRegs in
 let usedRegs=Array.to_list allocation.allocations |> List.filter_map (function Some (PhysReg reg) -> Some reg | _ -> None) |> distinct in
 let callCount reg=Option.value (RegMap.find_opt reg callCounts) ~default:0 in
 let ordered=List.stable_sort (fun left right -> Stdlib.compare (-callCount left,left) (-callCount right,right)) usedRegs in
 let calleeRegs=calleeSavedRegsFor arch in
 let mustUseCallee=max 0 (List.length ordered-List.length callerSavedRegs) in
 let crossingCount=List.length (List.filter (fun reg -> callCount reg>0) ordered) in
 let targetRegs,sourceRegs=match calleeWrites with
 | None -> let calleeCount=min (List.length calleeRegs) (max mustUseCallee crossingCount) in List.take calleeCount calleeRegs @ List.take (List.length ordered-calleeCount) callerSavedRegs,ordered
 | Some callees ->
  let callSites=Array.to_list (Array.mapi (fun idx block ->
   let snapshots=snapshots idx block in
   let writes=match arch with Platform.ARM64 -> A.callWritesForSaves callees block | Platform.X86_64 -> X64CalleeClobbers.callWritesForSaves callees block in
   if List.length snapshots<>List.length writes then Crash.crash "Call liveness and clobber envelopes disagree";
   List.map (fun ((liveInts,_),writes) -> RegSet.of_list (liveRegs liveInts),writes) (List.combine snapshots writes)) blocks) |> List.concat in
  let callerCost color reg=List.fold_left (fun cost (liveColors,writes) -> cost+(if RegSet.mem color liveColors && A.containsInt reg writes then 1 else 0)) 0 callSites in
  let bestCallerCost color=List.map (callerCost color) callerSavedRegs |> minBy Fun.id in
  let crossesFullClobber color=List.exists (fun (liveColors,writes) -> RegSet.mem color liveColors && List.for_all (fun reg -> A.containsInt reg writes) callerSavedRegs) callSites in
  let calleeCount=min (List.length calleeRegs) (max mustUseCallee (List.length (List.filter (fun color -> crossesFullClobber color || bestCallerCost color>1) ordered))) in
  let byNeed=List.stable_sort (fun left right -> Stdlib.compare (not (crossesFullClobber left),-bestCallerCost left,-callCount left,left) (not (crossesFullClobber right),-bestCallerCost right,-callCount right,right)) ordered in
  let calleeColors=List.take calleeCount byNeed in let callerColors=List.drop calleeCount byNeed in
  let sorted=List.stable_sort (fun left right -> let key color=(-(bestCostGap (List.map (callerCost color) callerSavedRegs)),-callCount color,color) in Stdlib.compare (key left) (key right)) callerColors in
  let _,callerAssignments=List.fold_left (fun (available,assignments) color -> let chosen=minBy (fun reg -> callerCost color reg,reg) available in List.filter ((<>) chosen) available,(color,chosen)::assignments) (callerSavedRegs,[]) sorted in
  let calleeAssignments=List.combine calleeColors (List.take calleeCount calleeRegs) in
  let remap=RegMap.of_list (calleeAssignments @ callerAssignments) in
  let targetRegs=List.map (fun color -> match RegMap.find_opt color remap with Some reg -> reg | None -> Crash.crash ("Missing call-aware color for "^physRegName color)) ordered in targetRegs,ordered in
 let remap=RegMap.of_list (List.combine sourceRegs targetRegs) in
 let remappedAllocations=Array.map (function Some (PhysReg reg) -> (match RegMap.find_opt reg remap with Some mapped -> Some (PhysReg mapped) | None -> Crash.crash ("Missing register remapping for "^physRegName reg)) | other -> other) allocation.allocations in
 {allocation with allocations=remappedAllocations;usedCalleeSaved=List.sort Stdlib.compare (List.filter (fun reg -> List.mem reg calleeRegs) targetRegs)}
let chooseArm64FloatRegistersForCalls callees blocks classifiedBlocks intDomain floatDomain intLiveness floatLiveness (allocation:F.fAllocationResult) =
 let callSites=Array.to_list (Array.mapi (fun idx block ->
  let snapshots=computeSaveRegsPreparation intDomain floatDomain block classifiedBlocks.(idx).instrFacts intLiveness.(idx).liveOut floatLiveness.(idx).liveOut in
  let writes=A.callWritesForSaves callees block in
  if List.length snapshots<>List.length writes then Crash.crash "ARM64 Float call liveness and clobber envelopes disagree";
  List.map (fun ((_,liveFloats),writes) ->
   let liveColors=Bitset.indicesToList liveFloats |> List.filter_map (fun index -> match allocation.F.allocations.(index) with Some (F.FPhysReg reg) -> Some reg | _ -> None) |> FRegSet.of_list in liveColors,writes) (List.combine snapshots writes)) blocks) |> List.concat in
 let usedRegs=Array.to_list allocation.F.allocations |> List.filter_map (function Some (F.FPhysReg reg) -> Some reg | _ -> None) |> distinct in
 let callCount color=List.fold_left (fun total (liveColors,_) -> total+(if FRegSet.mem color liveColors then 1 else 0)) 0 callSites in
 let callerCost color reg=List.fold_left (fun total (liveColors,writes) -> total+(if FRegSet.mem color liveColors && A.containsFloat reg writes then 1 else 0)) 0 callSites in
 let bestCallerCost color=List.map (callerCost color) F.floatCallerSavedRegs |> minBy Fun.id in
 let crossesFullClobber color=List.exists (fun (liveColors,writes) -> FRegSet.mem color liveColors && List.for_all (fun reg -> A.containsFloat reg writes) F.floatCallerSavedRegs) callSites in
 let mustUseCallee=max 0 (List.length usedRegs-List.length F.floatCallerSavedRegs) in
 let calleeCount=min (List.length F.floatCalleeSavedRegs) (max mustUseCallee (List.length (List.filter (fun color -> crossesFullClobber color || bestCallerCost color>1) usedRegs))) in
 let byNeed=List.stable_sort (fun left right -> let key color=not (crossesFullClobber color),-bestCallerCost color,-callCount color,color in Stdlib.compare (key left) (key right)) usedRegs in
 let calleeColors=List.take calleeCount byNeed in let callerColors=List.drop calleeCount byNeed in
 let sorted=List.stable_sort (fun left right -> let key color=(-(bestCostGap (List.map (callerCost color) F.floatCallerSavedRegs)),-callCount color,color) in Stdlib.compare (key left) (key right)) callerColors in
 let _,callerAssignments=List.fold_left (fun (available,assignments) color -> let chosen=minBy (fun reg -> callerCost color reg,reg) available in List.filter ((<>) chosen) available,(color,chosen)::assignments) (F.floatCallerSavedRegs,[]) sorted in
 let calleeAssignments=List.combine calleeColors (List.take calleeCount F.floatCalleeSavedRegs) in
 let remap=FRegMap.of_list (calleeAssignments @ callerAssignments) in
 let remapped=Array.map (function Some (F.FPhysReg reg) -> (match FRegMap.find_opt reg remap with Some mapped -> Some (F.FPhysReg mapped) | None -> Crash.crash ("Missing call-aware ARM64 Float color for "^physFPRegName reg)) | other -> other) allocation.F.allocations in
 {allocation with F.allocations=remapped;usedCalleeSavedF=Array.to_list remapped |> List.filter_map (function Some (F.FPhysReg reg) when List.mem reg F.floatCalleeSavedRegs -> Some reg | _ -> None) |> distinct |> List.sort Stdlib.compare}
let parameterAt index regs=try List.nth regs index with Failure _ | Invalid_argument _ -> invalid_arg "The index was outside the range of elements in the list. (Parameter 'index')"
(*
   Precompute parameter info with separate int/float counters (AAPCS64)
   Needed for entry defs and float allocation.
   Float parameter - uses D registers
   Int/other parameter - uses X registers
   Extract FVirtual IDs from float params for allocation
   Float params use Virtual register IDs that are also FVirtual IDs
   Step 1: Classify instructions once, then solve both liveness domains together.
   Step 2: Build interference graph
   Step 2b: Collect coalescing preferences and move pairs
   Step 3: Run chordal graph coloring with phi coalescing
   Use optimal register order based on calling pattern:
   - Functions with non-tail calls: callee-saved first (save once in prologue/epilogue)
   - Leaf functions / tail-call-only: caller-saved first (no prologue overhead)
   Step 3b: Parameter info already computed (needed for float allocation and param moves)
   Step 3c: Run float register allocation. Its liveness was solved with the
   integer domain above, including float parameters absent from the CFG.
   Step 5: Build mapping that copies INT parameters from X0-X7
   to wherever chordal graph coloring allocated them.
   IMPORTANT: Use proper parallel move resolution to handle cycles!
   (e.g., X1→X2 and X2→X1 require a temp register)
   Need to copy from paramReg to allocatedReg
   Store to stack - not a register move, handle separately
   We'll handle stack stores separately
   Same register or not in mapping
   Collect stack stores separately (they don't conflict with register moves)
   Use parallel move resolution for register-to-register moves
   Convert move actions to LIR instructions using X16 as temp register
   IMPORTANT: stack stores must happen BEFORE register shuffles.
   Otherwise a shuffle may clobber a source parameter register
   (for example X5) before we spill that original parameter value.
   Step 6: Build mapping that copies FLOAT parameters from D0-D7
   Float parameters use FVirtual registers (same ID as Virtual)
   and don't go through linear scan - they map directly in CodeGen
   IMPORTANT: Use parallel move resolution to handle cases where destination
   registers collide with source registers (e.g., when FVirtual id maps to D0
   which is also a source register for other params)
   Float param comes in D0/D1/etc, needs to be in FVirtual id
   Step 6b: Extract entry-edge phi moves for float phis
   For phis at the entry block, we need to add moves from entry-edge sources
   to phi destinations. These moves don't get added by resolvePhiNodes because
   there's no predecessor block for "before function entry".
   Find sources where the predecessor label doesn't exist in the CFG
   (these are entry-edge sources)
   Generate moves for entry-edge sources
   Generate FMov instructions for entry-edge phi resolution
   Use parallel move resolution to handle potential register conflicts
   Step 7: Resolve phi nodes (convert to moves at predecessor exits)
   This must happen BEFORE applying allocation since we need to know where each
   value is allocated to generate the correct moves
   Step 8: Apply allocation to CFG with liveness info for SaveRegs/RestoreRegs population
   Step 9: Insert parameter copy instructions at the start of the entry block
   Float param copies go first (they use separate register bank)
   Entry-edge phi moves come after param copies (they copy from param FVirtual to phi dest FVirtual)
   IMPORTANT: Apply float allocation to param copy instructions since they were generated
   before applyFloatAllocationToCFG ran and still contain FVirtual registers
   Step 10: Set integer parameters to their calling convention registers.
   AAPCS64 uses separate counters for X and D argument registers; floats are
   skipped here because float setup is emitted by the allocator's FMovs.
*)
let allocateRegistersInternal arch calleeWrites clock (func:LIR.functionDef) =
 let scheduledCFG=F.scheduleFloatLoadsInCFG func.LIR.cfg in
 let paramsWithTypes=List.map (fun (tp:LIR.typedLIRParam) -> tp.LIR.reg,tp.LIR.typ) func.LIR.typedParams in
 let _,_,intParams,floatParams=List.fold_left (fun (intIdx,floatIdx,intAcc,floatAcc) (reg,typ) -> if typ=AST.TFloat64 then intIdx,floatIdx+1,intAcc,(reg,floatIdx)::floatAcc else intIdx+1,floatIdx,(reg,intIdx)::intAcc,floatAcc) (0,0,[],[]) paramsWithTypes in
 let intParams=List.rev intParams in let floatParams=List.rev floatParams in
 let virtualIds params=List.filter_map (fun (reg,_) -> match reg with LIR.Virtual id -> Some id | LIR.Physical _ -> None) params in
 let intParamVRegIds=virtualIds intParams in let floatParamFVirtualIds=virtualIds floatParams in
 let floatParamPrecolors=List.filter_map (fun (reg,paramIdx) -> match reg with LIR.Virtual id -> Some (id,paramIdx) | LIR.Physical _ -> None) floatParams in
 let blockIndex,blocks=buildBlockIndex scheduledCFG in
 let (classifiedBlocks,domain,livenessBits,floatDomain,floatLiveness),timings=timePhase clock "RegAlloc: Liveness" [] (fun () -> let classifiedBlocks=classifyBlocks blocks in let domain,livenessBits,floatDomain,floatLiveness=computeCombinedLivenessBitsFromFacts blockIndex classifiedBlocks intParamVRegIds floatParamFVirtualIds in classifiedBlocks,domain,livenessBits,floatDomain,floatLiveness) in
 let intParamBits=vregBitsFromList domain intParamVRegIds in
 let graph,timings=timePhase clock "RegAlloc: Interference Graph" timings (fun () -> buildInterferenceGraphBitsetWithLiveness blockIndex classifiedBlocks domain livenessBits intParamBits) in
 let (preferences,movePairs),timings=timePhase clock "RegAlloc: Coalescing Prep" timings (fun () -> let phiPairs=collectPhiPairs blocks in let moves=collectMovePairs blocks in phiPairs,dedupePairs (moves @ phiPairs)) in
 let colorResult,timings=match clock with
 | None -> timePhase clock "RegAlloc: Coloring" timings (fun () -> let regs=getAllocatableRegs arch blocks in coloringToAllocation (chordalGraphColor graph [] (List.length regs) preferences movePairs) regs)
 | Some now ->
  let start=now () in let regs=getAllocatableRegs arch blocks in
  let colorResult,colorTiming=chordalGraphColorWithTiming now graph [] (List.length regs) preferences movePairs in
  let result=coloringToAllocation colorResult regs in let totalMs=now ()-.start in
  let timings=appendTiming "RegAlloc: Coloring" totalMs timings |> appendTiming "RegAlloc: Coloring - Coalesce" colorTiming.coalesceMs |> appendTiming "RegAlloc: Coloring - MCS" colorTiming.mcsMs |> appendTiming "RegAlloc: Coloring - Greedy" colorTiming.greedyMs |> appendTiming "RegAlloc: Coloring - Expand" colorTiming.expandMs in result,timings in
 let result=match arch with Platform.ARM64 -> chooseRegistersForCalls arch calleeWrites blocks classifiedBlocks domain floatDomain livenessBits floatLiveness colorResult | Platform.X86_64 when Option.is_some calleeWrites -> chooseRegistersForCalls arch calleeWrites blocks classifiedBlocks domain floatDomain livenessBits floatLiveness colorResult | Platform.X86_64 -> colorResult in
 let floatParamBits=vregBitsFromList floatDomain floatParamFVirtualIds in
 let floatAllocation,timings=timePhase clock "RegAlloc: Float Allocation" timings (fun () -> F.chordalFloatAllocationWithLiveness (F.allocatableFloatRegsFor arch) result.stackSize blockIndex blocks classifiedBlocks floatParamBits floatParamPrecolors floatDomain floatLiveness) in
 let floatAllocation=match arch,calleeWrites with Platform.ARM64,Some callees -> chooseArm64FloatRegistersForCalls callees blocks classifiedBlocks domain floatDomain livenessBits floatLiveness floatAllocation | _ -> floatAllocation in
 let (intParamCopyInstrs,floatParamCopyInstrs,entryEdgePhiInstrs),timings=timePhase clock "RegAlloc: Param Moves" timings (fun () ->
  let intParamMoves=List.filter_map (fun (reg,paramIdx) -> match reg with
   | LIR.Virtual id -> let paramReg=parameterAt paramIdx parameterRegs in (match tryAllocation result id with Some (PhysReg allocatedReg) when allocatedReg<>paramReg -> Some (allocatedReg,LIR.Reg (LIR.Physical paramReg)) | _ -> None)
   | LIR.Physical _ -> None) intParams in
  let intParamStackStores=List.filter_map (fun (reg,paramIdx) -> match reg with LIR.Virtual id -> let paramReg=parameterAt paramIdx parameterRegs in (match tryAllocation result id with Some (StackSlot offset) -> Some (LIR.Store (offset,LIR.Physical paramReg)) | _ -> None) | LIR.Physical _ -> None) intParams in
  let getSrcReg=function LIR.Reg (LIR.Physical r) -> Some r | _ -> None in
  let moveActions=ParallelMoves.resolve intParamMoves getSrcReg in
  let regMoveInstrs=List.concat_map (function ParallelMoves.SaveToTemp reg -> [LIR.Mov (LIR.Physical LIR.X16,LIR.Reg (LIR.Physical reg))] | ParallelMoves.Move (dest,src) -> [LIR.Mov (LIR.Physical dest,src)] | ParallelMoves.MoveFromTemp dest -> [LIR.Mov (LIR.Physical dest,LIR.Reg (LIR.Physical LIR.X16))]) moveActions in
  let intParamCopyInstrs=intParamStackStores @ regMoveInstrs in
  let floatParamMoves=List.filter_map (fun (reg,paramIdx) -> match reg with LIR.Virtual id -> let srcDReg=parameterAt paramIdx floatParamRegs in Some (LIR.FVirtual id,LIR.FPhysical srcDReg) | LIR.Physical _ -> None) floatParams in
  let floatParamCopyInstrs=generateFloatMoveInstrsWithAllocation floatParamMoves floatAllocation in
  let entryBlockBeforeResolution=blocks.(blockIndex.entryIndex) in
  let entryEdgeFloatPhiMoves=entryBlockBeforeResolution.LIR.instrs |> List.filter_map (function LIR.FPhi (dest,sources) -> Some (sources |> List.filter (fun (_,predLabel) -> Option.is_none (tryBlockIndex blockIndex predLabel)) |> List.map (fun (src,_) -> dest,src)) | _ -> None) |> List.concat in
  let entryEdgePhiInstrs=generateFloatMoveInstrsWithAllocation entryEdgeFloatPhiMoves floatAllocation in
  intParamCopyInstrs,floatParamCopyInstrs,entryEdgePhiInstrs) in
 let blocksWithPhiResolved,timings=timePhase clock "RegAlloc: Phi Resolution" timings (fun () -> if Array.exists (fun block -> block.hasPhiNodes) classifiedBlocks then resolvePhiNodes blockIndex blocks result floatAllocation else blocks) in
 let applyStart=Option.map (fun now -> now ()) clock in
 let blockPreparations,timings=timePhase clock "RegAlloc: Apply Preparation" timings (fun () -> prepareCFGAllocation blocksWithPhiResolved result floatAllocation livenessBits floatLiveness classifiedBlocks) in
 let allocatedBlocks,timings=timePhase clock "RegAlloc: Apply Rewrite" timings (fun () -> applyPreparedCFGAllocation arch blocksWithPhiResolved result floatAllocation blockPreparations) in
 let timings=match clock,applyStart with Some now,Some start -> appendTiming "RegAlloc: Apply Allocation" (now ()-.start) timings | _ -> timings in
 let (cfgWithParamCopies,allocatedTypedParams),timings=timePhase clock "RegAlloc: Finalize" timings (fun () ->
  let allocatedFloatParamCopyInstrs=List.concat_map (F.applyFloatAllocationToInstrs floatAllocation) floatParamCopyInstrs in
  let allocatedEntryEdgePhiInstrs=List.concat_map (F.applyFloatAllocationToInstrs floatAllocation) entryEdgePhiInstrs in
  let updatedBlocks=Array.copy allocatedBlocks in let entryBlock=updatedBlocks.(blockIndex.entryIndex) in
  let entryBlockWithCopies={entryBlock with LIR.instrs=allocatedFloatParamCopyInstrs @ allocatedEntryEdgePhiInstrs @ intParamCopyInstrs @ entryBlock.LIR.instrs} in
  updatedBlocks.(blockIndex.entryIndex)<-entryBlockWithCopies;
  let cfgWithParamCopies:LIR.cfg={LIR.entry=scheduledCFG.LIR.entry;blocks=blocksToMap blockIndex updatedBlocks} in
  let _,allocatedTypedParamsRev=List.fold_left (fun (intIdx,acc) (tp:LIR.typedLIRParam) -> if tp.LIR.typ=AST.TFloat64 then intIdx,{tp with LIR.reg=LIR.Physical LIR.X0}::acc else let paramReg=parameterAt intIdx parameterRegs in intIdx+1,{tp with LIR.reg=LIR.Physical paramReg}::acc) (0,[]) func.LIR.typedParams in
  cfgWithParamCopies,List.rev allocatedTypedParamsRev) in
 let allocatedFunc:LIR.functionDef={LIR.id=func.LIR.id;name=func.LIR.name;typedParams=allocatedTypedParams;cfg=cfgWithParamCopies;stackSize=floatAllocation.F.stackSize;usedCalleeSaved=result.usedCalleeSaved;codegenFacts=Option.map (fun facts -> {facts with LIR.arm64UsedCalleeSavedF=floatAllocation.F.usedCalleeSavedF}) func.LIR.codegenFacts} in
 let optimized=allocatedFunc |> LIR_Peephole.removePostAllocationMovesFromFunction |> LIR_Peephole.optimizeAllocatedCounterUpdates in optimized,timings
(*
   Allocate registers for a function
*)
let allocateRegisters arch func=fst (allocateRegistersInternal arch None None func)
let allocateRegistersWithCallSummaries arch callees func=fst (allocateRegistersInternal arch (Some callees) None func)
(*
   Allocate registers for a function and collect phase timings
*)
let allocateRegistersWithTiming arch func=allocateRegistersInternal arch None (Some (fun () -> Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)) func
