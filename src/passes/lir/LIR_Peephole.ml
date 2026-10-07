(*
   Performs low-level optimizations on LIR:
   - Remove identity operations (add x, y, 0 → mov x, y)
   - Remove self-moves (mov x, x → remove)
   - Constant multiplication optimizations (mul x, y, 0 → mov x, 0)
   - Dead move elimination
   - Retarget dead floating-point results into their copy destinations
   - Retarget separated dead floating-point additions into their copy destinations
   These optimizations work on individual instructions or small sequences.
*)
(* LIR_Peephole.fs - LIR Peephole Optimizations *)
[@@@warning "-4"]
open LIR
module RegMap=Map.Make(struct type t=reg let compare=Stdlib.compare end)
module RegSet=Set.Make(struct type t=reg let compare=Stdlib.compare end)
module FRegMap=Map.Make(struct type t=fReg let compare=Stdlib.compare end)
module LabelSet=Set.Make(struct type t=label let compare (Label a) (Label b)=StringOrder.compare a b end)
type domBitSet=Bitset.bitset
type dominators={indexOf:int LabelMap.t;sets:domBitSet array}
type dominatorCache={succs:label list LabelMap.t;dominators:dominators}
let dominatorSetsEqual left right=Array.length left=Array.length right && Array.for_all2 Bitset.equal left right
let labelName (Label name)=name
(*
   Check if two registers are the same
*)
let sameReg r1 r2 =
(match r1, r2 with
| LIR.Physical p1, LIR.Physical p2 -> (p1 = p2)
| LIR.Virtual v1, LIR.Virtual v2 -> (v1 = v2)
| LIR.Physical _, LIR.Virtual _ | LIR.Virtual _, LIR.Physical _ -> (false))

let sameFReg r1 r2 =
(match r1, r2 with
| LIR.FPhysical p1, LIR.FPhysical p2 -> (p1 = p2)
| LIR.FVirtual v1, LIR.FVirtual v2 -> (v1 = v2)
| LIR.FPhysical _, LIR.FVirtual _ | LIR.FVirtual _, LIR.FPhysical _ -> (false))

(*
   Get successor labels from a terminator
*)
let getSuccessors term =
(match term with
| Ret -> ([])
| Jump label -> ([label])
| Branch (_, trueLabel, falseLabel) -> ([trueLabel; falseLabel])
| BranchZero (_, zeroLabel, nonZeroLabel) -> ([zeroLabel; nonZeroLabel])
| BranchBitZero (_, _, zeroLabel, nonZeroLabel) -> ([zeroLabel; nonZeroLabel])
| BranchBitNonZero (_, _, nonZeroLabel, zeroLabel) -> ([nonZeroLabel; zeroLabel])
| CondBranch (_, trueLabel, falseLabel) -> ([trueLabel; falseLabel]))

(*
   Build predecessor map for the CFG
*)
let buildPredecessors (cfg:cfg)=
 let emptyPreds=LabelMap.map (fun _ -> []) cfg.blocks in
 LabelMap.fold (fun label block preds -> List.fold_left (fun acc succ -> let existing=Option.value (LabelMap.find_opt succ acc) ~default:[] in LabelMap.add succ (label::existing) acc) preds (getSuccessors block.terminator)) cfg.blocks emptyPreds
(*
   Build successor map for the CFG
*)
let buildSuccessors (cfg:cfg)=LabelMap.map (fun block -> getSuccessors block.terminator) cfg.blocks
let validateCFGShape (cfg:cfg)=
 if not (LabelMap.mem cfg.entry cfg.blocks) then Crash.crash ("LIR Peephole: entry label "^labelName cfg.entry^" not found in CFG blocks") else
 LabelMap.iter (fun label block -> List.iter (fun succ -> if not (LabelMap.mem succ cfg.blocks) then Crash.crash ("LIR Peephole: block "^labelName label^" has missing successor label "^labelName succ)) (getSuccessors block.terminator)) cfg.blocks
(*
   Compute dominator sets for each block
*)
let computeDominators (cfg:cfg) preds=
 let labels=LabelMap.bindings cfg.blocks |> List.map fst |> Array.of_list in
 let indexOf=Array.to_list (Array.mapi (fun idx label -> label,idx) labels) |> LabelMap.of_list in
 let entryIndex=match LabelMap.find_opt cfg.entry indexOf with Some idx -> idx | None -> Crash.crash ("LIR Peephole: entry label Label \""^labelName cfg.entry^"\" not found in CFG blocks") in
 let labelCount=Array.length labels in let wordCount=Bitset.wordCount labelCount in
 let allBits=Bitset.all labelCount in let entryBits=Bitset.singleton wordCount entryIndex in
 let predIndices=Array.map (fun label -> Option.value (LabelMap.find_opt label preds) ~default:[] |> List.filter_map (fun pred -> LabelMap.find_opt pred indexOf)) labels in
 let initial=Array.init labelCount (fun idx -> if idx=entryIndex then entryBits else allBits) in
 let rec loop doms=
  let updated=Array.init labelCount (fun idx -> if idx=entryIndex then entryBits else
   let predSets=List.filter_map (fun predIdx -> if predIdx<0 || predIdx>=Array.length doms then None else Some doms.(predIdx)) predIndices.(idx) in
   match predSets with [] -> Bitset.singleton wordCount idx | first::rest -> Bitset.add idx (Bitset.intersectMany first rest)) in
  if dominatorSetsEqual doms updated then updated else loop updated in
 {indexOf;sets=loop initial}
(*
   Identify natural loops via backedges (header dominates source)
*)
let findNaturalLoopsWithCache (cfg:cfg) domCache=
 let preds=buildPredecessors cfg in let succs=buildSuccessors cfg in
 let doms,cache'=match domCache with Some cache when LabelMap.equal (=) cache.succs succs -> cache.dominators,domCache | _ -> let doms=computeDominators cfg preds in doms,Some {succs;dominators=doms} in
 let dominates dominator node=match LabelMap.find_opt dominator doms.indexOf,LabelMap.find_opt node doms.indexOf with Some domIdx,Some nodeIdx when nodeIdx>=0 && nodeIdx<Array.length doms.sets -> Bitset.containsIndex domIdx doms.sets.(nodeIdx) | _ -> false in
 let backedges=LabelMap.fold (fun from successors acc -> List.fold_left (fun acc' succ -> if dominates succ from then let existing=Option.value (LabelMap.find_opt succ acc') ~default:[] in LabelMap.add succ (from::existing) acc' else acc') acc successors) succs LabelMap.empty in
 let loops=LabelMap.fold (fun header sources loops ->
  let loopBlocks=List.fold_left (fun acc source ->
   let initial=LabelSet.of_list [header;source] in
   let rec grow work loopSet=match work with [] -> loopSet | node::rest ->
    let nodePreds=Option.value (LabelMap.find_opt node preds) ~default:[] in
    let loopSet',work'=List.fold_left (fun (setAcc,workAcc) pred -> if LabelSet.mem pred setAcc then setAcc,workAcc else if dominates header pred then LabelSet.add pred setAcc,pred::workAcc else setAcc,workAcc) (loopSet,rest) nodePreds in grow work' loopSet' in
   LabelSet.union acc (grow [source] initial)) LabelSet.empty sources in
  if LabelSet.is_empty loopBlocks then loops else LabelMap.add header loopBlocks loops) backedges LabelMap.empty in loops,cache'
(*
   Check whether the CFG has any directed cycle.
*)
let hasCycle (cfg:cfg)=
 let succs=buildSuccessors cfg in
 let rec visit visiting visited label=if LabelSet.mem label visiting then true,visited else if LabelSet.mem label visited then false,visited else
  let visiting'=LabelSet.add label visiting in let successors=Option.value (LabelMap.find_opt label succs) ~default:[] in
  let rec visitSuccessors remaining visitedAcc=match remaining with [] -> false,LabelSet.add label visitedAcc | succ::rest -> let foundCycle,visited'=visit visiting' visitedAcc succ in if foundCycle then true,visited' else visitSuccessors rest visited' in
  visitSuccessors successors visited in
 let labels=LabelMap.bindings cfg.blocks |> List.map fst in
 let rec visitAll remaining visited=match remaining with [] -> false | label::rest -> if LabelSet.mem label visited then visitAll rest visited else let foundCycle,visited'=visit LabelSet.empty visited label in foundCycle || visitAll rest visited' in
 visitAll labels LabelSet.empty
(*
   A virtual destination whose constant definition can move outside a loop.
*)
type hoistableConstDest=IntVirtualDest of int | FloatVirtualDest of int
module ConstDestSet=Set.Make(struct type t=hoistableConstDest let compare=Stdlib.compare end)
(*
   Check whether an instruction is a hoistable constant definition.
*)
let isHoistableConstInstr instr =
(match instr with
| Mov (LIR.Virtual id, Imm _) -> (Some (IntVirtualDest id))
| FLoad (LIR.FVirtual id, _) -> (Some (FloatVirtualDest id))
| _ -> (None))

(*
   Check whether an instruction represents a call (affects register saving)
*)
let isCallInstr instr =
(match instr with
| Call _ | TailCall _ | IndirectCall _ | IndirectTailCall _ | ClosureCall _ | ClosureTailCall _ -> (true)
| _ -> (false))

(*
   Check whether an instruction is pure arithmetic/logic for LICM safety
*)
let isPureLoopInstr instr =
(match instr with
| Mov _ | Phi _ | FPhi _ | Add _ | Sub _ | Mul _ | Sdiv _ | Msub _ | Madd _ | Cmp _ | Cset _ | Select _ | And _ | And_imm _ | Orr _ | Eor _ | Lsl _ | Lsr _ | Asr _ | Lsl_imm _ | Lsr_imm _ | Asr_imm _ | Neg _ | Mvn _ | Sxtb _ | Sxth _ | Sxtw _ | Uxtb _ | Uxth _ | Uxtw _ | FMov _ | FLoad _ | FSpillLoad _ | FSpillStore _ | FAdd _ | FSub _ | FMul _ | FMadd _ | FDiv _ | FNeg _ | FAbs _ | FSqrt _ | FCmp _ | Int64ToFloat _ | FloatToInt64 _ | GpToFp _ | FpToGp _ -> (true)
| _ -> (false))

(*
   Hoist loop-invariant integer and float constants into simple preheaders.
*)
let applyLoopInvariantConstHoist (cfg:cfg) domCache=
 if not (hasCycle cfg) then cfg,false,domCache else
 let loops,cache'=findNaturalLoopsWithCache cfg domCache in let preds=buildPredecessors cfg in
 let result=LabelMap.fold (fun header loopBlocks (cfgAcc,changedAcc) ->
  let outsidePreds=Option.value (LabelMap.find_opt header preds) ~default:[] |> List.filter (fun pred -> not (LabelSet.mem pred loopBlocks)) in
  let tryGetPreheader=match outsidePreds with [preheader] -> (match LabelMap.find_opt preheader cfgAcc.blocks with Some {terminator=Jump target;_} when target=header -> Some preheader | _ -> None) | _ -> None in
  match tryGetPreheader with None -> cfgAcc,changedAcc | Some preheader ->
   let loopHasCall=LabelSet.exists (fun label -> match LabelMap.find_opt label cfgAcc.blocks with None -> false | Some block -> List.exists isCallInstr block.instrs) loopBlocks in
   let loopIsPure=LabelSet.for_all (fun label -> match LabelMap.find_opt label cfgAcc.blocks with None -> true | Some block -> List.for_all isPureLoopInstr block.instrs) loopBlocks in
   if loopHasCall || not loopIsPure then cfgAcc,changedAcc else
   let blockOrder=header::(LabelSet.remove header loopBlocks |> LabelSet.elements |> List.stable_sort (fun a b -> StringOrder.compare (labelName a) (labelName b))) in
   let hoistedRev,hoistedDests=List.fold_left (fun (instrs,dests) label -> match LabelMap.find_opt label cfgAcc.blocks with None -> instrs,dests | Some block -> List.fold_left (fun (instrsAcc,destsAcc) instr -> match isHoistableConstInstr instr with Some dest when not (ConstDestSet.mem dest destsAcc) -> instr::instrsAcc,ConstDestSet.add dest destsAcc | _ -> instrsAcc,destsAcc) (instrs,dests) block.instrs) ([],ConstDestSet.empty) blockOrder in
   let hoistedInstrs=List.rev hoistedRev in if hoistedInstrs=[] then cfgAcc,changedAcc else
   let blocks'=LabelMap.mapi (fun label block -> if label=preheader then {block with instrs=block.instrs @ hoistedInstrs} else if LabelSet.mem label loopBlocks then {block with instrs=List.filter (fun instr -> match isHoistableConstInstr instr with Some dest -> not (ConstDestSet.mem dest hoistedDests) | None -> true) block.instrs} else block) cfgAcc.blocks in
   {cfgAcc with blocks=blocks'},true) loops (cfg,false) in let cfg',changed=result in cfg',changed,cache'

(*
   Optimize a single instruction (returns None to remove, Some to replace)
   Remove self-moves: mov x, x → remove
   Add with zero: add x, y, 0 → mov x, y (if x != y) or remove (if x == y)
   Add with zero on left: we don't have this form in LIR
   Sub with zero: sub x, y, 0 → mov x, y or remove
   Multiply by zero: mul x, y, z where z is zero → mov x, 0
   This requires both operands to be registers in LIR, so we can't detect 0
   Multiply by one: would require one operand to be immediate, but Mul takes two regs
   For now, keep the instruction as-is
*)
let optimizeInstr instr =
(match instr with
| Mov (dest, Reg src) when sameReg dest src -> (None)
| FMov (dest, src) when sameFReg dest src -> (None)
| Add (dest, left, Imm 0L) -> ((if sameReg dest left then (None) else (Some (Mov (dest, Reg left)))))
| Sub (dest, left, Imm 0L) -> ((if sameReg dest left then (None) else (Some (Mov (dest, Reg left)))))
| _ -> (Some instr))

let fRegUsedInInstr target instr =
let same = (sameFReg target) in
(match instr with
| FArgMoves moves -> (moves |> List.exists (fun (_, src) -> same src))
| PrintFloat src | PrintFloatNoNewline src | FMov (_, src) | FNeg (_, src) | FAbs (_, src) | FSqrt (_, src) | FloatToInt64 (_, src) | FloatToBits (_, src) | FpToGp (_, src) | FloatToString (_, src) -> (same src)
| FAdd (_, left, right) | FSub (_, left, right) | FMul (_, left, right) | FDiv (_, left, right) | FCmp (left, right) -> (same left || same right)
| FMadd (_, left, right, addend) -> (same left || same right || same addend)
| FPhi (_, sources) -> (sources |> List.exists (fun (src, _) -> same src))
| _ -> (false))

let fRegUsedInInstrs target instrs=List.exists (fRegUsedInInstr target) instrs
(*
   Retarget a dead virtual FAdd result into a later virtual-register copy.
   Keeping the producer in place preserves floating-point evaluation order;
   virtual LIR is in SSA form, so proving that the copy is the temporary's only
   use makes replacing the temporary and copy with one definition safe.
*)
let retargetSeparatedDeadFAdds instrs=
 let rec findCopy temp betweenReversed remaining=match remaining with
 | FMov (FVirtual destId,moveSource)::tail when sameFReg temp moveSource && betweenReversed<>[] -> let dest=FVirtual destId in let between=List.rev betweenReversed in if sameFReg temp dest || fRegUsedInInstrs temp tail then None else Some (dest,between,tail)
 | instr::_ when fRegUsedInInstr temp instr -> None
 | instr::tail -> findCopy temp (instr::betweenReversed) tail | [] -> None in
 let rec loop acc remaining=match remaining with
 | (FAdd ((FVirtual _ as temp),left,right) as instr)::rest when not (sameFReg temp left) && not (sameFReg temp right) -> (match findCopy temp [] rest with Some (dest,between,tail) when not (sameFReg dest left) && not (sameFReg dest right) -> loop (List.rev between @ (FAdd (dest,left,right)::acc)) tail | _ -> loop (instr::acc) rest)
 | instr::rest -> loop (instr::acc) rest | [] -> List.rev acc in loop [] instrs
let tryRetargetFloatingResultIntoMove instr next rest=
 match instr,next with
 | FNeg (temp,src),FMov (dest,moveSrc) when sameFReg temp moveSrc && not (fRegUsedInInstrs temp rest) -> Some (FNeg (dest,src))
 | FAdd (temp,left,right),FMov (dest,moveSrc) when sameFReg temp moveSrc && not (fRegUsedInInstrs temp rest) -> Some (FAdd (dest,left,right))
 | FSub (temp,left,right),FMov (dest,moveSrc) when sameFReg temp moveSrc && not (fRegUsedInInstrs temp rest) -> Some (FSub (dest,left,right))
 | FMul (temp,left,right),FMov (dest,moveSrc) when sameFReg temp moveSrc && not (fRegUsedInInstrs temp rest) -> Some (FMul (dest,left,right))
 | FDiv (temp,left,right),FMov (dest,moveSrc) when sameFReg temp moveSrc && not (fRegUsedInInstrs temp rest) -> Some (FDiv (dest,left,right))
 | _ -> None
(*
   Optimize a list of instructions (single-pass peephole)
*)
let optimizeInstrsWithChange instrs=
 let rec loop changed remaining=match remaining with
 | instr::next::rest -> (match tryRetargetFloatingResultIntoMove instr next rest with Some folded -> let optimizedRest,_=loop true rest in folded::optimizedRest,true | None -> (match optimizeInstr instr with Some instr' -> let changed'=changed || not (instr==instr') in let optimizedRest,restChanged=loop changed' (next::rest) in instr'::optimizedRest,restChanged | None -> loop true (next::rest)))
 | instr::rest -> (match optimizeInstr instr with Some instr' -> let changed'=changed || not (instr==instr') in let optimizedRest,restChanged=loop changed' rest in instr'::optimizedRest,restChanged | None -> loop true rest)
 | [] -> [],changed in
 let retargeted=retargetSeparatedDeadFAdds instrs in loop (instrs<>retargeted) retargeted
let optimizeInstrs instrs=fst (optimizeInstrsWithChange instrs)
let removeSelfMovesFromInstrs instrs=
 let rec loop = function
 | instr::next::rest -> (match tryRetargetFloatingResultIntoMove instr next rest with Some folded -> folded::loop rest | None -> (match instr with Mov (dest,Reg src) when sameReg dest src -> loop (next::rest) | FMov (dest,src) when sameFReg dest src -> loop (next::rest) | _ -> instr::loop (next::rest)))
 | instr::rest -> (match instr with Mov (dest,Reg src) when sameReg dest src -> loop rest | FMov (dest,src) when sameFReg dest src -> loop rest | _ -> instr::loop rest)
 | [] -> [] in loop instrs
let removeSelfMovesFromFunction (func:functionDef)={func with cfg={func.cfg with blocks=LabelMap.map (fun block -> {block with instrs=removeSelfMovesFromInstrs block.instrs}) func.cfg.blocks}}
type floatingValueIdentity=InitialFRegValue of fReg | WrittenFRegValue of int
let currentFRegValue reg aliases=Option.value (FRegMap.find_opt reg aliases) ~default:(InitialFRegValue reg)
let recordFRegWrite dest valueIdentity aliases=FRegMap.add dest (WrittenFRegValue valueIdentity) aliases
let fRegWriteDest instr =
(match instr with
| FPhi (dest, _) | FLoad (dest, _) | FAdd (dest, _, _) | FSub (dest, _, _) | FMul (dest, _, _) | FMadd (dest, _, _, _) | FDiv (dest, _, _) | FNeg (dest, _) | FAbs (dest, _) | FSqrt (dest, _) | Int64ToFloat (dest, _) | GpToFp (dest, _) -> (Some dest)
| _ -> (None))

let clobbersFRegs instr =
(match instr with
| Call _ | TailCall _ | IndirectCall _ | IndirectTailCall _ | ClosureCall _ | ClosureTailCall _ | RestoreRegs _ | FArgMoves _ -> (true)
| _ -> (false))

let fRegWrittenByInstr target instr=match instr with FMov (dest,_) -> sameFReg target dest | _ -> (match fRegWriteDest instr with Some dest -> sameFReg target dest | None -> false)
(*
   Delay a dead allocated FAdd until its later copy and write the copy
   destination directly. The crossed instructions must be pure and may not
   overwrite any value consumed by the delayed addition.
*)
let sinkSeparatedAllocatedFAdds instrs=
 let rec findCopy temp left right betweenReversed remaining=match remaining with
 | FMov ((FPhysical _ as dest),moveSource)::tail when sameFReg temp moveSource && betweenReversed<>[] ->
  let between=List.rev betweenReversed in let overwritesInput=List.exists (fun instr -> fRegWrittenByInstr temp instr || fRegWrittenByInstr left instr || fRegWrittenByInstr right instr) between in
  if overwritesInput || fRegUsedInInstrs temp tail then None else Some (dest,between,tail)
 | instr::_ when not (isPureLoopInstr instr) -> None
 | instr::_ when fRegUsedInInstr temp instr || fRegWrittenByInstr temp instr -> None
 | instr::tail -> findCopy temp left right (instr::betweenReversed) tail | [] -> None in
 let rec loop acc remaining=match remaining with
 | (FAdd ((FPhysical _ as temp),left,right) as instr)::rest -> (match findCopy temp left right [] rest with Some (dest,between,tail) -> let acc'=FAdd (dest,left,right)::(List.rev between @ acc) in loop acc' tail | None -> loop (instr::acc) rest)
 | instr::rest -> loop (instr::acc) rest | [] -> List.rev acc in loop [] instrs
let removeRedundantFloatingCopyBackMovesWithChange instrs=
 let rec loop aliases nextValueIdentity acc changed remaining=match remaining with
 | [] -> List.rev acc,changed
 | (FMov (dest,src) as instr)::rest -> let srcValue=currentFRegValue src aliases in let destValue=currentFRegValue dest aliases in let aliases'=FRegMap.add dest srcValue aliases in let redundant=destValue=srcValue in let acc'=if redundant then acc else instr::acc in loop aliases' nextValueIdentity acc' (changed || redundant) rest
 | instr::rest -> let aliases',nextValueIdentity'=if clobbersFRegs instr then FRegMap.empty,nextValueIdentity else match fRegWriteDest instr with Some dest -> recordFRegWrite dest nextValueIdentity aliases,nextValueIdentity+1 | None -> aliases,nextValueIdentity in loop aliases' nextValueIdentity' (instr::acc) changed rest in
 loop FRegMap.empty 0 [] false instrs
let removeRedundantFloatingCopyBackMoves instrs=fst (removeRedundantFloatingCopyBackMovesWithChange instrs)
(*
   Sink a loop-counter decrement past an accumulator update so its temporary
   copy-back becomes a single destructive subtraction. Restrict this late
   rewrite to the complete three-instruction block shape, making the temporary
   provably dead at the block boundary without rebuilding liveness.
*)
let sinkImmediateCounterUpdate = function
| [Sub (temp,counter,Imm amount);accInstr;Mov (copyDest,Reg copySource)] when sameReg temp copySource && sameReg counter copyDest && not (sameReg temp counter) ->
 let parts=match accInstr with
 | Add (dest,left,Reg right) | Sub (dest,left,Reg right) | Sdiv (dest,left,right) | Mul (dest,left,right) | Eor (dest,left,right) | And (dest,left,right) | Orr (dest,left,right) | Lsl (dest,left,right) | Lsr (dest,left,right) -> Some (dest,[left;right])
 | Madd (dest,left,right,add) | Msub (dest,left,right,add) -> Some (dest,[left;right;add]) | _ -> None in
 (match parts with Some (dest,inputs) when not (sameReg dest temp) && not (sameReg dest counter) && List.for_all (fun reg -> not (sameReg reg temp)) inputs -> Some [accInstr;Sub (counter,counter,Imm amount)] | _ -> None)
| _ -> None
let removePostAllocationMovesFromFunction (func:functionDef)=
 let blocks=LabelMap.map (fun block -> {block with instrs=block.instrs |> removeSelfMovesFromInstrs |> removeRedundantFloatingCopyBackMoves |> sinkSeparatedAllocatedFAdds}) func.cfg.blocks in {func with cfg={func.cfg with blocks}}
let optimizeAllocatedCounterUpdates (func:functionDef)=
 let blocks=LabelMap.map (fun block -> let cleanedInstrs=removeSelfMovesFromInstrs block.instrs in let instrs=Option.value (sinkImmediateCounterUpdate cleanedInstrs) ~default:cleanedInstrs in {block with instrs}) func.cfg.blocks in {func with cfg={func.cfg with blocks}}

let foldOperandRegUse folder state operand =
(match operand with
| Reg reg -> (folder state reg)
| Imm _ | FloatImm _ | StackSlot _ | StringSymbol _ | FloatSymbol _ | FuncAddr _ -> (state))

(*
   Fold over the integer registers read by an instruction.
*)
let foldRegUses folder state instr =
(match instr with
| Mov (_, src) | RefCountIncString src | RefCountDecString src | RefCountIncBlob src | RefCountDecBlob src -> (foldOperandRegUse folder state src)
| RefCountIncInt src | RefCountDecInt src -> (foldOperandRegUse folder state src)
| Phi (_, sources, _) -> (sources |> List.fold_left (fun acc (src, _) -> foldOperandRegUse folder acc src) state)
| Store (_, src) | PrintInt64 src | PrintUInt64 src | PrintBool src | PrintInt64NoNewline src | PrintUInt64NoNewline src | PrintBoolNoNewline src | PrintHeapStringNoNewline src | PrintBlob src | PrintList (src, _) | PrintSum (src, _, _) | PrintRecord (src, _, _) | Int64ToFloat (_, src) | GpToFp (_, src) | RawFree src | MappedFree src | FloatToString (src, _) -> (folder state src)
| Add (_, left, right) | Sub (_, left, right) | Cmp (left, right) -> (foldOperandRegUse folder (folder state left) right)
| Mul (_, left, right) | Sdiv (_, left, right) | Udiv (_, left, right) | And (_, left, right) | Orr (_, left, right) | Eor (_, left, right) | Lsl (_, left, right) | Lsr (_, left, right) | Asr (_, left, right) | RawGet (_, left, right) | RawGetByte (_, left, right) -> (folder (folder state left) right)
| Select (_, whenTrue, whenFalse, _) -> (folder (folder state whenTrue) whenFalse)
| RawAlloc (_, numBytes) -> (folder state numBytes)
| MappedAlloc (_, numBytes) -> (folder state numBytes)
| FileWriteFromPtr (_, path, ptr, length) -> (foldOperandRegUse folder state path |> fun acc -> folder (folder acc ptr) length)
| Msub (_, mulLeft, mulRight, sub) | Madd (_, mulLeft, mulRight, sub) | RawWriteWord (mulLeft, mulRight, sub) | RawWriteByte (mulLeft, mulRight, sub) | RawSlotInit (mulLeft, mulRight, sub, _) -> (folder (folder (folder state mulLeft) mulRight) sub)
| And_imm (_, src, _) | Lsl_imm (_, src, _) | Lsr_imm (_, src, _) | Asr_imm (_, src, _) | Neg (_, src) | Mvn (_, src) | Sxtb (_, src) | Sxth (_, src) | Sxtw (_, src) | Uxtb (_, src) | Uxth (_, src) | Uxtw (_, src) | PrintHeapString src | HeapLoad (_, src, _) | RefCountInc (src, _, _, _) | RefCountDec (src, _, _, _) -> (folder state src)
| Call (_, _, args) | TailCall (_, args) -> (args |> List.fold_left (foldOperandRegUse folder) state)
| ArgMoves args | TailArgMoves args -> (args |> List.fold_left (fun acc (_, src) -> foldOperandRegUse folder acc src) state)
| IndirectCall (_, func, args) | IndirectTailCall (func, args) -> (args |> List.fold_left (foldOperandRegUse folder) (folder state func))
| ClosureCall (_, closure, args) | ClosureTailCall (closure, args) -> (args |> List.fold_left (foldOperandRegUse folder) (folder state closure))
| ClosureAlloc (_, _, captures) -> (captures |> List.fold_left (foldOperandRegUse folder) state)
| HeapStore (addr, _, src, _) -> (foldOperandRegUse folder (folder state addr) src)
| StringConcat (_, first, second, remaining) -> (first :: second :: remaining |> List.fold_left (foldOperandRegUse folder) state)
| CanonicalBufferEq (_, _, left, right) | FileWriteBlob (_, left, right) | FileAppendText (_, left, right) -> (foldOperandRegUse folder state left |> fun acc -> foldOperandRegUse folder acc right)
| FileReadBlob (_, path) | FileExists (_, path) | FileDelete (_, path) | FileCreateDirectory (_, path) | FileSetExecutable (_, path) -> (foldOperandRegUse folder state path)
| StdoutWrite (_, value, _) -> (foldOperandRegUse folder state value)
| CliNative (_, _, args) -> (args |> List.fold_left (foldOperandRegUse folder) state)
| Cset _ | SaveRegs _ | RestoreRegs _ | FArgMoves _ | PrintFloat _ | PrintFloatNoNewline _ | PrintString _ | StdinReadLine _ | RuntimeError _ | RuntimeErrorString _ | PrintChars _ | LIR.Exit | FPhi _ | FMov _ | FLoad _ | FSpillLoad _ | FSpillStore _ | FAdd _ | FSub _ | FMul _ | FMadd _ | FDiv _ | FNeg _ | FAbs _ | FSqrt _ | FCmp _ | FloatToInt64 _ | FloatToBits _ | FpToGp _ | HeapAlloc _ | LoadFuncAddr _ | RandomInt64 _ | DateTimeNow _ | Sleep _ | CoverageHit _ -> (state))

let foldTerminatorRegUses folder state terminator =
(match terminator with
| Branch (reg, _, _) | BranchZero (reg, _, _) | BranchBitZero (reg, _, _, _) | BranchBitNonZero (reg, _, _, _) -> (folder state reg)
| Ret | Jump _ | CondBranch _ -> (state))

(*
   Check if a register is read by an instruction.
*)
let regUsedInInstr target instr=foldRegUses (fun used reg -> used || sameReg reg target) false instr
(*
   Record the last read of each temporary whose remaining uses affect a fold.
   Restricting the map to candidates avoids both repeated suffix scans and a
   full-block liveness map for blocks that contain no relevant peepholes.
*)
let lastRelevantRegUses candidates instrs=
 let rec loop index uses remaining=match remaining with instr::rest -> let uses'=foldRegUses (fun acc reg -> if RegSet.mem reg candidates then RegMap.add reg index acc else acc) uses instr in loop (index+1) uses' rest | [] -> uses in loop 0 RegMap.empty instrs
let regUsedAfter lastUses index reg=match RegMap.find_opt reg lastUses with Some lastUse -> lastUse>index | None -> false
(*
   Check if a register is used in any instruction (for dead code detection)
*)
let isRegUsedInInstrs reg instrs=List.exists (regUsedInInstr reg) instrs
let cfgRegUseCounts (cfg:cfg)=
 let addUse counts reg=let count=Option.value (RegMap.find_opt reg counts) ~default:0 in RegMap.add reg (count+1) counts in
 LabelMap.fold (fun _ block counts -> let instructionUses=List.fold_left (fun acc instr -> foldRegUses addUse acc instr) counts block.instrs in foldTerminatorRegUses addUse instructionUses block.terminator) cfg.blocks RegMap.empty
(*
   Describes how a multiplication constant differs from a power of two.
*)
type mulConstantPattern=PowerOfTwoPlusOne | PowerOfTwoMinusOne
(*
   Check if a value is suitable for multiply-by-constant strength reduction.
   Returns the shift and whether the constant is one above or below that power of two.
   3 = 2 + 1 = (1 << 1) + 1
   5 = 4 + 1 = (1 << 2) + 1
   7 = 8 - 1 = (1 << 3) - 1
   9 = 8 + 1 = (1 << 3) + 1
   15 = 16 - 1 = (1 << 4) - 1
   17 = 16 + 1 = (1 << 4) + 1
   31 = 32 - 1 = (1 << 5) - 1
   33 = 32 + 1 = (1 << 5) + 1
   63 = 64 - 1 = (1 << 6) - 1
   65 = 64 + 1 = (1 << 6) + 1
*)
let tryMulConstantPattern n =
(match n with
| 3L -> (Some (1, PowerOfTwoPlusOne))
| 5L -> (Some (2, PowerOfTwoPlusOne))
| 7L -> (Some (3, PowerOfTwoMinusOne))
| 9L -> (Some (3, PowerOfTwoPlusOne))
| 15L -> (Some (4, PowerOfTwoMinusOne))
| 17L -> (Some (4, PowerOfTwoPlusOne))
| 31L -> (Some (5, PowerOfTwoMinusOne))
| 33L -> (Some (5, PowerOfTwoPlusOne))
| 63L -> (Some (6, PowerOfTwoMinusOne))
| 65L -> (Some (6, PowerOfTwoPlusOne))
| _ -> (None))

let mulByConstantCandidates instrs=
 let rec loop candidates remaining=match remaining with
 | Mov (constReg,Imm n)::Mul (_,mulLeft,mulRight)::rest when Option.is_some (tryMulConstantPattern n) && ((sameReg constReg mulRight && not (sameReg constReg mulLeft)) || (sameReg constReg mulLeft && not (sameReg constReg mulRight))) -> loop (RegSet.add constReg candidates) rest
 | _::rest -> loop candidates rest | [] -> candidates in loop RegSet.empty instrs
(*
   Try to optimize multiply-by-constant patterns
   Pattern: Mov temp, Imm n; Mul dest, x, temp → Lsl_imm temp, x, shift; Add/Sub dest, x, Reg temp
   This converts multiplication by constants like 3, 5, 7, 9 to shift+add/sub sequences
   which ARM64 can execute in a single ADD_shifted/SUB_shifted instruction
*)
let tryMulByConstantWithChange instrs=
 let candidates=mulByConstantCandidates instrs in if RegSet.is_empty candidates then instrs,false else
 let lastUses=lastRelevantRegUses candidates instrs in
 let rec loop index acc changed remaining=match remaining with
 | [] -> List.rev acc,changed | [single] -> List.rev (single::acc),changed
 | (Mov (constReg,Imm n) as mov)::(Mul (mulDest,mulLeft,mulRight) as mul)::rest when sameReg constReg mulRight && not (sameReg constReg mulLeft) ->
  (match tryMulConstantPattern n with Some (shift,pattern) when not (regUsedAfter lastUses (index+1) constReg) -> let shiftInstr=Lsl_imm (constReg,mulLeft,shift) in let combineInstr=match pattern with PowerOfTwoPlusOne -> Add (mulDest,mulLeft,Reg constReg) | PowerOfTwoMinusOne -> Sub (mulDest,constReg,Reg mulLeft) in loop (index+2) (combineInstr::shiftInstr::acc) true rest | _ -> loop (index+1) (mov::acc) changed (mul::rest))
 | (Mov (constReg,Imm n) as mov)::(Mul (mulDest,mulLeft,mulRight) as mul)::rest when sameReg constReg mulLeft && not (sameReg constReg mulRight) ->
  (match tryMulConstantPattern n with Some (shift,pattern) when not (regUsedAfter lastUses (index+1) constReg) -> let shiftInstr=Lsl_imm (constReg,mulRight,shift) in let combineInstr=match pattern with PowerOfTwoPlusOne -> Add (mulDest,mulRight,Reg constReg) | PowerOfTwoMinusOne -> Sub (mulDest,constReg,Reg mulRight) in loop (index+2) (combineInstr::shiftInstr::acc) true rest | _ -> loop (index+1) (mov::acc) changed (mul::rest))
 | instr::rest -> loop (index+1) (instr::acc) changed rest in loop 0 [] false instrs
let tryMulByConstant instrs=fst (tryMulByConstantWithChange instrs)
let mulAddCandidates instrs=
 let rec loop candidates remaining=match remaining with
 | Mul (mulDest,_,_)::Add (_,addLeft,Reg addRight)::rest when (sameReg mulDest addLeft && not (sameReg mulDest addRight)) || (sameReg mulDest addRight && not (sameReg mulDest addLeft)) -> loop (RegSet.add mulDest candidates) rest
 | _::rest -> loop candidates rest | [] -> candidates in loop RegSet.empty instrs
(*
   Try to fuse MUL + ADD into MADD (multiply-add)
   Pattern: MUL temp, a, b; ADD dest, temp, Reg c → MADD dest, a, b, c
   Or:      MUL temp, a, b; ADD dest, Reg c, temp → MADD dest, a, b, c (commutative)
*)
let tryFuseMulAddWithChange instrs=
 let candidates=mulAddCandidates instrs in if RegSet.is_empty candidates then instrs,false else
 let lastUses=lastRelevantRegUses candidates instrs in
 let rec loop index acc changed remaining=match remaining with
 | [] -> List.rev acc,changed | [single] -> List.rev (single::acc),changed
 | (Mul (mulDest,mulLeft,mulRight) as mul)::(Add (addDest,addLeft,Reg addRight) as add)::rest when sameReg mulDest addLeft && not (sameReg mulDest addRight) -> if not (regUsedAfter lastUses (index+1) mulDest) then loop (index+2) (Madd (addDest,mulLeft,mulRight,addRight)::acc) true rest else loop (index+1) (mul::acc) changed (add::rest)
 | (Mul (mulDest,mulLeft,mulRight) as mul)::(Add (addDest,addLeft,Reg addRight) as add)::rest when sameReg mulDest addRight && not (sameReg mulDest addLeft) -> if not (regUsedAfter lastUses (index+1) mulDest) then loop (index+2) (Madd (addDest,mulLeft,mulRight,addLeft)::acc) true rest else loop (index+1) (mul::acc) changed (add::rest)
 | instr::rest -> loop (index+1) (instr::acc) changed rest in loop 0 [] false instrs
let tryFuseMulAdd instrs=fst (tryFuseMulAddWithChange instrs)
let mulSubCandidates instrs=
 let rec loop candidates remaining=match remaining with Mul (mulDest,_,_)::Sub (_,minuend,Reg subtrahend)::rest when sameReg mulDest subtrahend && not (sameReg mulDest minuend) -> loop (RegSet.add mulDest candidates) rest | _::rest -> loop candidates rest | [] -> candidates in loop RegSet.empty instrs
(*
   Try to fuse MUL + SUB into MSUB (multiply-subtract)
   Pattern: MUL temp, a, b; SUB dest, minuend, Reg temp → MSUB dest, a, b, minuend
*)
let tryFuseMulSubWithChange instrs=
 let candidates=mulSubCandidates instrs in if RegSet.is_empty candidates then instrs,false else
 let lastUses=lastRelevantRegUses candidates instrs in
 let rec loop index acc changed remaining=match remaining with [] -> List.rev acc,changed | [single] -> List.rev (single::acc),changed
 | (Mul (mulDest,mulLeft,mulRight) as mul)::(Sub (subDest,minuend,Reg subtrahend) as sub)::rest when sameReg mulDest subtrahend && not (sameReg mulDest minuend) -> if not (regUsedAfter lastUses (index+1) mulDest) then loop (index+2) (Msub (subDest,mulLeft,mulRight,minuend)::acc) true rest else loop (index+1) (mul::acc) changed (sub::rest)
 | instr::rest -> loop (index+1) (instr::acc) changed rest in loop 0 [] false instrs
let tryFuseMulSub instrs=fst (tryFuseMulSubWithChange instrs)
(*
   Fuse a dead floating multiply into an immediately following addition.
   This changes the rounding point, so callers enable it only for targets whose
   cost model selects a hardware fused operation.
*)
let tryFuseFloatMultiplyAdd instrs=
 let rec loop acc changed remaining=match remaining with
 | FMul (temp,left,right)::FAdd (dest,addLeft,addRight)::rest when sameFReg temp addLeft && not (sameFReg temp addRight) && not (fRegUsedInInstrs temp rest) -> loop (FMadd (dest,left,right,addRight)::acc) true rest
 | FMul (temp,left,right)::FAdd (dest,addLeft,addRight)::rest when sameFReg temp addRight && not (sameFReg temp addLeft) && not (fRegUsedInInstrs temp rest) -> loop (FMadd (dest,left,right,addLeft)::acc) true rest
 | instr::rest -> loop (instr::acc) changed rest | [] -> List.rev acc,changed in loop [] false instrs

let tryRegisterPhiSources trueLabel falseLabel instrs=
 let sourceFor label sources=List.find_map (fun (operand,sourceLabel) -> if sourceLabel=label then Some operand else None) sources in
 let isSelectableScalarType = function Some (AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TBool | AST.TUnit | AST.TChar | AST.TDateTime | AST.TInternalRawPtr) -> true | _ -> false in
 let rec collect selects remaining=match remaining with
 | Phi (dest,sources,valueType)::rest when List.length sources=2 && isSelectableScalarType valueType -> (match sourceFor trueLabel sources,sourceFor falseLabel sources with Some (Reg whenTrue),Some (Reg whenFalse) -> collect ((dest,whenTrue,whenFalse)::selects) rest | _ -> None)
 | FPhi _::_ -> None | _ when selects=[] -> None
 | _ -> let selectInstrs=List.rev selects |> List.map (fun (dest,whenTrue,whenFalse) -> Select (dest,whenTrue,whenFalse,EQ)) in Some (selectInstrs,remaining) in collect [] instrs
(*
   Replace an empty two-arm scalar diamond with flag-based selects. The
   comparison and selected registers stay in the predecessor, and the join's
   phi definitions become ordinary select definitions.
*)
let formSelectDiamonds (cfg:cfg)=
 let tryRewrite blocks predecessors label block=
  let selection=match block.terminator with
  | CondBranch (condition,trueLabel,falseLabel) when (match List.rev block.instrs with Cmp _::_ -> true | _ -> false) -> Some (condition,trueLabel,falseLabel,block.instrs)
  | Branch (conditionReg,trueLabel,falseLabel) -> Some (NE,trueLabel,falseLabel,block.instrs @ [Cmp (conditionReg,Imm 0L)])
  | BranchZero (conditionReg,zeroLabel,nonZeroLabel) -> Some (EQ,zeroLabel,nonZeroLabel,block.instrs @ [Cmp (conditionReg,Imm 0L)]) | _ -> None in
  match selection with Some (condition,trueLabel,falseLabel,predecessorInstrs) when trueLabel<>falseLabel ->
   (match LabelMap.find_opt trueLabel blocks,LabelMap.find_opt falseLabel blocks with
   | Some trueBlock,Some falseBlock when trueBlock.instrs=[] && falseBlock.instrs=[] && LabelMap.find_opt trueLabel predecessors=Some [label] && LabelMap.find_opt falseLabel predecessors=Some [label] ->
    (match trueBlock.terminator,falseBlock.terminator with Jump trueJoin,Jump falseJoin when trueJoin=falseJoin ->
     (match LabelMap.find_opt trueJoin blocks with Some joinBlock ->
      (match tryRegisterPhiSources trueLabel falseLabel joinBlock.instrs with Some (selects,remainingJoinInstrs) ->
       let selects=List.map (function Select (dest,whenTrue,whenFalse,_) -> Select (dest,whenTrue,whenFalse,condition) | _ -> Crash.crash "Select formation created a non-select instruction") selects in
       Some ({block with instrs=predecessorInstrs @ selects;terminator=Jump trueJoin},trueLabel,falseLabel,trueJoin,{joinBlock with instrs=remainingJoinInstrs}) | None -> None)
     | None -> None) | _ -> None) | _ -> None) | _ -> None in
 let rec rewrite blocks changed=
  let predecessors=buildPredecessors {cfg with blocks} in
  let candidate=LabelMap.bindings blocks |> List.find_map (fun (label,block) -> Option.map (fun rewrite -> label,rewrite) (tryRewrite blocks predecessors label block)) in
  match candidate with None -> {cfg with blocks},changed | Some (label,(entryBlock,trueLabel,falseLabel,joinLabel,joinBlock)) ->
   let updated=blocks |> LabelMap.add label entryBlock |> LabelMap.add joinLabel joinBlock |> LabelMap.remove trueLabel |> LabelMap.remove falseLabel in rewrite updated true in rewrite cfg.blocks false
(*
   Try to fuse Cset + Branch into CondBranch
   Pattern: last instruction is Cset dest, cond; terminator is Branch dest, trueL, falseL
   Result: remove Cset, replace Branch with CondBranch cond, trueL, falseL
   Check if last instruction is Cset writing to condReg
   Removing Cset is valid only when this terminator is the Boolean's
   sole read. Virtual registers are function-scoped, so a successor
   may retain the comparison result even when this block does not.
   Fuse: remove Cset and replace Branch with CondBranch
*)
let tryFuseCondBranch regUseCounts instrs terminator=match terminator with
| Branch (condReg,trueLabel,falseLabel) -> (match List.rev instrs with Cset (dest,cond)::_ when sameReg dest condReg -> let otherInstrs=List.take (List.length instrs-1) instrs in let useCount=Option.value (RegMap.find_opt condReg regUseCounts) ~default:1 in if useCount=1 && not (isRegUsedInInstrs condReg otherInstrs) then Some (otherInstrs,CondBranch (cond,trueLabel,falseLabel)) else None | _ -> None)
| _ -> None
(*
   Eliminate a materialized Boolean negation used only by a branch.
   MIR lowers `not source` as `negated = 1 - source`; branching on that
   normalized Boolean is equivalent to branching on source with swapped edges.
   The Sub reads the materialized one and the terminator reads its
   result. A later use of that result needs both instructions retained.
*)
let tryFuseBooleanNotBranch regUseCounts instrs terminator=match terminator,List.rev instrs with
| Branch (branchReg,trueLabel,falseLabel),Sub (subDest,oneReg,Reg sourceReg)::Mov (oneDest,Imm 1L)::remainingReversed when sameReg branchReg subDest && sameReg subDest oneReg && sameReg oneReg oneDest && not (sameReg sourceReg branchReg) -> let useCount=Option.value (RegMap.find_opt branchReg regUseCounts) ~default:2 in if useCount=2 then Some (List.rev remainingReversed,Branch (sourceReg,falseLabel,trueLabel)) else None
| _ -> None
(*
   Check if a value is a power of 2 (exactly one bit set)
*)
let isPowerOf2 n=n>0L && Int64.logand n (Int64.sub n 1L)=0L
(*
   Get the bit position of a power-of-2 value (log2)
*)
let bitPosition n=let rec loop pos x=if x=1L then pos else loop (pos+1) (Int64.shift_right x 1) in loop 0 n
(*
   Try to fuse AND_imm (power-of-2 mask) + BranchZero/Branch into BranchBitZero/BranchBitNonZero
   Pattern: last instruction is AND_imm dest, src, mask where mask is power of 2
   terminator is BranchZero(dest, ...) or Branch(dest, ...)
   Result: BranchBitZero(src, bitNum, ...) or BranchBitNonZero(src, bitNum, ...)
   This uses TBZ/TBNZ instructions which test a single bit
   Check that andDest is not used in the remaining instructions
   AND_imm + CBZ → TBZ
   AND_imm + CBNZ → TBNZ
*)
let tryFuseAndBitBranch instrs terminator=match List.rev instrs with
| And_imm (andDest,andSrc,mask)::_ when isPowerOf2 mask ->
 let bit=bitPosition mask in let otherInstrs=List.take (List.length instrs-1) instrs in
 if isRegUsedInInstrs andDest otherInstrs then None else (match terminator with BranchZero (condReg,zeroLabel,nonZeroLabel) when sameReg condReg andDest -> Some (otherInstrs,BranchBitZero (andSrc,bit,zeroLabel,nonZeroLabel)) | Branch (condReg,nonZeroLabel,zeroLabel) when sameReg condReg andDest -> Some (otherInstrs,BranchBitNonZero (andSrc,bit,nonZeroLabel,zeroLabel)) | _ -> None)
| _ -> None
(*
   Try to fuse CMP reg, #0 + CondBranch into Branch/BranchZero
   Pattern: last instruction is CMP reg, #0; terminator is CondBranch(EQ/NE, ...)
   Result:
   - CMP reg, #0 + CondBranch(EQ, true, false) → BranchZero(reg, true, false)  [uses CBZ]
   - CMP reg, #0 + CondBranch(NE, true, false) → Branch(reg, true, false)      [uses CBNZ]
   Check if last instruction is CMP reg, #0
   CMP reg, #0 + B.eq → CBZ reg (BranchZero)
   CMP reg, #0 + B.ne → CBNZ reg (Branch)
   Other conditions (LT, GT, LE, GE) can't be fused with CBZ/CBNZ
*)
let tryFuseCmpZeroBranch instrs terminator=match terminator with
| CondBranch (cond,trueLabel,falseLabel) -> (match List.rev instrs with Cmp (cmpReg,Imm 0L)::_ -> let otherInstrs=List.take (List.length instrs-1) instrs in (match cond with EQ -> Some (otherInstrs,BranchZero (cmpReg,trueLabel,falseLabel)) | NE -> Some (otherInstrs,Branch (cmpReg,trueLabel,falseLabel)) | _ -> None) | _ -> None)
| _ -> None
(*
   Apply TBZ/TBNZ fusion if applicable
   Fuses AND_imm (power-of-2 mask) + BranchZero/Branch → BranchBitZero/BranchBitNonZero
*)
let applyAndBitBranchFusion instrs terminator=Option.value (tryFuseAndBitBranch instrs terminator) ~default:(instrs,terminator)
(*
   Optimize a basic block (returns whether anything changed)
   Apply multiply-by-constant strength reduction (Mov + Mul → Lsl + Add/Sub)
   Apply MUL + ADD → MADD fusion
   Apply MUL + SUB → MSUB fusion
   Drop a materialized Boolean negation when the branch can swap its edges.
   Try to fuse Cset + Branch into CondBranch
   After fusing Cset + Branch → CondBranch, try to fuse CMP #0 + CondBranch → CBZ/CBNZ
   Also try CMP #0 + CondBranch fusion on the original terminator
   Try to fuse AND_imm (power-of-2) + BranchZero/Branch → TBZ/TBNZ
*)
let optimizeBlockWithRegUseCounts fuseFloatMultiplyAdd regUseCounts block=
 let instrs',instructionChanged=optimizeInstrsWithChange block.instrs in
 let instrsCopyCleaned,floatingCopyChanged=removeRedundantFloatingCopyBackMovesWithChange instrs' in
 let instrsFloatCombined,floatingMultiplyAddChanged=if fuseFloatMultiplyAdd then tryFuseFloatMultiplyAdd instrsCopyCleaned else instrsCopyCleaned,false in
 let containsMultiply=List.exists (function Mul _ -> true | _ -> false) instrsFloatCombined in
 let instrs'',multiplyChanged=if not containsMultiply then instrsFloatCombined,false else let instrs1,c1=tryMulByConstantWithChange instrsFloatCombined in let instrs2,c2=tryFuseMulAddWithChange instrs1 in let instrs3,c3=tryFuseMulSubWithChange instrs2 in instrs3,c1 || c2 || c3 in
 let instrsBeforeCondBranch,terminatorBeforeCondBranch,booleanNotChanged=match tryFuseBooleanNotBranch regUseCounts instrs'' block.terminator with Some (is,term) -> is,term,true | None -> instrs'',block.terminator,false in
 let instrs''',terminator',conditionalBranchChanged=match tryFuseCondBranch regUseCounts instrsBeforeCondBranch terminatorBeforeCondBranch with
 | Some (is,term) -> (match tryFuseCmpZeroBranch is term with Some (is2,term2) -> is2,term2,true | None -> is,term,true)
 | None -> (match tryFuseCmpZeroBranch instrsBeforeCondBranch terminatorBeforeCondBranch with Some (is,term) -> is,term,true | None -> instrsBeforeCondBranch,terminatorBeforeCondBranch,false) in
 let finalInstrs,finalTerminator,bitBranchChanged=match tryFuseAndBitBranch instrs''' terminator' with Some (is,term) -> is,term,true | None -> instrs''',terminator',false in
 {block with instrs=finalInstrs;terminator=finalTerminator},(instructionChanged || floatingCopyChanged || floatingMultiplyAddChanged || multiplyChanged || booleanNotChanged || conditionalBranchChanged || bitBranchChanged)
let optimizeBlock block=optimizeBlockWithRegUseCounts false RegMap.empty block
(*
   Optimize a CFG in a single pass (returns whether anything changed)
*)
let optimizeCFGOnce fuseFloatMultiplyAdd (cfg:cfg) domCache=
 let regUseCounts=cfgRegUseCounts cfg in
 let blocks',changed=LabelMap.fold (fun label block (acc,ch) -> let block',blockChanged=optimizeBlockWithRegUseCounts fuseFloatMultiplyAdd regUseCounts block in LabelMap.add label block' acc,ch || blockChanged) cfg.blocks (LabelMap.empty,false) in
 let cfg'={cfg with blocks=blocks'} in let cfg'',hoisted,cache'=applyLoopInvariantConstHoist cfg' domCache in cfg'',changed || hoisted,cache'
(*
   Optimize a CFG until fixed point
*)
let optimizeCFGWithCosts fuseFloatMultiplyAdd (cfg:cfg)=
 validateCFGShape cfg;
 let rec loop current remaining iteration domCache=if remaining<=0 then current else let locallyOptimized,changed,nextCache=optimizeCFGOnce fuseFloatMultiplyAdd current domCache in let next,selectChanged=formSelectDiamonds locallyOptimized in if changed || selectChanged then loop next (remaining-1) (iteration+1) nextCache else next in
 loop cfg 10 1 None
let optimizeCFG cfg=optimizeCFGWithCosts false cfg
(*
   Optimize a function
*)
let optimizeFunction (func:functionDef)={func with cfg=optimizeCFG func.cfg}
(*
   Apply target-specific combines only when they are both semantically legal
   and reduce the selected target's instruction cost. Ordinary Float multiply
   followed by add has two language-visible rounding points, so neither target
   contracts it; ARM64 FMADD remains available for explicitly fused operations.
*)
let optimizeFunctionFor arch (func:functionDef)=let fuseFloatMultiplyAdd=match arch with Platform.ARM64 | Platform.X86_64 -> false in {func with cfg=optimizeCFGWithCosts fuseFloatMultiplyAdd func.cfg}
(*
   Optimize a program
*)
let optimizeProgram (Program (functions,variants,records))=let functions'=List.map optimizeFunction functions in Program (functions',variants,records)
