(* Unrolling.fs - Unroll bounded scalar counted-loop shapes. *)
[@@@warning "-4"]
open MIR
module F = MIROptimizationFacts
module S = SSA_Construction
module T = MIRLoopTopology
module I = MIRInduction
(*
   Scalar values can be duplicated or moved without changing ownership.
*)
let isScalarValueType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TBool | AST.TFloat64 | AST.TUnit -> true | _ -> false
type countedLoopUnrollCandidate = {header : label; latch : label; exit : label; guard : instr; guardResult : vReg; phiBackedges : operand VRegMap.t; latchBlock : basicBlock; exitInstrs : instr list; exitResult : operand}
let maxUnrolledBodyInstructions = 12
let isUnrollableScalarInstr = function Mov (_, _, Some typ) -> isScalarValueType typ | BinOp (_, _, _, _, typ) -> isScalarValueType typ | UnaryOp _ | FloatSqrt _ | FloatAbs _ | FloatNeg _ | Int64ToFloat _ | FloatToInt64 _ | FloatToBits _ -> true | _ -> false
let isScalarExitResult headerPhis exitInstrs = function Int64Const _ | BoolConst _ | FloatSymbol _ -> true | Register reg -> List.exists (fun instr -> F.getInstrDest instr = Some reg) (headerPhis @ exitInstrs) | _ -> false
let substituteUnrolledOperand substitutions operand = match operand with Register reg -> Option.value ~default:operand (VRegMap.find_opt reg substitutions) | _ -> operand
let cloneScalarInstr substitutions destination instr =
 let sub = substituteUnrolledOperand substitutions in match instr with
 | Mov (_, source, typ) -> Some (Mov (destination, sub source, typ))
 | BinOp (_, op, a, b, typ) -> Some (BinOp (destination, op, sub a, sub b, typ))
 | UnaryOp (_, op, src) -> Some (UnaryOp (destination, op, sub src))
 | FloatSqrt (_, src) -> Some (FloatSqrt (destination, sub src))
 | FloatAbs (_, src) -> Some (FloatAbs (destination, sub src))
 | FloatNeg (_, src) -> Some (FloatNeg (destination, sub src))
 | Int64ToFloat (_, src) -> Some (Int64ToFloat (destination, sub src))
 | FloatToInt64 (_, src) -> Some (FloatToInt64 (destination, sub src))
 | FloatToBits (_, src) -> Some (FloatToBits (destination, sub src))
 | _ -> None
let nextInt id = Int32.to_int (Int32.add (Int32.of_int id) 1l)
let cloneScalarInstrs first initial instrs =
 let state = List.fold_left (fun state instr -> match state, F.getInstrDest instr with Some (cloned, substitutions, next), Some original -> let destination = VReg next in (match cloneScalarInstr substitutions destination instr with Some instruction -> Some (instruction :: cloned, VRegMap.add original (Register destination) substitutions, nextInt next) | None -> None) | _ -> None) (Some ([], initial, first)) instrs in
 Option.map (fun (cloned, substitutions, next) -> List.rev cloned, substitutions, next) state
let isInvariantCountedLoopBound preheader latch loopBlocks (cfg : cfg) = function
 | Register register ->
  let definitions = LabelSet.elements loopBlocks |> List.concat_map (fun label -> match LabelMap.find_opt label cfg.blocks with Some block -> block.instrs | None -> []) |> List.filter (fun instr -> F.getInstrDest instr = Some register) in
  (match definitions with [] -> true | [Phi (destination, sources, Some AST.TInt64)] when destination = register ->
   let initial = List.filter (fun (_, label) -> label = preheader) sources and backedge = List.filter (fun (_, label) -> label = latch) sources in
   (match sources, initial, backedge, LabelMap.find_opt latch cfg.blocks with [_; _], [_], [(Register backedge, _)], Some block -> I.resolveLatchCopy block.instrs backedge = register | _ -> false) | _ -> false)
 | Int64Const _ -> true | _ -> false
let tryCountedLoopUnrollCandidate (cfg : cfg) header loopBlocks =
 let predecessors = S.buildPredecessors cfg in
 let preds = Option.value ~default:[] (LabelMap.find_opt header predecessors) in
 let outside = List.filter (fun label -> not (LabelSet.mem label loopBlocks)) preds and inside = List.filter (fun label -> LabelSet.mem label loopBlocks) preds in
 match outside, inside, LabelMap.find_opt header cfg.blocks with [preheader], [latch], Some headerBlock when LabelSet.equal loopBlocks (LabelSet.of_list [header; latch]) ->
  let headerPhis, headerBody = List.partition (function Phi _ -> true | _ -> false) headerBlock.instrs in
  (match LabelMap.find_opt preheader cfg.blocks, LabelMap.find_opt latch cfg.blocks, headerBody, headerBlock.terminator with
   | Some preheaderBlock, Some latchBlock, [(BinOp (guardResult, Gte, Register induction, bound, AST.TInt64) as guard)], Branch (Register condition, exitLabel, bodyLabel)
    when preheaderBlock.terminator = Jump header && bodyLabel = latch && condition = guardResult && latchBlock.terminator = Jump header && List.length latchBlock.instrs <= maxUnrolledBodyInstructions && List.for_all isUnrollableScalarInstr latchBlock.instrs && headerPhis <> [] && List.for_all (function Phi (_, _, Some typ) -> isScalarValueType typ | _ -> false) headerPhis && isInvariantCountedLoopBound preheader latch loopBlocks cfg bound ->
    let backedges = List.filter_map (function Phi (destination, sources, _) -> let initial = List.filter (fun (_, label) -> label = preheader) sources and backedge = List.filter (fun (_, label) -> label = latch) sources in (match sources, initial, backedge with [_; _], [_], [(value, _)] -> Some (destination, value) | _ -> None) | _ -> None) headerPhis in
    let advances = List.find_map (fun (destination, backedge) -> if destination <> induction then None else Some (match backedge with Register next -> let resolved = I.resolveLatchCopy latchBlock.instrs next in List.exists (I.isIncrementByOne AST.TInt64 induction resolved) latchBlock.instrs | _ -> false)) backedges |> Option.value ~default:false in
    (match backedges, advances, LabelMap.find_opt exitLabel cfg.blocks with values, true, Some exitBlock when List.length values = List.length headerPhis && LabelMap.find_opt exitLabel predecessors = Some [header] && List.length exitBlock.instrs <= maxUnrolledBodyInstructions && List.for_all isUnrollableScalarInstr exitBlock.instrs ->
     (match exitBlock.terminator with Ret result when isScalarExitResult headerPhis exitBlock.instrs result -> Some {header; latch; exit = exitLabel; guard; guardResult; phiBackedges = VRegMap.of_list values; latchBlock; exitInstrs = exitBlock.instrs; exitResult = result} | _ -> None)
    | _ -> None)
   | _ -> None)
 | _ -> None
let freshUnrollLabel (cfg : cfg) (Label baseName) suffix = let rec choose index = let numbered = if index = 0 then suffix else suffix ^ "_" ^ string_of_int index in let candidate = Label (baseName ^ "_" ^ numbered) in if LabelMap.mem candidate cfg.blocks then choose (nextInt index) else candidate in choose 0
let replaceLatchPhiSource candidate substitutions secondLatch = function Phi (destination, sources, typ) -> let sources = List.map (fun (operand, label) -> if label = candidate.latch then substituteUnrolledOperand substitutions operand, secondLatch else operand, label) sources in Phi (destination, sources, typ) | instr -> instr
(*
Only a two-block natural loop with one `i >= limit` guard is eligible. The
limit must be invariant, `i` must advance by exactly one, and both the latch and
scalar return path must fit the strict size cap. Calls, allocation, ownership,
memory access, and other effects are rejected. The first iteration retains its
original instruction order; cloned floating-point operations form the second
iteration in the same order, so evaluation is not reassociated.
*)
let applyCountedLoopUnrollingWithTopology (topology : T.loopTopology) (cfg : cfg) =
 let candidate = LabelMap.bindings topology.T.loops |> List.find_map (fun (header, loopBlocks) -> tryCountedLoopUnrollCandidate cfg header loopBlocks) in
 match candidate with None -> cfg, false | Some candidate ->
 let secondLatch = freshUnrollLabel cfg candidate.latch "unroll_second" in
 let remainderExit = freshUnrollLabel cfg candidate.exit "unroll_remainder" in
 let initial = candidate.phiBackedges and fresh = I.nextRegisterId cfg in
 match cloneScalarInstrs fresh initial [candidate.guard] with None -> cfg, false | Some (clonedGuard, guardSubstitutions, afterGuard) ->
 match VRegMap.find_opt candidate.guardResult guardSubstitutions with None -> cfg, false | Some clonedGuardResult ->
 match cloneScalarInstrs afterGuard initial candidate.latchBlock.instrs with None -> cfg, false | Some (secondInstrs, secondValues, afterSecond) ->
 match cloneScalarInstrs afterSecond initial candidate.exitInstrs with None -> cfg, false | Some (remainderInstrs, remainderValues, _) ->
 match LabelMap.find_opt candidate.header cfg.blocks with None -> cfg, false | Some headerBlock ->
 let headerBlock = {headerBlock with instrs = List.map (replaceLatchPhiSource candidate secondValues secondLatch) headerBlock.instrs} in
 let firstLatch = {candidate.latchBlock with instrs = candidate.latchBlock.instrs @ clonedGuard; terminator = Branch (clonedGuardResult, remainderExit, secondLatch)} in
 let secondBlock = {label = secondLatch; instrs = secondInstrs; terminator = Jump candidate.header} in
 let remainderBlock = {label = remainderExit; instrs = remainderInstrs; terminator = Ret (substituteUnrolledOperand remainderValues candidate.exitResult)} in
 let blocks = cfg.blocks |> LabelMap.add candidate.header headerBlock |> LabelMap.add candidate.latch firstLatch |> LabelMap.add secondLatch secondBlock |> LabelMap.add remainderExit remainderBlock in {cfg with blocks}, true
let applyCountedLoopUnrolling cfg = match T.tryBuildLoopTopology cfg with None -> cfg, false | Some topology -> applyCountedLoopUnrollingWithTopology topology cfg
