(* SparseConditionalConstants.fs - Joint SSA constant and CFG-edge analysis. *)
[@@@warning "-4"]
open MIR
module F = MIROptimizationFacts
module C = MIRCopyPropagation
module K = MIRConstants
module S = SSA_Construction
module EdgeOrder = struct type t = label * label let compare (Label a, Label b) (Label a', Label b') = let order = StringOrder.compare a a' in if order = 0 then StringOrder.compare b b' else order end
module EdgeMap = Map.Make (EdgeOrder)
module EdgeSet = Set.Make (EdgeOrder)
module IntMap = Map.Make (Int)
type latticeValue = Unknown | Constant of operand | IntegerRange of int64 * int64 | AggregateRefs of VRegSet.t | Overdefined
type pathFacts = {booleans : bool VRegMap.t; integerRanges : (int64 * int64) VRegMap.t}
let emptyPathFacts = {booleans = VRegMap.empty; integerRanges = VRegMap.empty}
type blockWorklist = {front : label list; back : label list}
let enqueueWorklist label worklist = {worklist with back = label :: worklist.back}
let tryDequeueWorklist worklist = match worklist.front with label :: rest -> Some (label, {worklist with front = rest}) | [] -> (match List.rev worklist.back with [] -> None | label :: rest -> Some (label, {front = rest; back = []}))
type analysisState = {values : latticeValue VRegMap.t; executableBlocks : LabelSet.t; executableEdges : EdgeSet.t; edgeFacts : pathFacts EdgeMap.t; blockFacts : pathFacts LabelMap.t; predecessors : label list LabelMap.t; definitions : instr VRegMap.t; heapValues : latticeValue IntMap.t VRegMap.t; trackHeapValues : bool; callResults : AST.functionId -> operand option; worklist : blockWorklist; pending : LabelSet.t}
let sameConstant a b = match a, b with Int64Const a, Int64Const b -> a = b | BoolConst a, BoolConst b -> a = b | FloatSymbol a, FloatSymbol b -> Int64.bits_of_float a = Int64.bits_of_float b | StringSymbol a, StringSymbol b -> a = b | FuncAddr a, FuncAddr b -> a = b | _ -> false
let sameLatticeValue a b = match a, b with Unknown, Unknown | Overdefined, Overdefined -> true | Constant a, Constant b -> sameConstant a b | IntegerRange (a, b), IntegerRange (c, d) -> a = c && b = d | AggregateRefs a, AggregateRefs b -> VRegSet.equal a b | _ -> false
let rangeContaining a b c d = IntegerRange (min a c, max b d)
let mergeLattice current incoming = match current, incoming with
 | Overdefined, _ | _, Overdefined -> Overdefined
 | Unknown, value | value, Unknown -> value
 | Constant a, Constant b when sameConstant a b -> current
 | Constant (Int64Const a), Constant (Int64Const b) -> rangeContaining a a b b
 | Constant (Int64Const value), IntegerRange (lo, hi) | IntegerRange (lo, hi), Constant (Int64Const value) -> rangeContaining lo hi value value
 | IntegerRange (a, b), IntegerRange (c, d) -> rangeContaining a b c d
 | AggregateRefs a, AggregateRefs b -> let merged = VRegSet.union a b in if VRegSet.cardinal merged <= 16 then AggregateRefs merged else Overdefined
 | Constant _, Constant _ | Constant _, IntegerRange _ | IntegerRange _, Constant _ | Constant _, AggregateRefs _ | AggregateRefs _, Constant _ | IntegerRange _, AggregateRefs _ | AggregateRefs _, IntegerRange _ -> Overdefined
let integerRangeForType = function AST.TInt8 -> Some (IntegerRange (-128L, 127L)) | AST.TInt16 -> Some (IntegerRange (-32768L, 32767L)) | AST.TInt32 -> Some (IntegerRange (-2147483648L, 2147483647L)) | AST.TUInt8 -> Some (IntegerRange (0L, 255L)) | AST.TUInt16 -> Some (IntegerRange (0L, 65535L)) | AST.TUInt32 -> Some (IntegerRange (0L, 4294967295L)) | _ -> None
let operandValue values = function Register register -> Option.value ~default:Unknown (VRegMap.find_opt register values) | (Int64Const _ | BoolConst _ | FloatSymbol _ | StringSymbol _ | FuncAddr _) as operand -> Constant operand
let valueForType typ value =
 let compatible = match typ, value with
 | AST.TFloat64, Constant (FloatSymbol _) | AST.TString, Constant (StringSymbol _) | AST.TChar, Constant (StringSymbol _) | AST.TBool, Constant (BoolConst _)
 | (AST.TInt64 | AST.TInternalRawPtr | AST.TFunction _), Constant (FuncAddr _)
 | (AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TDateTime | AST.TUnit), Constant (Int64Const _)
 | (AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32), IntegerRange _
 | (AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TDict _), AggregateRefs _
 | AST.TSum _, Constant (Int64Const _) | (AST.TList _ | AST.TDict _), Constant (Int64Const 0L) | _, Unknown | _, Overdefined -> true
 | _ -> false in
 if compatible then match value with Overdefined -> Option.value ~default:Overdefined (integerRangeForType typ) | _ -> value else Overdefined
let typedOperandValue values typ operand = valueForType typ (operandValue values operand)
let resolvedOperand values operand = match operandValue values operand with Constant constant -> constant | Unknown | IntegerRange _ | AggregateRefs _ | Overdefined -> operand
let evaluatedOperationValue values operands folded = match folded with Some result -> operandValue values result | None -> if List.mem Overdefined (List.map (operandValue values) operands) then Overdefined else Unknown
let tryFoldFloatBinOp op left right = match op, left, right with
 | Add, FloatSymbol a, FloatSymbol b -> Some (FloatSymbol (a +. b))
 | Sub, FloatSymbol a, FloatSymbol b -> Some (FloatSymbol (a -. b))
 | Mul, FloatSymbol a, FloatSymbol b -> Some (FloatSymbol (a *. b))
 | Div, FloatSymbol a, FloatSymbol b -> Some (FloatSymbol (a /. b))
 | Mod, FloatSymbol a, FloatSymbol b -> Some (FloatSymbol (mod_float a b))
 | Eq, FloatSymbol a, FloatSymbol b -> Some (BoolConst (a = b))
 | Neq, FloatSymbol a, FloatSymbol b -> Some (BoolConst (a <> b))
 | Lt, FloatSymbol a, FloatSymbol b -> Some (BoolConst (a < b))
 | Gt, FloatSymbol a, FloatSymbol b -> Some (BoolConst (a > b))
 | Lte, FloatSymbol a, FloatSymbol b -> Some (BoolConst (a <= b))
 | Gte, FloatSymbol a, FloatSymbol b -> Some (BoolConst (a >= b))
 | _ -> None
let tryRangeComparison op left right =
 let bounds = function Constant (Int64Const value) -> Some (value, value) | IntegerRange (lo, hi) -> Some (lo, hi) | _ -> None in
 match bounds left, bounds right with Some (a, b), Some (c, d) ->
  (match op with Eq when b < c || d < a -> Some (BoolConst false) | Eq when a = b && a = c && c = d -> Some (BoolConst true) | Neq when b < c || d < a -> Some (BoolConst true) | Neq when a = b && a = c && c = d -> Some (BoolConst false) | Lt when b < c -> Some (BoolConst true) | Lt when a >= d -> Some (BoolConst false) | Gt when a > d -> Some (BoolConst true) | Gt when b <= c -> Some (BoolConst false) | Lte when b <= c -> Some (BoolConst true) | Lte when a > d -> Some (BoolConst false) | Gte when a >= d -> Some (BoolConst true) | Gte when b < c -> Some (BoolConst false) | _ -> None)
 | _ -> None
let mergePathFacts left right =
 let booleans = VRegMap.filter (fun reg value -> VRegMap.find_opt reg right.booleans = Some value) left.booleans in
 let integerRanges = VRegMap.fold (fun reg (a, b) merged -> match VRegMap.find_opt reg right.integerRanges with Some (c, d) -> VRegMap.add reg (min a c, max b d) merged | None -> merged) left.integerRanges VRegMap.empty in {booleans; integerRanges}
let pathOperandValue state facts operand = match operand with Register reg -> (match VRegMap.find_opt reg facts.integerRanges with Some (lo, hi) -> (match operandValue state.values operand with Constant (Int64Const value) -> Constant (Int64Const value) | IntegerRange (a, b) -> IntegerRange (max a lo, min b hi) | _ -> IntegerRange (lo, hi)) | None -> operandValue state.values operand) | _ -> operandValue state.values operand
let isPathRangeType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 -> true | _ -> false
let knownBooleanValue state facts operand =
 let rec resolve visited operand = match operand with BoolConst value -> Some value | Register register ->
  (match VRegMap.find_opt register facts.booleans with Some value -> Some value | None when VRegSet.mem register visited -> None | None ->
   let visited = VRegSet.add register visited in
   let derived = match VRegMap.find_opt register state.definitions with
   | Some (Mov (_, source, _)) -> resolve visited source
   | Some (UnaryOp (_, Not, source)) -> Option.map not (resolve visited source)
   | Some (BinOp (_, And, left, right, AST.TBool)) -> (match resolve visited left, resolve visited right with Some false, _ | _, Some false -> Some false | Some true, Some true -> Some true | _ -> None)
   | Some (BinOp (_, Or, left, right, AST.TBool)) -> (match resolve visited left, resolve visited right with Some true, _ | _, Some true -> Some true | Some false, Some false -> Some false | _ -> None)
   | Some (BinOp (_, op, left, right, typ)) when isPathRangeType typ -> (match tryRangeComparison op (pathOperandValue state facts left) (pathOperandValue state facts right) with Some (BoolConst value) -> Some value | _ -> None)
   | _ -> None in
   match derived, operandValue state.values operand with Some value, _ -> Some value | None, Constant (BoolConst value) -> Some value | _ -> None)
 | _ -> None in resolve VRegSet.empty operand
let conditionValue state label condition = let facts = Option.value ~default:emptyPathFacts (LabelMap.find_opt label state.blockFacts) in match knownBooleanValue state facts condition with Some value -> Constant (BoolConst value) | None -> operandValue state.values condition
let reversedComparison = function Lt -> Some Gt | Lte -> Some Gte | Gt -> Some Lt | Gte -> Some Lte | (Eq | Neq) as op -> Some op | _ -> None
let refinedBound op takeTrue constant = match op, takeTrue with
 | Eq, true | Neq, false -> Some (constant, constant)
 | Lt, true | Gte, false when constant > Int64.min_int -> Some (Int64.min_int, Int64.sub constant 1L)
 | Lt, false | Gte, true -> Some (constant, Int64.max_int)
 | Lte, true | Gt, false -> Some (Int64.min_int, constant)
 | Lte, false | Gt, true when constant < Int64.max_int -> Some (Int64.add constant 1L, Int64.max_int)
 | _ -> None
let withComparisonRange definitions condition takeTrue facts = match VRegMap.find_opt condition definitions with
 | Some (BinOp (_, op, left, right, typ)) when isPathRangeType typ ->
  let candidate = match left, right with Register reg, Int64Const constant -> Some (reg, op, constant) | Int64Const constant, Register reg -> Option.map (fun op -> reg, op, constant) (reversedComparison op) | _ -> None in
  (match candidate with Some (reg, op, constant) -> (match refinedBound op takeTrue constant with Some (lo, hi) ->
   let typeLo, typeHi = match integerRangeForType typ with Some (IntegerRange (lo, hi)) -> lo, hi | _ -> Int64.min_int, Int64.max_int in
   let oldLo, oldHi = Option.value ~default:(typeLo, typeHi) (VRegMap.find_opt reg facts.integerRanges) in
   let lo = max oldLo (max lo typeLo) and hi = min oldHi (min hi typeHi) in
   if lo <= hi then {facts with integerRanges = VRegMap.add reg (lo, hi) facts.integerRanges} else facts
  | None -> facts) | None -> facts)
 | _ -> facts
let withDerivedBooleanFacts state condition takeTrue facts =
 let rec refineOperand visited operand value facts = match operand with Register reg -> refineRegister visited reg value facts | BoolConst established when established <> value -> None | _ -> Some facts
 and refineRegister visited reg value facts = match VRegMap.find_opt reg facts.booleans with Some established when established <> value -> None | _ when VRegSet.mem reg visited -> Some facts | _ ->
  let visited = VRegSet.add reg visited in let facts = {facts with booleans = VRegMap.add reg value facts.booleans} in
  match VRegMap.find_opt reg state.definitions with
  | Some (Mov (_, source, _)) -> refineOperand visited source value facts
  | Some (UnaryOp (_, Not, source)) -> refineOperand visited source (not value) facts
  | Some (BinOp (_, And, left, right, AST.TBool)) when value -> Option.bind (refineOperand visited left true facts) (refineOperand visited right true)
  | Some (BinOp (_, And, left, right, AST.TBool)) -> (match knownBooleanValue state facts left, knownBooleanValue state facts right with Some true, _ -> refineOperand visited right false facts | _, Some true -> refineOperand visited left false facts | _ -> Some facts)
  | Some (BinOp (_, Or, left, right, AST.TBool)) when not value -> Option.bind (refineOperand visited left false facts) (refineOperand visited right false)
  | Some (BinOp (_, Or, left, right, AST.TBool)) -> (match knownBooleanValue state facts left, knownBooleanValue state facts right with Some false, _ -> refineOperand visited right true facts | _, Some false -> refineOperand visited left true facts | _ -> Some facts)
  | Some (BinOp (_, _, _, _, typ)) when isPathRangeType typ -> Some (withComparisonRange state.definitions reg value facts)
  | _ -> Some facts in refineRegister VRegSet.empty condition takeTrue facts
let operationValue values operands folded = match folded with Some result -> operandValue values result | None -> if List.mem Overdefined operands then Overdefined else if List.mem Unknown operands then Unknown else Overdefined
let instructionValue state blockLabel instr =
 let values = state.values in match instr with
 | Mov (dest, source, typ) -> Some (dest, (match typ with Some typ -> typedOperandValue values typ source | None -> operandValue values source))
 | BinOp (dest, op, left, right, typ) ->
  let leftValue = typedOperandValue values typ left and rightValue = typedOperandValue values typ right in
  let left = resolvedOperand values left and right = resolvedOperand values right in
  let folded = match typ with AST.TFloat64 -> tryFoldFloatBinOp op left right | _ -> (match K.tryFoldBinOp op left right typ with Some result -> Some result | None -> tryRangeComparison op leftValue rightValue) in
  Some (dest, operationValue values [leftValue; rightValue] folded)
 | UnaryOp (dest, op, source) -> let folded = match op, resolvedOperand values source with Neg, Int64Const value -> Some (Int64Const (Int64.neg value)) | Not, BoolConst value -> Some (BoolConst (not value)) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | Phi (dest, sources, typ) -> let value = List.filter (fun (_, from) -> EdgeSet.mem (from, blockLabel) state.executableEdges) sources |> List.fold_left (fun merged (source, _) -> let incoming = match typ with Some typ -> typedOperandValue values typ source | None -> operandValue values source in mergeLattice merged incoming) Unknown in Some (dest, value)
 | HeapAlloc (dest, _) when state.trackHeapValues -> Some (dest, AggregateRefs (VRegSet.singleton dest))
 | HeapAlloc (dest, _) -> Some (dest, Overdefined)
 | HeapLoad (dest, address, offset, typ) ->
  let value = match Option.value ~default:Unknown (VRegMap.find_opt address values) with
  | AggregateRefs allocations -> VRegSet.fold (fun allocation merged -> let incoming = Option.bind (VRegMap.find_opt allocation state.heapValues) (IntMap.find_opt offset) |> Option.value ~default:Unknown in mergeLattice merged incoming) allocations Unknown
  | Unknown -> Unknown | Constant _ | IntegerRange _ | Overdefined -> Overdefined in
  Some (dest, (match typ with Some typ -> valueForType typ value | None -> value))
 | StringConcat (dest, first, second, remaining) -> let values = List.map (operandValue values) (first :: second :: remaining) in let strings = List.filter_map (function Constant (StringSymbol value) -> Some value | _ -> None) values in let value = if List.length strings = List.length values then Constant (StringSymbol (String.concat "" strings)) else if List.mem Overdefined values then Overdefined else Unknown in Some (dest, value)
 | CanonicalBufferEq (dest, _, left, right) -> let a = operandValue values left and b = operandValue values right in let value = match a, b with Constant (StringSymbol a), Constant (StringSymbol b) -> Constant (BoolConst (a = b)) | _ when a = Overdefined || b = Overdefined -> Overdefined | _ -> Unknown in Some (dest, value)
 | FloatSqrt (dest, source) -> let folded = match resolvedOperand values source with FloatSymbol value -> Some (FloatSymbol (sqrt value)) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | FloatAbs (dest, source) -> let folded = match resolvedOperand values source with FloatSymbol value -> Some (FloatSymbol (abs_float value)) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | FloatNeg (dest, source) -> let folded = match resolvedOperand values source with FloatSymbol value -> Some (FloatSymbol (-. value)) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | Int64ToFloat (dest, source) -> let folded = match resolvedOperand values source with Int64Const value -> Some (FloatSymbol (Int64.to_float value)) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | FloatToInt64 (dest, source) -> let folded = match resolvedOperand values source with FloatSymbol value when Float.is_finite value && value >= -9223372036854775808. && value < 9223372036854775808. -> Some (Int64Const (Int64.of_float (Float.trunc value))) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | FloatToBits (dest, source) -> let folded = match resolvedOperand values source with FloatSymbol value -> Some (Int64Const (Int64.bits_of_float value)) | _ -> None in Some (dest, evaluatedOperationValue values [source] folded)
 | Call (dest, fn, _, _, typ) -> let value = match state.callResults fn with Some result -> typedOperandValue values typ result | None -> Option.value ~default:Overdefined (integerRangeForType typ) in Some (dest, value)
 | IndirectCall (dest, _, _, _, typ) | ClosureCall (dest, _, _, _, typ) -> Some (dest, Option.value ~default:Overdefined (integerRangeForType typ))
 | other -> Option.map (fun dest -> dest, Overdefined) (F.getInstrDest other)
let buildRegisterUsers (cfg : cfg) =
 let add label users reg = let current = Option.value ~default:LabelSet.empty (VRegMap.find_opt reg users) in VRegMap.add reg (LabelSet.add label current) users in
 LabelMap.fold (fun label block users -> let users = List.fold_left (F.foldInstrUses (add label)) users block.instrs in F.foldTerminatorUses (add label) users block.terminator) cfg.blocks VRegMap.empty
let enqueueBlock label state = if LabelSet.mem label state.executableBlocks && not (LabelSet.mem label state.pending) then {state with worklist = enqueueWorklist label state.worklist; pending = LabelSet.add label state.pending} else state
let updateValue users dest incoming state =
 let current = Option.value ~default:Unknown (VRegMap.find_opt dest state.values) in let merged = mergeLattice current incoming in
 if sameLatticeValue merged current then state else
 let state = {state with values = VRegMap.add dest merged state.values} in
 LabelSet.fold (fun label state -> enqueueBlock label state) (Option.value ~default:LabelSet.empty (VRegMap.find_opt dest users)) state
let factsOnEdge (cfg : cfg) source target state =
 let facts = Option.value ~default:emptyPathFacts (LabelMap.find_opt source state.blockFacts) in
 match LabelMap.find_opt source cfg.blocks with Some {terminator = Branch (Register condition, yes, no); _} when yes <> no ->
 let takeTrue = target = yes in (match knownBooleanValue state facts (Register condition) with Some established when established <> takeTrue -> emptyPathFacts | _ -> Option.value ~default:emptyPathFacts (withDerivedBooleanFacts state condition takeTrue facts)) | _ -> facts
let samePathFacts left right = VRegMap.equal Bool.equal left.booleans right.booleans && VRegMap.equal (=) left.integerRanges right.integerRanges
let sameOptionalFacts value facts = match value with Some value -> samePathFacts value facts | None -> false
let distinctLabels values = let _, reversed = List.fold_left (fun (seen, result) value -> if LabelSet.mem value seen then seen, result else LabelSet.add value seen, value :: result) (LabelSet.empty, []) values in List.rev reversed
let refreshBlockFacts (cfg : cfg) target state = if target = cfg.entry then state else
 let incoming = Option.value ~default:[] (LabelMap.find_opt target state.predecessors) |> distinctLabels |> List.filter_map (fun predecessor -> let edge = predecessor, target in if EdgeSet.mem edge state.executableEdges then Some (Option.value ~default:emptyPathFacts (EdgeMap.find_opt edge state.edgeFacts)) else None) in
 match incoming with [] -> state | first :: rest -> let merged = List.fold_left mergePathFacts first rest in if sameOptionalFacts (LabelMap.find_opt target state.blockFacts) merged then state else enqueueBlock target {state with blockFacts = LabelMap.add target merged state.blockFacts}
let labelText (Label text) = HostStructuralFormat.format (HostStructuralFormat.Union ("Label", [HostStructuralFormat.Text text]))
let activateEdge (cfg : cfg) source target state =
 let edge = source, target in let facts = factsOnEdge cfg source target state in
 if EdgeSet.mem edge state.executableEdges && sameOptionalFacts (EdgeMap.find_opt edge state.edgeFacts) facts then state else
 let state = {state with executableEdges = EdgeSet.add edge state.executableEdges; executableBlocks = LabelSet.add target state.executableBlocks; edgeFacts = EdgeMap.add edge facts state.edgeFacts} in
 let state = if LabelMap.mem target cfg.blocks then enqueueBlock target state else Crash.crash ("SCCP edge targets missing block " ^ labelText target) in refreshBlockFacts cfg target state
let recordHeapStore users address offset source state = if not state.trackHeapValues then state else match Option.value ~default:Unknown (VRegMap.find_opt address state.values) with
 | AggregateRefs allocations ->
  let incoming = operandValue state.values source in
  let heap, changed = VRegSet.fold (fun allocation (heap, changed) -> let fields = Option.value ~default:IntMap.empty (VRegMap.find_opt allocation heap) in let current = Option.value ~default:Unknown (IntMap.find_opt offset fields) in let merged = mergeLattice current incoming in if sameLatticeValue merged current then heap, changed else VRegMap.add allocation (IntMap.add offset merged fields) heap, true) allocations (state.heapValues, false) in
  if changed then let state = {state with heapValues = heap} in LabelSet.fold (fun label state -> enqueueBlock label state) (Option.value ~default:LabelSet.empty (VRegMap.find_opt address users)) state else state
 | Unknown | Constant _ | IntegerRange _ | Overdefined -> state
let analyze callResults (cfg : cfg) =
 let users = buildRegisterUsers cfg in
 let definitions = LabelMap.fold (fun _ block definitions -> List.fold_left (fun definitions instr -> match F.getInstrDest instr with Some dest -> VRegMap.add dest instr definitions | None -> definitions) definitions block.instrs) cfg.blocks VRegMap.empty in
 let liveIns = VRegMap.fold (fun reg _ registers -> if VRegMap.mem reg definitions then registers else VRegSet.add reg registers) users VRegSet.empty in
 let values = VRegSet.fold (fun reg values -> VRegMap.add reg Overdefined values) liveIns VRegMap.empty in
 let hasAggregatePhi = LabelMap.exists (fun _ block -> List.exists (function Phi (_, _, Some (AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TDict _)) -> true | _ -> false) block.instrs) cfg.blocks in
 let hasHeapLoad = LabelMap.exists (fun _ block -> List.exists (function HeapLoad _ -> true | _ -> false) block.instrs) cfg.blocks in
 let initial = {values; executableBlocks = LabelSet.singleton cfg.entry; executableEdges = EdgeSet.empty; edgeFacts = EdgeMap.empty; blockFacts = LabelMap.singleton cfg.entry emptyPathFacts; predecessors = S.buildPredecessors cfg; definitions; heapValues = VRegMap.empty; trackHeapValues = hasAggregatePhi && hasHeapLoad; callResults; worklist = {front = [cfg.entry]; back = []}; pending = LabelSet.singleton cfg.entry} in
 let rec work state = match tryDequeueWorklist state.worklist with
 | None -> let unresolved = LabelSet.elements state.executableBlocks |> List.find_map (fun label -> match LabelMap.find_opt label cfg.blocks with Some {terminator = Branch (condition, yes, no); _} when conditionValue state label condition = Unknown && (not (EdgeSet.mem (label, yes) state.executableEdges) || not (EdgeSet.mem (label, no) state.executableEdges)) -> Some (label, yes, no) | _ -> None) in
  (match unresolved with None -> state | Some (label, yes, no) -> work (activateEdge cfg label no (activateEdge cfg label yes state)))
 | Some (label, remaining) -> let state = {state with worklist = remaining; pending = LabelSet.remove label state.pending} in
  (match LabelMap.find_opt label cfg.blocks with None -> Crash.crash ("SCCP worklist contains missing block " ^ labelText label) | Some block ->
   let state = List.fold_left (fun state instr -> let state = match instr with HeapStore (address, offset, source, _) -> recordHeapStore users address offset source state | _ -> state in match instructionValue state label instr with Some (dest, value) -> updateValue users dest value state | None -> state) state block.instrs in
   let state = match block.terminator with Ret _ -> state | Jump target -> activateEdge cfg label target state | Branch (condition, yes, no) ->
    (match conditionValue state label condition with Constant (BoolConst true) -> activateEdge cfg label yes state | Constant (BoolConst false) -> activateEdge cfg label no state | Unknown -> state | Constant _ | IntegerRange _ | AggregateRefs _ | Overdefined -> activateEdge cfg label no (activateEdge cfg label yes state)) in work state) in work initial
let equalCFG (left : cfg) (right : cfg) = left.entry = right.entry && LabelMap.equal (fun (left : basicBlock) (right : basicBlock) -> left.label = right.label && left.instrs = right.instrs && left.terminator = right.terminator) left.blocks right.blocks
(*
   A single unconditional block has no edge or phi facts to solve. Its
   copy-resolved instruction rewrite still folds local expressions below.
   Copy substitution and constant materialization share this rewrite.
   Phi operands retain their incoming-edge identities.
   SCCP's lattice records exact values, while the local folder
   also recognizes identities that yield a nonconstant register.
   Fold copied operands before Float literal bits are converted
   for lowering, preserving the earlier pass order's identities.
*)
let applyToCFG callResults copies (cfg : cfg) =
 let straight = match LabelMap.find_opt cfg.entry cfg.blocks with Some {terminator = Ret _; _} when LabelMap.cardinal cfg.blocks = 1 -> true | _ -> false in
 let analysis = if straight then None else Some (analyze callResults cfg) in
 let constantFor reg = match Option.bind analysis (fun facts -> VRegMap.find_opt reg facts.values) with Some (Constant constant) -> Some constant | Some Unknown | Some (IntegerRange _) | Some (AggregateRefs _) | Some Overdefined | None -> None in
 let rewriteInstruction label instr =
  let copied = match instr with Phi _ -> instr | _ -> C.propagateCopyInstr copies instr in
  match copied with
  | Mov (dest, _, typ) -> (match constantFor dest with Some constant -> Mov (dest, constant, typ) | None -> copied)
  | BinOp (dest, op, left, right, typ) ->
   let resultType = match op with Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> AST.TBool | _ -> typ in
   (match constantFor dest with Some constant -> Mov (dest, constant, Some resultType) | None ->
    match K.tryFoldBinOp op left right typ with Some result -> Mov (dest, result, None) | None when typ = AST.TFloat64 ->
     let floatOperand = function Int64Const bits -> FloatSymbol (Int64.float_of_bits bits) | operand -> operand in BinOp (dest, op, floatOperand left, floatOperand right, typ)
    | None -> copied)
  | UnaryOp (dest, _, _) -> (match constantFor dest with Some constant -> Mov (dest, constant, None) | None -> copied)
  | Phi (dest, sources, typ) -> (match constantFor dest with Some constant -> Mov (dest, constant, typ) | None -> let sources = List.filter (fun (_, predecessor) -> match analysis with Some facts -> EdgeSet.mem (predecessor, label) facts.executableEdges | None -> true) sources in Phi (dest, sources, typ))
  | CanonicalBufferEq (dest, _, _, _) -> (match constantFor dest with Some ((BoolConst _) as constant) -> Mov (dest, constant, Some AST.TBool) | _ -> copied)
  | FloatSqrt (dest, _) | FloatAbs (dest, _) | FloatNeg (dest, _) | Int64ToFloat (dest, _) -> (match constantFor dest with Some ((FloatSymbol _) as constant) -> Mov (dest, constant, Some AST.TFloat64) | _ -> copied)
  | FloatToBits (dest, _) -> (match constantFor dest with Some ((Int64Const _) as constant) -> Mov (dest, constant, Some AST.TUInt64) | _ -> copied)
  | FloatToInt64 (dest, _) -> (match constantFor dest with Some ((Int64Const _) as constant) -> Mov (dest, constant, Some AST.TInt64) | _ -> copied)
  | HeapLoad (dest, _, _, typ) -> (match constantFor dest, typ with Some ((FloatSymbol _) as constant), Some AST.TFloat64 -> Mov (dest, constant, typ) | Some ((Int64Const _ | BoolConst _) as constant), _ -> Mov (dest, constant, typ) | _ -> copied)
  | _ -> copied in
 let rewriteTerminator label terminator =
  let terminator = C.propagateCopyTerminator copies terminator in match terminator with
  | Branch (_, yes, no) when yes = no -> Jump yes
  | Branch (BoolConst true, yes, _) -> Jump yes
  | Branch (BoolConst false, _, no) -> Jump no
  | Branch (Register condition, yes, no) -> let proven = match analysis with Some facts -> conditionValue facts label (Register condition) | None -> Unknown in
   (match proven with Constant (BoolConst true) -> Jump yes | Constant (BoolConst false) -> Jump no | _ -> match constantFor condition with Some (BoolConst true) -> Jump yes | Some (BoolConst false) -> Jump no | _ -> terminator)
  | _ -> terminator in
 let blocks = LabelMap.filter (fun label _ -> match analysis with Some facts -> LabelSet.mem label facts.executableBlocks | None -> true) cfg.blocks |> LabelMap.mapi (fun label block -> {block with instrs = List.map (rewriteInstruction label) block.instrs; terminator = rewriteTerminator label block.terminator}) in
 let optimized = {cfg with blocks} in optimized, not (equalCFG optimized cfg)
let applySparseConditionalConstantPropagationWithCallResults callResults (cfg : cfg) = let conditional = LabelMap.exists (fun _ block -> match block.terminator with Branch _ -> true | Ret _ | Jump _ -> false) cfg.blocks in if conditional then applyToCFG callResults VRegMap.empty cfg else cfg, false
(*
   Discover copy aliases once, then rewrite them with SCCP's constants and
   reachable edges. Straight-line returns use the local rewrite alone.
*)
let applySparseConditionalSimplification cfg = let copies = C.resolveCopyMap (C.buildCopyMap cfg) in applyToCFG (fun _ -> None) copies cfg
let applySparseConditionalConstantPropagation cfg = applySparseConditionalConstantPropagationWithCallResults (fun _ -> None) cfg
