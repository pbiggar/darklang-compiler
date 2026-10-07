(* CommonExpressions.fs - Reuse and path-complete scalar expressions under effect constraints. *)
[@@@warning "-4"]
open MIR
module S = SSA_Construction
module T = MIRLoopTopology
(*
   Common Subexpression Elimination (CSE)
   Detect identical computations and replace with reference to first result
   Expression key for CSE - represents a pure computation
*)
type exprKey =
 | BinExpr of binOp * operand * operand * AST.semanticType
 | UnaryExpr of unaryOp * operand
 | ScalarHeapLoadExpr of vReg * int * AST.semanticType
 | DirectCallExpr of AST.functionId * operand list * AST.semanticType
let compareOperand left right =
 let tag = function Int64Const _ -> 0 | BoolConst _ -> 1 | FloatSymbol _ -> 2 | StringSymbol _ -> 3 | Register _ -> 4 | FuncAddr _ -> 5 in
 match left, right with
 | Int64Const a, Int64Const b -> Int64.compare a b
 | BoolConst a, BoolConst b -> Bool.compare a b
 | FloatSymbol a, FloatSymbol b -> Float.compare a b
 | StringSymbol a, StringSymbol b -> StringOrder.compare a b
 | Register (VReg a), Register (VReg b) -> Int.compare a b
 | FuncAddr a, FuncAddr b -> Int64.unsigned_compare (AST.functionIdValue a) (AST.functionIdValue b)
 | _ -> Int.compare (tag left) (tag right)
let chain comparisons = let rec first = function [] -> 0 | value :: rest -> if value = 0 then first rest else value in first comparisons
let rec compareOperands left right = match left, right with [], [] -> 0 | [], _ -> -1 | _, [] -> 1 | a :: aa, b :: bb -> let order = compareOperand a b in if order = 0 then compareOperands aa bb else order
let compareKey left right =
 let tag = function BinExpr _ -> 0 | UnaryExpr _ -> 1 | ScalarHeapLoadExpr _ -> 2 | DirectCallExpr _ -> 3 in
 match left, right with
 | BinExpr (op, a, b, typ), BinExpr (op', a', b', typ') -> chain [Stdlib.compare op op'; compareOperand a a'; compareOperand b b'; AST.compareSemanticType typ typ']
 | UnaryExpr (op, a), UnaryExpr (op', a') -> chain [Stdlib.compare op op'; compareOperand a a']
 | ScalarHeapLoadExpr (VReg a, offset, typ), ScalarHeapLoadExpr (VReg b, offset', typ') -> chain [Int.compare a b; Int.compare offset offset'; AST.compareSemanticType typ typ']
 | DirectCallExpr (fn, args, typ), DirectCallExpr (fn', args', typ') -> chain [Int64.unsigned_compare (AST.functionIdValue fn) (AST.functionIdValue fn'); compareOperands args args'; AST.compareSemanticType typ typ']
 | _ -> Int.compare (tag left) (tag right)
module ExprMap = Map.Make (struct type t = exprKey let compare = compareKey end)
type exprAvailability = {arithmetic : vReg ExprMap.t; scalarHeapLoads : vReg ExprMap.t; directCalls : vReg ExprMap.t}
let emptyExprAvailability = {arithmetic = ExprMap.empty; scalarHeapLoads = ExprMap.empty; directCalls = ExprMap.empty}
let tryFindAvailable key available = ExprMap.find_opt key (match key with BinExpr _ | UnaryExpr _ -> available.arithmetic | ScalarHeapLoadExpr _ -> available.scalarHeapLoads | DirectCallExpr _ -> available.directCalls)
let addAvailable key dest available = match key with
 | BinExpr _ | UnaryExpr _ -> {available with arithmetic = ExprMap.add key dest available.arithmetic}
 | ScalarHeapLoadExpr _ -> {available with scalarHeapLoads = ExprMap.add key dest available.scalarHeapLoads}
 | DirectCallExpr _ -> {available with directCalls = ExprMap.add key dest available.directCalls}
(*
   Check if a binary operation is commutative (order of operands doesn't matter)
*)
let isCommutative = function Add | Mul | And | Or | Eq | Neq | BitAnd | BitOr | BitXor -> true | Sub | Div | Mod | Lt | Gt | Lte | Gte | Shl | Shr -> false
(*
   Normalize operand order for commutative operations (for consistent hashing)
   Use structural comparison to ensure consistent ordering
*)
let normalizeOperands op left right = if isCommutative op && compareOperand left right > 0 then right, left else left, right
(*
   Build expression key for a BinOp
*)
let makeBinExprKey op left right typ = let a, b = normalizeOperands op left right in BinExpr (op, a, b, typ)
(*
   Build expression key for a UnaryOp
*)
let makeUnaryExprKey op src = UnaryExpr (op, src)
(*
   Build an availability key for an exact typed scalar heap load.
*)
let makeScalarHeapLoadExprKey addr offset typ = ScalarHeapLoadExpr (addr, offset, typ)
let isCrossBlockCSEType = function AST.TInt64 | AST.TInt32 | AST.TInt16 | AST.TInt8 | AST.TUInt64 | AST.TUInt32 | AST.TUInt16 | AST.TUInt8 | AST.TFloat64 | AST.TBool | AST.TChar | AST.TDateTime -> true | _ -> false
(*
   Calls, memory operations, and ownership operations invalidate heap-load
   availability without discarding independent arithmetic expression keys.
*)
let clearScalarHeapLoadAvailability available = {available with scalarHeapLoads = ExprMap.empty}
let clearDirectCallAvailability available = {available with directCalls = ExprMap.empty}
let clearHeapLoadAndDirectCallAvailability available = {available with scalarHeapLoads = ExprMap.empty; directCalls = ExprMap.empty}
type partialRedundancyCandidate = {block : label; dest : vReg; key : exprKey; instr : instr; valueType : AST.semanticType option; operands : operand list}
let tryPartialRedundancyCandidate block = function
 | BinOp (dest, op, left, right, typ) as instr when isCrossBlockCSEType typ && op <> Div && op <> Mod -> Some {block; dest; key = makeBinExprKey op left right typ; instr; valueType = Some typ; operands = [left; right]}
 | UnaryOp (dest, op, src) as instr -> Some {block; dest; key = makeUnaryExprKey op src; instr; valueType = None; operands = [src]}
 | _ -> None
let replaceInstrWithPhi candidate sources block =
 let rec split reversed = function (Phi _ as phi) :: rest -> split (phi :: reversed) rest | remaining -> List.rev reversed, remaining in
 let phis, remaining = split [] block.instrs in
 let remaining = List.filter (function BinOp (dest, _, _, _, _) | UnaryOp (dest, _, _) -> dest <> candidate.dest | _ -> true) remaining in
 {block with instrs = phis @ [Phi (candidate.dest, sources, candidate.valueType)] @ remaining}
let withDest dest = function BinOp (_, op, a, b, typ) -> BinOp (dest, op, a, b, typ) | UnaryOp (_, op, a) -> UnaryOp (dest, op, a) | _ -> Crash.crash "MIR PRE: candidate is not an arithmetic expression"
let labelText (Label name) = StructuralFormat.format (StructuralFormat.Union ("Label", [StructuralFormat.Text name]))
let nextInt value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
(*
   Complete expressions that are available on only some incoming paths. The
   insertion boundary is an unconditional edge into the join, so PRE neither
   speculates work onto another successor nor changes when trapping operations
   run. Expressions depending on join-local definitions are not movable.
*)
let applyPartialRedundancyElimination exitAvailability (cfg : cfg) =
 let predecessors = S.buildPredecessors cfg in
 let candidates = LabelMap.bindings cfg.blocks |> List.concat_map (fun (label, block) -> List.filter_map (tryPartialRedundancyCandidate label) block.instrs) in
 let maximum = LabelMap.fold (fun _ block maximum -> VRegSet.fold (fun (VReg id) maximum -> max id maximum) (VRegSet.union (S.getBlockDefs block) (S.getBlockUses block)) maximum) cfg.blocks (-1) in
 let find message label blocks = match LabelMap.find_opt label blocks with Some block -> block | None -> Crash.crash (message ^ labelText label) in
 let rec apply remaining blocks availability nextRegId changed = match remaining with [] -> blocks, changed | candidate :: rest ->
  let block = find "MIR PRE: missing block " candidate.block blocks in
  let localDefs = S.getBlockDefs block in
  let usesJoinLocalDefinition = List.exists (function Register reg -> VRegSet.mem reg localDefs | _ -> false) candidate.operands in
  let _, incoming = List.fold_left (fun (seen, result) label -> if LabelSet.mem label seen then seen, result else LabelSet.add label seen, label :: result) (LabelSet.empty, []) (Option.value ~default:[] (LabelMap.find_opt candidate.block predecessors)) in
  let incoming = List.rev incoming in
  let incomingAvailability = List.map (fun predecessor -> predecessor, Option.bind (LabelMap.find_opt predecessor availability) (tryFindAvailable candidate.key)) incoming in
  let hasAvailablePath = List.exists (fun (_, value) -> Option.is_some value) incomingAvailability in
  let missingPathsCanInsert = List.for_all (fun (predecessor, value) -> match value, LabelMap.find_opt predecessor blocks with Some _, _ -> true | None, Some block -> block.terminator = Jump candidate.block | None, None -> false) incomingAvailability in
  if List.length incoming < 2 || usesJoinLocalDefinition || not hasAvailablePath || not missingPathsCanInsert then apply rest blocks availability nextRegId changed else
  let blocks, availability, nextRegId, sources = List.fold_left (fun (blocks, availability, fresh, sources) (predecessor, value) -> match value with
   | Some reg -> blocks, availability, fresh, (Register reg, predecessor) :: sources
   | None -> let dest = VReg fresh in let block = find "MIR PRE: missing predecessor " predecessor blocks in
    let block = {block with instrs = block.instrs @ [withDest dest candidate.instr]} in
    let available = addAvailable candidate.key dest (Option.value ~default:emptyExprAvailability (LabelMap.find_opt predecessor availability)) in
    LabelMap.add predecessor block blocks, LabelMap.add predecessor available availability, nextInt fresh, (Register dest, predecessor) :: sources) (blocks, availability, nextRegId, []) incomingAvailability in
  let block = find "MIR PRE: missing join " candidate.block blocks in
  apply rest (LabelMap.add candidate.block (replaceInstrWithPhi candidate (List.rev sources) block) blocks) availability nextRegId true in
 let blocks, changed = apply candidates cfg.blocks exitAvailability (nextInt maximum) false in {cfg with blocks}, changed
(*
   Apply CSE and PRE to a CFG, carrying available expressions into dominated
   blocks and completing safe expressions at joins.
   Unknown and non-scalar values can carry ownership edges;
   do not make earlier scalar loads available past them.
   Exact callee, operand, and scalar-result identity plus
   the whole-program effect proof make reuse safe.
   Keep only the current call available across a call
   boundary; independent arithmetic and safe loads remain.
   Unproven calls may affect memory and observable state.
   A previously computed raw address can outlive its managed
   owner if reuse removes the later use that kept it alive.
   Pure scalar instructions cannot affect memory, so exact
   scalar loads remain reusable locally. Preserve the bounded
   direct-call and cross-block live-range policy.
   Do not extend a new cross-block live range across calls,
   allocations, memory operations, or other runtime lowering.
   Local CSE remains available through exprMap.
   Each child receives expressions available from its dominators. Availability
   is cleared by the barriers above, and the same immutable map is passed to
   siblings so expressions never flow between non-dominating paths.
   Dominators are undefined for unreachable blocks. Retain local CSE there so
   this transformation remains complete when invoked independently.
*)
let applyCSEWithEffectFreeCallsAndTopology existingTopology effectFreeFunctions (cfg : cfg) =
 let optimizeBlock available block =
  let instrs, _, exported, changed = List.fold_left (fun (instrs, exprMap, exported, changed) instr ->
   let retain exprMap exported = instr :: instrs, exprMap, exported, changed in
   let reuse dest typ previous exprMap = Mov (dest, Register previous, typ) :: instrs, exprMap, exported, true in
   match instr with
   | BinOp (dest, op, left, right, typ) -> let key = makeBinExprKey op left right typ in
    let available = if isCrossBlockCSEType typ then exprMap else clearScalarHeapLoadAvailability exprMap in
    (match tryFindAvailable key available with Some previous -> reuse dest None previous available | None -> retain (addAvailable key dest available) (if isCrossBlockCSEType typ then addAvailable key dest exported else emptyExprAvailability))
   | UnaryOp (dest, op, src) -> let key = makeUnaryExprKey op src in (match tryFindAvailable key exprMap with Some previous -> reuse dest None previous exprMap | None -> retain (addAvailable key dest exprMap) (addAvailable key dest exported))
   | HeapLoad (dest, addr, offset, Some typ) when isCrossBlockCSEType typ -> let key = makeScalarHeapLoadExprKey addr offset typ in (match tryFindAvailable key exprMap with Some previous -> reuse dest (Some typ) previous exprMap | None -> retain (addAvailable key dest exprMap) (addAvailable key dest exported))
   | HeapLoad _ -> retain (clearScalarHeapLoadAvailability exprMap) emptyExprAvailability
   | Call (dest, fn, args, _, typ) when SpecializationIdentity.FunctionSet.mem fn effectFreeFunctions && isCrossBlockCSEType typ -> let key = DirectCallExpr (fn, args, typ) in (match tryFindAvailable key exprMap with Some previous -> reuse dest (Some typ) previous exprMap | None -> retain (addAvailable key dest (clearDirectCallAvailability exprMap)) (addAvailable key dest (clearDirectCallAvailability exported)))
   | Call _ -> retain (clearHeapLoadAndDirectCallAvailability exprMap) emptyExprAvailability
   | RefCountDec _ | RefCountDecString _ | RefCountDecBlob _ | RefCountDecInt _ | RawFree _ | MappedFree _ -> retain emptyExprAvailability emptyExprAvailability
   | Mov (_, _, Some typ) when not (isCrossBlockCSEType typ) -> retain (clearScalarHeapLoadAvailability exprMap) emptyExprAvailability
   | Phi (_, _, Some typ) when not (isCrossBlockCSEType typ) -> retain (clearScalarHeapLoadAvailability exprMap) emptyExprAvailability
   | Mov _ | Phi _ -> retain exprMap exported
   | FloatSqrt _ | FloatAbs _ | FloatNeg _ | Int64ToFloat _ | FloatToInt64 _ | FloatToBits _ -> retain (clearDirectCallAvailability exprMap) emptyExprAvailability
   | _ -> retain (clearHeapLoadAndDirectCallAvailability exprMap) emptyExprAvailability) ([], available, available, false) block.instrs in
  {block with instrs = List.rev instrs}, exported, changed in
 let topology = match existingTopology with Some topology -> topology | None -> T.buildDominatorTopology cfg in
 let children = S.buildDomTree topology.T.immediateDominators in
 let rec optimize available label (blocks, exits, changed) = match LabelMap.find_opt label cfg.blocks with
 | None -> Crash.crash ("MIR CSE: missing dominator-tree block " ^ labelText label)
 | Some block -> let block, available, blockChanged = optimizeBlock available block in
  List.fold_left (fun state child -> optimize available child state) (LabelMap.add label block blocks, LabelMap.add label available exits, changed || blockChanged) (Option.value ~default:[] (LabelMap.find_opt label children)) in
 let reachable = optimize emptyExprAvailability cfg.entry (LabelMap.empty, LabelMap.empty, false) in
 let blocks, exits, changed = LabelMap.fold (fun label block ((blocks, exits, changed) as state) -> if LabelMap.mem label blocks then state else let block, available, blockChanged = optimizeBlock emptyExprAvailability block in LabelMap.add label block blocks, LabelMap.add label available exits, changed || blockChanged) cfg.blocks reachable in
 let optimized, preChanged = applyPartialRedundancyElimination exits {cfg with blocks} in optimized, changed || preChanged, topology
let applyCSEWithEffectFreeCalls effectFreeFunctions cfg = let optimized, changed, _ = applyCSEWithEffectFreeCallsAndTopology None effectFreeFunctions cfg in optimized, changed
let applyCSE cfg = applyCSEWithEffectFreeCalls SpecializationIdentity.FunctionSet.empty cfg
