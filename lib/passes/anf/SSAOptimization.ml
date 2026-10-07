(* SSAOptimization.fs - Simplify typed high-level SSA before specialization. *)
[@@@warning "-4"]
module A = ANF
module S = SSAANF
module C = ANFConstants
module E = ANFEffects
module Sub = ANFSubstitution
module X = ANFExpressionOptimization
module M = C.TempMap
module Set = E.TempSet
module Labels = Stdlib.Set.Make (struct type t = S.label let compare (S.Label left) (S.Label right) = Int.compare left right end)
let addInt left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let successors = function S.Return _ -> [] | S.Jump (target, _) -> [target] | S.Branch (_, yes, no) -> [yes; no]
let termUses = function S.Return atom -> E.addAtomUse atom Set.empty | S.Jump (_, args) -> List.fold_left (fun uses atom -> E.addAtomUse atom uses) Set.empty args | S.Branch (condition, _, _) -> E.addAtomUse condition Set.empty
let reachableBlocks (func : S.functionDef) =
 let rec visit seen label = if Labels.mem label seen then seen else
  let seen = Labels.add label seen in
  match S.LabelMap.find_opt label func.S.blocks with None -> Crash.crash "SSA optimization: missing successor block" | Some block -> List.fold_left visit seen (successors block.S.terminator) in visit Labels.empty func.S.entry
let useCounts (func : S.functionDef) =
 let add uses ids = Set.fold (fun id uses -> M.add id (addInt 1 (Option.value ~default:0 (M.find_opt id uses))) uses) ids uses in
 S.LabelMap.fold (fun _ block uses -> List.fold_left (fun uses (_, operation) -> add uses (E.cexprTempUses operation)) (add uses (termUses block.S.terminator)) block.S.operations) func.S.blocks M.empty
let rewriteTerminator (options : C.optimizeOptions) env = function
 | S.Return atom -> S.Return (Sub.substAtom env atom)
 | S.Jump (target, args) -> S.Jump (target, List.map (Sub.substAtom env) args)
 | S.Branch (condition, yes, no) -> match Sub.substAtom env condition with A.BoolLiteral true when options.C.enableConstFolding -> S.Jump (yes, []) | A.BoolLiteral false when options.C.enableConstFolding -> S.Jump (no, []) | _ when yes = no -> S.Jump (yes, []) | condition -> S.Branch (condition, yes, no)
let rewriteOperations context (options : C.optimizeOptions) typeEnv tupleEnv env operations =
 let rewritten, known, tuples, _ = List.fold_left (fun (rewritten, known, tuples, cse) (id, operation) ->
  let operation, _ = Sub.optimizeCExpr context options known typeEnv tuples operation in
  let operation, cse = if options.C.enableCSE then match X.tryCSEKey operation with Some key -> (match X.CSEnv.find_opt key cse with Some existing -> A.Atom (A.Var existing), cse | None -> operation, X.CSEnv.add key id cse) | None -> operation, cse else operation, cse in
  let known = match operation with A.Atom atom when options.C.enableCopyProp && not (E.mustPreserveEvaluation context operation) -> M.add id atom known | _ -> known in
  let tuples = match operation with A.TupleAlloc elems -> let fields = C.IntMap.of_list (List.filter_map (fun (index, atom) -> if E.canForwardTupleElement context typeEnv atom then Some (index, atom) else None) (List.mapi (fun index atom -> index, atom) elems)) in M.add id fields tuples | _ -> tuples in
  (id, operation) :: rewritten, known, tuples, cse) ([], env, tupleEnv, X.CSEnv.empty) operations in List.rev rewritten, known, tuples
let rewriteIndexCalls (context : C.optimizeContext) uses operations =
 let hasName id name = FunctionIdMap.tryFind id context.C.functionNames = Some name in
 let resolve name = StringOrder.Map.find_opt name context.C.functionIds in
 let getAtName id = Option.fold ~none:false ~some:(fun name -> name = "Darklang.Stdlib.List.getAt" || String.starts_with ~prefix:"Darklang.Stdlib.List.getAt_" name) (FunctionIdMap.tryFind id context.C.functionNames) in
 let rec rewrite = function
  | (indexId, A.Call (from, [native])) :: (resultId, A.Call (getAt, [value; A.Var used])) :: rest when indexId = used && M.find_opt indexId uses = Some 1 && hasName from "Darklang.Stdlib.Int.fromInt64" && getAtName getAt ->
    let target = Option.bind (FunctionIdMap.tryFind getAt context.C.functionNames) (fun name -> let prefix = "Darklang.Stdlib.List.getAt" in if name = prefix || String.starts_with ~prefix:(prefix ^ "_") name then resolve ("Darklang.Stdlib.List.__getAt" ^ String.sub name (String.length prefix) (String.length name - String.length prefix)) else None) in
    (match target with Some target -> (resultId, A.Call (target, [value; native])) :: rewrite rest | None -> (indexId, A.Call (from, [native])) :: rewrite ((resultId, A.Call (getAt, [value; A.Var used])) :: rest))
  | (indexId, A.Call (from, [native])) :: (resultId, A.Call (getAt, [value; A.Var used])) :: rest when indexId = used && M.find_opt indexId uses = Some 1 && hasName from "Darklang.Stdlib.Int.fromInt64" && hasName getAt "Darklang.Stdlib.String.getByteAt" ->
    (match resolve "Darklang.Stdlib.String.__getByteAtInt64" with Some target -> (resultId, A.Call (target, [value; native])) :: rewrite rest | None -> (indexId, A.Call (from, [native])) :: rewrite ((resultId, A.Call (getAt, [value; A.Var used])) :: rest))
  | head :: tail -> head :: rewrite tail | [] -> [] in rewrite operations
let predecessorCounts (func : S.functionDef) = S.LabelMap.fold (fun _ block counts -> List.fold_left (fun counts target -> S.LabelMap.add target (addInt 1 (Option.value ~default:0 (S.LabelMap.find_opt target counts))) counts) counts (successors block.S.terminator)) func.S.blocks S.LabelMap.empty
let nextValue (func : S.functionDef) = addInt 1 (M.fold (fun (A.TempId id) _ maxId -> max id maxId) func.S.freshValueTypes 4000)
let rewriteByteOptionMatches (context : C.optimizeContext) (func : S.functionDef) =
 let resolve name = StringOrder.Map.find_opt name context.C.functionIds in
 match resolve "Darklang.Stdlib.String.__getByteAtInt64", resolve "Darklang.Stdlib.String.__byteLength", resolve "Darklang.Stdlib.String.__byteAtUnchecked" with
 | Some checkedByte, Some byteLength, Some unchecked when S.LabelMap.exists (fun _ block -> List.exists (function _, A.Call (target, _) when target = checkedByte -> true | _ -> false) block.S.operations) func.S.blocks ->
   let predecessors = predecessorCounts func in let uses = useCounts func in
   let rewrite label (block : S.block) ((current : S.functionDef), next) =
    let candidate = match List.rev block.S.operations, block.S.terminator with
     | (condition, A.Prim (A.Neq, A.Var option, A.IntLiteral (A.Int64 256L))) :: (call, A.Call (target, [value; index])) :: prefix, S.Branch (A.Var branch, some, _) when condition = branch && option = call && target = checkedByte -> Some (condition, call, value, index, some, List.rev prefix, false)
     | (condition, A.Prim (A.Eq, A.Var option, A.IntLiteral (A.Int64 256L))) :: (call, A.Call (target, [value; index])) :: prefix, S.Branch (A.Var branch, _, some) when condition = branch && option = call && target = checkedByte -> Some (condition, call, value, index, some, List.rev prefix, true)
     | _ -> None in
    match candidate with None -> current, next | Some (condition, call, value, index, some, prefix, inverted) ->
    let someBlock = match S.LabelMap.find_opt some current.S.blocks with Some block -> block | None -> Crash.crash "SSA optimization: byte match successor is missing" in
    let payload = match someBlock.S.operations with (payload, A.TypedAtom (A.Var source, AST.TUInt8)) :: rest when source = call -> Some (payload, rest) | _ -> None in
    let expectedUses = if Option.is_some payload then 2 else 1 in
    if Option.value ~default:0 (M.find_opt call uses) <> expectedUses || (Option.is_some payload && S.LabelMap.find_opt some predecessors <> Some 1) then current, next else
    let length = A.TempId next and nonnegative = A.TempId (addInt next 1) and less = A.TempId (addInt next 2) in
    let operations = prefix @ [length, A.Call (byteLength, [value]); nonnegative, A.Prim (A.Gte, index, A.IntLiteral (A.Int64 0L)); less, A.Prim (A.Lt, index, A.Var length); condition, A.Prim (A.And, A.Var nonnegative, A.Var less)] in
    let someBlock = match payload with None -> someBlock | Some (payload, rest) -> {someBlock with S.operations = (payload, A.Call (unchecked, [value; index])) :: rest} in
    let terminator = if inverted then match block.S.terminator with S.Branch (condition, yes, no) -> S.Branch (condition, no, yes) | _ -> Crash.crash "SSA optimization: expected byte branch" else block.S.terminator in
    let blocks = S.LabelMap.add some someBlock (S.LabelMap.add label {block with S.operations; terminator} current.S.blocks) in
    let types = M.add less AST.TBool (M.add nonnegative AST.TBool (M.add length AST.TInt64 current.S.freshValueTypes)) in {current with S.blocks; freshValueTypes = types}, addInt next 3 in
   fst (S.LabelMap.fold rewrite func.S.blocks (func, nextValue func))
 | _ -> func
let eliminateDominatedDuplicatesWithCandidates candidates (func : S.functionDef) =
 let labels = Labels.of_list (List.map fst (S.LabelMap.bindings func.S.blocks)) in
 let predecessors = S.LabelMap.fold (fun source block preds -> List.fold_left (fun preds target -> S.LabelMap.add target (Labels.add source (Option.value ~default:Labels.empty (S.LabelMap.find_opt target preds))) preds) preds (successors block.S.terminator)) func.S.blocks S.LabelMap.empty in
 let incoming = Labels.fold (fun label incoming -> S.LabelMap.add label (Labels.elements (Option.value ~default:Labels.empty (S.LabelMap.find_opt label predecessors))) incoming) labels S.LabelMap.empty in
 let successorIndex = S.LabelMap.map (fun block -> successors block.S.terminator) func.S.blocks in
 let initial = Labels.fold (fun label dom -> S.LabelMap.add label (if label = func.S.entry then Labels.singleton label else labels) dom) labels S.LabelMap.empty in
 let rec settle known pending = match Labels.min_elt_opt pending with
  | None -> known
  | Some label ->
    let pending = Labels.remove label pending in
    let next = if label = func.S.entry then Labels.singleton label else
     let sources = match S.LabelMap.find_opt label incoming with Some sources -> List.filter_map (fun source -> S.LabelMap.find_opt source known) sources | None -> Crash.crash "SSA dominator predecessor index lost a block" in
     let common = match sources with [] -> Labels.empty | first :: rest -> List.fold_left Labels.inter first rest in Labels.add label common in
    match S.LabelMap.find_opt label known with Some previous when Labels.equal previous next -> settle known pending | Some _ -> let successors = match S.LabelMap.find_opt label successorIndex with Some successors -> successors | None -> Crash.crash "SSA dominator successor index lost a block" in settle (S.LabelMap.add label next known) (List.fold_left (fun pending target -> Labels.add target pending) pending successors) | None -> Crash.crash "SSA dominator state lost a block" in
 let dominators = settle initial labels in
 {func with S.blocks = S.LabelMap.mapi (fun label block ->
  let dominates = Option.value ~default:Labels.empty (S.LabelMap.find_opt label dominators) in
  {block with S.operations = List.mapi (fun index (id, operation) -> match X.tryCSEKey operation with None -> id, operation | Some key ->
   let prior = List.find_map (fun (sourceLabel, sourceIndex, source) -> if source <> id && Labels.mem sourceLabel dominates && (sourceLabel <> label || sourceIndex < index) then Some source else None) (Option.value ~default:[] (X.CSEnv.find_opt key candidates)) in
   match prior with Some source -> id, A.Atom (A.Var source) | None -> id, operation) block.S.operations}) func.S.blocks}
let eliminateDominatedDuplicates (func : S.functionDef) =
 let candidates = S.LabelMap.fold (fun label block candidates -> List.fold_left (fun candidates (index, (id, operation)) -> match X.tryCSEKey operation with None -> candidates | Some key -> X.CSEnv.add key (Option.value ~default:[] (X.CSEnv.find_opt key candidates) @ [label, index, id]) candidates) candidates (List.mapi (fun index value -> index, value) block.S.operations)) func.S.blocks X.CSEnv.empty in
 if X.CSEnv.for_all (fun _ values -> List.length values < 2) candidates then func else eliminateDominatedDuplicatesWithCandidates candidates func
let mergeSinglePredecessorJumps (func : S.functionDef) =
 let counts = predecessorCounts func in
 let rec merge (current : S.functionDef) = function
  | [] -> current
  | (label, _) :: rest when not (S.LabelMap.mem label current.S.blocks) -> merge current rest
  | (label, block) :: rest -> match block.S.terminator with
    | S.Jump (target, []) when label <> target && target <> func.S.entry && S.LabelMap.find_opt target counts = Some 1 -> (match S.LabelMap.find_opt target current.S.blocks with Some successor when successor.S.parameters = [] -> let combined = {block with S.operations = block.S.operations @ successor.S.operations; terminator = successor.S.terminator} in merge {current with S.blocks = S.LabelMap.add label combined (S.LabelMap.remove target current.S.blocks)} rest | _ -> merge current rest)
    | _ -> merge current rest in merge func (S.LabelMap.bindings func.S.blocks)
let simplifyBooleanReturnBranches (func : S.functionDef) =
 let literal target = match S.LabelMap.find_opt target func.S.blocks with Some block when block.S.parameters = [] && block.S.operations = [] -> (match block.S.terminator with S.Return (A.BoolLiteral value) -> Some value | _ -> None) | _ -> None in
 let blocks, types, _ = S.LabelMap.fold (fun label block (blocks, types, next) -> match block.S.terminator with
  | S.Branch (condition, yes, no) when yes <> no -> (match literal yes, literal no with
    | Some true, Some false -> S.LabelMap.add label {block with S.terminator = S.Return condition} blocks, types, next
    | Some false, Some true -> let id = A.TempId next in S.LabelMap.add label {block with S.operations = block.S.operations @ [id, A.UnaryPrim (A.Not, condition)]; terminator = S.Return (A.Var id)} blocks, M.add id AST.TBool types, addInt next 1
    | _ -> blocks, types, next)
  | _ -> blocks, types, next) func.S.blocks (func.S.blocks, func.S.freshValueTypes, nextValue func) in {func with S.blocks; freshValueTypes = types}
let devirtualizeCaptureFreeClosures (func : S.functionDef) =
 let allocations = S.LabelMap.bindings func.S.blocks |> List.concat_map (fun (_, block) -> List.filter_map (function id, A.ClosureAlloc (target, []) -> Some (id, target) | _ -> None) block.S.operations) in
 if allocations = [] then func else
 let counts = useCounts func in
 let calls = S.LabelMap.fold (fun _ block calls -> List.fold_left (fun calls (_, operation) -> match operation with A.ClosureCall (A.Var id, args) | A.ClosureTailCall (A.Var id, args) when not (List.exists (E.atomUsesTemp id) args) -> M.add id (addInt 1 (Option.value ~default:0 (M.find_opt id calls))) calls | _ -> calls) calls block.S.operations) func.S.blocks M.empty in
 let candidates = M.of_list (List.filter_map (fun (id, target) -> let calls = Option.value ~default:0 (M.find_opt id calls) in if calls > 0 && M.find_opt id counts = Some calls then Some (id, target) else None) allocations) in
 if M.is_empty candidates then func else
 {func with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = List.filter_map (fun (id, operation) -> match operation with
  | A.ClosureAlloc (_, []) when M.mem id candidates -> None
  | A.ClosureCall (A.Var source, args) -> Some (id, match M.find_opt source candidates with Some target -> A.Call (target, A.UnitLiteral :: args) | None -> operation)
  | A.ClosureTailCall (A.Var source, args) -> Some (id, match M.find_opt source candidates with Some target -> A.TailCall (target, A.UnitLiteral :: args) | None -> operation)
  | _ -> Some (id, operation)) block.S.operations}) func.S.blocks}
(*
   SSA identities are unique. A known scalar definition may be substituted
   globally even when it lies inside a branch: every valid use is dominated
   by that definition. Repeating the pass resolves definitions in any block
   label order without changing the fixed-point iteration limit.
*)
let rewriteOnce context (options : C.optimizeOptions) (func : S.functionDef) =
 let types = List.fold_left (fun types (param : A.typedParam) -> M.add param.A.id param.A.typ types) func.S.freshValueTypes func.S.typedParams in
 let known = S.LabelMap.fold (fun _ block env -> let _, env, _ = rewriteOperations context options types M.empty env block.S.operations in env) func.S.blocks M.empty in
 let blocks = S.LabelMap.map (fun block -> let operations, _, _ = rewriteOperations context options types M.empty known block.S.operations in {block with S.operations; terminator = rewriteTerminator options known block.S.terminator}) func.S.blocks in
 let rewritten = {func with S.blocks} in
 let rewritten = if options.C.enableConstFolding then simplifyBooleanReturnBranches rewritten else rewritten in
 let reachable = reachableBlocks rewritten in
 let rewritten = {rewritten with S.blocks = S.LabelMap.filter (fun label _ -> Labels.mem label reachable) rewritten.S.blocks} in
 let rewritten = if options.C.enableCSE then eliminateDominatedDuplicates rewritten else rewritten in
 let uses = useCounts rewritten in
 let rewritten = {rewritten with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = rewriteIndexCalls context uses block.S.operations}) rewritten.S.blocks} in
 let rewritten = rewriteByteOptionMatches context rewritten in
 let rewritten = mergeSinglePredecessorJumps rewritten in
 if not options.C.enableDCE then rewritten else
 let uses = useCounts rewritten in {rewritten with S.blocks = S.LabelMap.map (fun block -> {block with S.operations = List.filter (fun (id, operation) -> Option.value ~default:0 (M.find_opt id uses) > 0 || E.mustPreserveEvaluation context operation) block.S.operations}) rewritten.S.blocks}
let equalFunction (left : S.functionDef) (right : S.functionDef) = left.S.id = right.S.id && left.S.name = right.S.name && left.S.typedParams = right.S.typedParams && left.S.returnType = right.S.returnType && left.S.returnOwnership = right.S.returnOwnership && left.S.entry = right.S.entry && S.LabelMap.equal (=) left.S.blocks right.S.blocks && M.equal (=) left.S.freshValueTypes right.S.freshValueTypes
let optimizeFunction context options func =
 let compact (func : S.functionDef) =
  let labels = S.LabelMap.bindings func.S.blocks |> List.mapi (fun index (old, _) -> old, S.Label index) |> List.to_seq |> S.LabelMap.of_seq in
  let mapped label = match S.LabelMap.find_opt label labels with Some value -> value | None -> Crash.crash "SSA optimization: compacted edge target is missing" in
  let blocks = S.LabelMap.bindings func.S.blocks |> List.map (fun (old, block) -> let label = mapped old in let terminator = match block.S.terminator with S.Return atom -> S.Return atom | S.Jump (target, args) -> S.Jump (mapped target, args) | S.Branch (condition, yes, no) -> S.Branch (condition, mapped yes, mapped no) in label, {block with S.label; terminator}) |> List.to_seq |> S.LabelMap.of_seq in
  {func with S.entry = mapped func.S.entry; blocks} in
 let rec iterate remaining current = if remaining <= 0 then current else let next = rewriteOnce context options current in if equalFunction next current then current else iterate (remaining - 1) next in compact (devirtualizeCaptureFreeClosures (iterate 10 func))
