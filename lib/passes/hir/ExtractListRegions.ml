(* ExtractListRegions.fs - Recognize closed list computations and prove scalar scope eligibility. *)
[@@@warning "-4"]
module C = CheckedAST
module H = HIR
module L = ListRegion
module D = Destruction
module B = C.BindingIdMap
module S = H.ValueSet
module F = SpecializationIdentity.FunctionSet
module Bindings = ClosureAnalysis.BindingSet
let ( let* ) = Option.bind
let increment value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
type scalarLifetime = EnclosingLifetime | JoinEntryLifetime
type extractionName = SourceBinding of AST.bindingId | RegionResult
module Names = Map.Make (struct type t = extractionName let compare left right = match left, right with SourceBinding a, SourceBinding b -> AST.compareBindingId a b | SourceBinding _, RegionResult -> -1 | RegionResult, SourceBinding _ -> 1 | RegionResult, RegionResult -> 0 end)
type extraction = {lists : H.value Names.t; values : H.value Names.t; operations : ((L.transform * L.reuseSelection) L.operation, L.functionalBlock) H.operation list; runtimeInputs : S.t; nextId : int; lifetime : scalarLifetime}
let listCall = function C.Call (name, args) -> Some (name, NonEmptyList.toList args) | _ -> None
(*
   Prove scope destruction separately from evaluation effects. Calls use an
   explicit contract; nominal payloads and unknown closures remain unproven.
   Reject unsupported syntax before inference: declaration overlays
   need not contain the pattern/layout metadata of their base context.
*)
let inertExpression infer callIsInert =
 let inertType = D.hasInertDestruction in
 let rec check types expr =
  let typedInert () = match infer types expr with Ok typ -> inertType typ | Error _ -> false in
  let recur = check types in match expr with
  | C.FuncRef _ -> true
  | C.Closure (_, captures) -> List.for_all recur captures
  | C.Let (C.LPVariable name, value, body) -> if not (recur value) then false else (match infer types value with Ok typ when inertType typ -> check (B.add name typ types) body | _ -> false)
  | C.Let ((C.LPUnit | C.LPWildcard), value, body) | C.Sequence (value, body) -> recur value && recur body
  | C.If (condition, yes, no) -> recur condition && recur yes && recur no
  | C.BinOp (_, left, right) -> recur left && recur right
  | C.UnaryOp (_, value) | C.TupleAccess (value, _) -> recur value
  | C.Call (name, args) -> callIsInert name && List.for_all recur (NonEmptyList.toList args) && typedInert ()
  | C.TupleLiteral values -> List.for_all recur (C.tupleElementsToList values)
  | C.ListLiteral values -> List.for_all recur values
  | C.Local _ -> typedInert ()
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _ | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _ | C.UInt128Literal _ | C.BigIntLiteral _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _ | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ -> true
  | _ -> false in check
(*
   Retain dependencies with each local proof so registry composition can revoke
   transitive proofs when a definition changes. Recursive components need no
   unrolling: the consumer rejects the backwards closure of unproven callees.
*)
let scopeContracts infer functions =
 let rec calls expr =
  let many values = List.map calls values |> List.fold_left F.union F.empty in match expr with
  | C.Call (name, args) -> F.add name (many (NonEmptyList.toList args))
  | C.TupleLiteral values -> many (C.tupleElementsToList values)
  | C.Closure (_, values) | C.ListLiteral values -> many values
  | C.Let (_, value, body) | C.Sequence (value, body) | C.BinOp (_, value, body) -> many [value;body]
  | C.If (condition, yes, no) -> many [condition;yes;no]
  | C.UnaryOp (_, value) | C.TupleAccess (value, _) -> calls value
  | _ -> F.empty in
 List.map (fun (func : C.functionDef) -> let parameters = NonEmptyList.toList (C.functionParameterTypes func) in let types = B.of_seq (List.to_seq parameters) in
  let localInert = D.hasInertDestruction (C.functionReturnType func) && List.for_all (fun (_, typ) -> D.hasInertDestruction typ) parameters && inertExpression infer (fun _ -> true) types func.C.body in
  func.C.id, {D.localDestruction = (if localInert then D.InertScope else D.UnprovenScope); calls = calls func.C.body}) functions |> FunctionIdMap.ofList
(*
   A failed recognition is semantic absence, not a compiler failure. The
   original checked expression then uses the supported persistent List path.
   A closure may not hide a region alias or an effectful destructor.
   Static code addresses, including closure comparators.
*)
let tryExtract inertScopes functionNames parameterTypes infer freeVariables expression =
 let named id name = FunctionIdMap.tryFind id functionNames = Some name in let inert = inertExpression infer (fun name -> F.mem name inertScopes) in
 let types state = Names.bindings state.values |> List.filter_map (fun (name, (value : H.value)) -> match name with SourceBinding id -> Some (id, value.H.typ) | RegionResult -> None) |> List.to_seq |> B.of_seq in
 let normalizedOperand state expr typ : H.operand = let inputs = Bindings.elements (freeVariables expr) |> List.filter_map (fun name -> Option.map (fun value -> name, value) (Names.find_opt (SourceBinding name) state.values)) |> List.to_seq |> B.of_seq in {H.expression = expr; typ; inputs} in
 let operand state accepts expr =
  let referencesList = Bindings.exists (fun name -> Names.mem (SourceBinding name) state.lists) (freeVariables expr) in
  let destructionInert = match state.lifetime with EnclosingLifetime -> true | JoinEntryLifetime -> inert (types state) expr in
  if referencesList || not destructionInert then None else match infer (types state) expr with Ok typ when accepts typ -> Some (normalizedOperand state expr typ) | _ -> None in
 let scalar state expr = operand state L.immediate expr in
 let callback state expected expr =
  let scopeInert = match state.lifetime, expr with EnclosingLifetime, _ -> true | JoinEntryLifetime, C.FuncRef name | JoinEntryLifetime, C.Closure (name, _) -> F.mem name inertScopes | JoinEntryLifetime, _ -> false in
  let immediateCaptures = match expr with C.Closure (_, captures) -> List.for_all (function C.FuncRef _ -> true | capture -> Option.is_some (scalar state capture)) captures | C.FuncRef _ -> true | _ -> false in
  if not immediateCaptures || not scopeInert then None else match infer (types state) expr with Ok typ when typ = expected -> Some (normalizedOperand state expr typ) | _ -> None in
 let fresh typ state : H.value * extraction = {H.id = H.ValueId state.nextId; typ}, {state with nextId = increment state.nextId} in
 let addList state operation = let value, next = fresh (AST.TList AST.TInt64) state in value, {next with operations = operation value :: state.operations} in
 let rec list state expr = match expr with
 | C.Local name -> Option.map (fun value -> value, state) (Names.find_opt (SourceBinding name) state.lists)
 | C.ListLiteral elements when List.length elements <= L.maxCapacity -> let values = List.map (fun value -> Option.bind (scalar state value) (fun (value : H.operand) -> if value.H.typ = AST.TInt64 then Some value else None)) elements in if List.for_all Option.is_some values then Some (addList state (fun output -> H.Leaf (L.Construct (output, L.Literal (List.filter_map Fun.id values))))) else None
 | _ -> (match listCall expr with
   | Some (id, [input]) when named id "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64" -> let* source, next = list state input in Some (source, {next with runtimeInputs = S.add source.H.id next.runtimeInputs})
   | Some (id, [count;value]) when named id "Darklang.Stdlib.List.repeatUnsafe_i64" -> let count = operand state ((=) AST.TInt) count in let value = operand state ((=) AST.TInt64) value in (match count, value with Some count, Some value -> Some (addList state (fun output -> H.Leaf (L.Construct (output, L.Repeat (count, value))))) | _ -> None)
   | Some (id, [input;fn]) when named id "Darklang.Stdlib.List.map_i64_i64" -> let* source, next = list state input in let* fn = callback state (AST.TFunction ([AST.TInt64], AST.TInt64)) fn in let reuse = if S.mem source.H.id next.runtimeInputs then L.RuntimeReuse else L.StaticReuse in let next = {next with runtimeInputs = S.remove source.H.id next.runtimeInputs} in Some (addList next (fun id -> H.Leaf (L.Transform (id, source, (L.Map fn, reuse)))))
   | Some (id, [input]) when named id "Darklang.Stdlib.List.reverse_i64" -> let* source, next = list state input in let reuse = if S.mem source.H.id next.runtimeInputs then L.RuntimeReuse else L.StaticReuse in let next = {next with runtimeInputs = S.remove source.H.id next.runtimeInputs} in Some (addList next (fun id -> H.Leaf (L.Transform (id, source, (L.Reverse, reuse)))))
   | _ -> None) in
 let rec bindScalar state name expr = match expr with
 | C.If (condition, yes, no) -> let* condition = operand state ((=) AST.TBool) condition in
   let* (L.FunctionalBlock yes as yesBlock), afterYes = region name {state with operations = []; lifetime = JoinEntryLifetime} yes in
   let* (L.FunctionalBlock no as noBlock), afterNo = region name {state with operations = []; nextId = afterYes; lifetime = JoinEntryLifetime} no in
   if yes.H.result.H.typ <> no.H.result.H.typ then None else let result, next = fresh yes.H.result.H.typ {state with nextId = afterNo} in
   Some {next with values = Names.add name result state.values; lists = Names.remove name state.lists; operations = H.Branch (result, condition, yesBlock, noBlock) :: state.operations}
 | _ -> bindSimpleScalar state name expr
 and bindSimpleScalar state name expr = match listCall expr with
 | Some (id, [input;initial;fn]) when named id "Darklang.Stdlib.List.fold_i64_i64" -> let* source, next = list state input in
   let initial = scalar state initial in let fn = callback state (AST.TFunction ([AST.TInt64;AST.TInt64], AST.TInt64)) fn in
   (match initial, fn with Some initial, Some fn when initial.H.typ = AST.TInt64 -> let result, after = fresh AST.TInt64 next in Some {after with values = Names.add name result next.values; lists = Names.remove name next.lists; operations = H.Leaf (L.Fold (result, source, initial, fn)) :: next.operations} | _ -> None)
 | _ -> let* value = scalar state expr in let result, next = fresh value.H.typ state in Some {next with values = Names.add name result state.values; lists = Names.remove name state.lists; operations = H.ScalarBinding (result, value) :: state.operations}
 and region finalName state expr = match expr with
 | C.Let (C.LPVariable name, value, body) -> let name = SourceBinding name in (match list state value with Some (id, next) -> region finalName {next with lists = Names.add name id next.lists; values = Names.add name id next.values} body | None -> let* next = bindScalar state name value in region finalName next body)
 | _ -> let* next = bindScalar state finalName expr in let* result = Names.find_opt finalName next.values in Some (L.FunctionalBlock {H.parameters = []; operations = List.rev next.operations; result}, next.nextId) in
 let isListOperation value = match listCall value with Some (id, _) when named id "Darklang.Stdlib.List.map_i64_i64" || named id "Darklang.Stdlib.List.reverse_i64" || named id "Darklang.Stdlib.List.repeatUnsafe_i64" || named id "Darklang.Stdlib.List.fold_i64_i64" -> true | _ -> false in
 let candidate = match expression with C.Let (_, C.ListLiteral _, _) -> true | C.Let (_, value, _) -> isListOperation value | _ -> isListOperation expression in
 if not candidate then None else
 let parameterNames = Bindings.inter (freeVariables expression) (Bindings.of_list (List.map fst (B.bindings parameterTypes))) in
 let parameters, nextId = List.fold_left (fun (parameters, next) name -> let typ = B.find name parameterTypes in B.add name {H.id = H.ValueId next; typ} parameters, increment next) (B.empty, 0) (Bindings.elements parameterNames) in
 let extractionParameters = B.bindings parameters |> List.map (fun (name, value) -> SourceBinding name, value) |> List.to_seq |> Names.of_seq in
 let* L.FunctionalBlock block, _ = region RegionResult {lists = Names.empty; values = extractionParameters; operations = []; runtimeInputs = S.empty; nextId; lifetime = EnclosingLifetime} expression in
 let rec containsListOperation (L.FunctionalBlock block) = List.exists (function H.Leaf _ -> true | H.Branch (_, _, yes, no) -> containsListOperation yes || containsListOperation no | H.Call _ | H.ScalarBinding _ -> false) block.H.operations in
 let blockParameters = B.bindings parameters |> List.map (fun (binding, value) -> {H.binding; value}) in let root = L.FunctionalBlock {block with H.parameters = blockParameters} in
 if not (containsListOperation root) then None else Some (L.FunctionalRegion root)
