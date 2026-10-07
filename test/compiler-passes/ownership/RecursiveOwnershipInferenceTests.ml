(* RecursiveOwnershipInferenceTests.ml - Group-wide ownership uniqueness proof laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module R = InferRecursiveOwnership
module Identity = struct type t = string let compare = StringOrder.compare module Set = StringOrder.Set end
module Inference = R.Make (Identity)
module U = Inference.Uniqueness
module V = Inference.Ownership
module S = Identity.Set
type testLeaf = Reuse of string * string
let value id : H.value = {H.id = H.ValueId id; typ = AST.TList AST.TInt64}
let unitValue : H.value = {H.id = H.ValueId 100; typ = AST.TUnit}
let binding name = AST.bindingId (Int32.to_int (List.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (Array.to_list (Text.scalars name))))
let semantics mappings : testLeaf V.semantics =
 let ownershipByValue = H.ValueMap.of_list (List.map (fun ((value : H.value), ownership) -> value.H.id, ownership) mappings) in
 {V.leaf = (fun (Reuse (input, output)) -> {O.inputs = [O.Consumed input]; outputs = [output]});
  leafUniqueness = (fun (Reuse (input, output)) -> {V.requiredInputs = S.singleton input; uniqueOutputs = S.singleton output});
  callOwnership = (fun _ -> None); scalarUses = (fun _ -> S.empty); scalarEscapes = (fun _ -> S.empty);
  blockArgument = (fun (value : H.value) -> match H.ValueMap.find_opt value.H.id ownershipByValue with Some ownership -> O.Managed ownership | None -> O.Unmanaged)}
let signature parameters result : string O.functionSignature = {O.parameters; result}
let block parameters operations result : (testLeaf, string) O.block = {O.body = {H.parameters; operations; result}}
let parameter name value : H.parameter = {H.binding = binding name; value}
let functionDefinition name ownership body : (testLeaf, string) O.functionDef = {O.definition = {H.id = TestIds.functionIdForName name; name; body}; ownership}
let call target arguments result = O.Evaluate (H.Call {H.target = TestIds.functionIdForName target; arguments; result})
let condition : H.operand = {H.expression = CheckedAST.BoolLiteral true; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty}
let boundaryValues boundary = R.boundaryToList boundary |> List.map (fun (boundary : string R.functionBoundary) -> boundary.R.name, boundary.R.ownership)
let infer semantics head tail = Inference.infer semantics {NonEmptyList.head; tail} |> Result.map (fun candidates -> List.map boundaryValues (R.toList candidates))
let inferForDemand semantics target uniqueArguments head tail = Inference.inferDemand semantics (TestIds.functionIdForName target) uniqueArguments {NonEmptyList.head; tail} |> Result.map (Option.map boundaryValues)
let svSignature (signature : string O.functionSignature) =
 let open StructuralValue in let id name value = Union (name, [Text value]) in
 let parameter = function O.UnmanagedParameter -> Union ("UnmanagedParameter", []) | O.BorrowedParameter value -> id "BorrowedParameter" value | O.ConsumedParameter value -> id "ConsumedParameter" value | O.UniqueParameter value -> id "UniqueParameter" value in
 let result = match signature.O.result with O.UnmanagedResult -> Union ("UnmanagedResult", []) | O.BorrowedResult value -> id "BorrowedResult" value | O.ProducedResult value -> id "ProducedResult" value | O.UniqueProducedResult value -> id "UniqueProducedResult" value in
 Record ["Parameters", Sequence (List.map parameter signature.O.parameters); "Result", result]
let svBoundary values = StructuralValue.Sequence (List.map (fun (name, signature) -> StructuralValue.Tuple [StructuralValue.Text name; svSignature signature]) values)
let showGroups groups = StructuralFormat.format (StructuralValue.Sequence (List.map svBoundary groups))
let showOptional = function None -> "None" | Some boundary -> "Some " ^ StructuralFormat.format (svBoundary boundary)
let showError = function
 | U.VariantLimitExceeded (count, maximum) -> Printf.sprintf "VariantLimitExceeded (%d, %d)" count maximum
 | U.RecursiveFunctionRequiresGroupInference id -> "RecursiveFunctionRequiresGroupInference " ^ StructuralFormat.format (AST.DiagnosticFormatting.func id)
 | U.NoVerifiedBoundary error -> "NoVerifiedBoundary (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")"
 | U.NoVerifiedFunctionGroup error -> "NoVerifiedFunctionGroup (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")"
let showResult show = function Ok result -> "Ok " ^ show result | Error error -> "Error (" ^ showError error ^ ")"
let selfRecursive () =
 let input = value 0 in let reused = value 1 in let result = value 2 in
 let semantics = semantics [input, "input"; reused, "reused"; result, "result"] in
 let body = block [parameter "input" input] [O.Evaluate (H.Leaf (Reuse ("input", "reused"))); call "loop" [reused] result] result in
 semantics, functionDefinition "loop" (signature [O.ConsumedParameter "input"] (O.ProducedResult "result")) body
let testInfersSelfRecursiveUniqueness () =
 let semantics, definition = selfRecursive () in
 let expected = [["loop", signature [O.UniqueParameter "input"] (O.UniqueProducedResult "result")]] in
 let actual = infer semantics definition [] in
 if actual = Ok expected then Ok () else Error ("Expected one verified recursive uniqueness boundary " ^ showGroups expected ^ ", got " ^ showResult showGroups actual)
let testInfersSelfRecursiveCallDemand () =
 let semantics, definition = selfRecursive () in
 let expected = Some ["loop", signature [O.UniqueParameter "input"] (O.UniqueProducedResult "result")] in
 let unavailable = inferForDemand semantics "loop" O.IntSet.empty definition [] in
 let available = inferForDemand semantics "loop" (O.IntSet.singleton 0) definition [] in
 if unavailable = Ok None && available = Ok expected then Ok () else Error ("Expected atomic recursive inference only for the usable demand, got unavailable=" ^ showResult showOptional unavailable ^ ", available=" ^ showResult showOptional available)
let testInfersMutualBoundaryTradeoffs () =
 let firstInput = value 10 in let firstRecursive = value 11 in let firstResult = value 12 in
 let secondInput = value 20 in let secondRecursive = value 21 in let secondResult = value 22 in
 let semantics = semantics [firstInput, "firstInput"; firstRecursive, "firstRecursive"; firstResult, "firstResult"; secondInput, "secondInput"; secondRecursive, "secondRecursive"; secondResult, "secondResult"] in
 let firstRecursiveBranch = block [] [call "second" [firstInput] firstRecursive] firstRecursive in
 let firstBaseBranch = block [] [] firstInput in
 let first = functionDefinition "first" (signature [O.ConsumedParameter "firstInput"] (O.ProducedResult "firstResult"))
  (block [parameter "firstInput" firstInput] [O.Evaluate (H.Branch (firstResult, condition, firstRecursiveBranch, firstBaseBranch))] firstResult) in
 let secondRecursiveBranch = block [] [call "first" [secondInput] secondRecursive] secondRecursive in
 let secondBaseBranch = block [] [] secondInput in
 let second = functionDefinition "second" (signature [O.ConsumedParameter "secondInput"] (O.ProducedResult "secondResult"))
  (block [parameter "secondInput" secondInput] [O.Evaluate (H.Branch (secondResult, condition, secondRecursiveBranch, secondBaseBranch))] secondResult) in
 let expected = [
  ["first", signature [O.ConsumedParameter "firstInput"] (O.ProducedResult "firstResult"); "second", signature [O.ConsumedParameter "secondInput"] (O.ProducedResult "secondResult")];
  ["first", signature [O.UniqueParameter "firstInput"] (O.UniqueProducedResult "firstResult"); "second", signature [O.UniqueParameter "secondInput"] (O.UniqueProducedResult "secondResult")]] in
 let actual = infer semantics first [second] in
 if actual = Ok expected then Ok () else Error ("Expected group-wide transfer and uniqueness tradeoffs " ^ showGroups expected ^ ", got " ^ showResult showGroups actual)
let testRejectsInvalidFunctionGroup () =
 let first = functionDefinition "duplicate" (signature [O.UnmanagedParameter] O.UnmanagedResult) (block [parameter "unit" unitValue] [] unitValue) in
 let second = first in
 match infer (semantics []) first [second] with
 | Error (U.NoVerifiedFunctionGroup (V.DuplicateFunctionName id)) when id = TestIds.functionIdForName "duplicate" -> Ok ()
 | actual -> Error ("Expected duplicate function names to reject group inference, got " ^ showResult showGroups actual)
let testBoundsGroupSearch () =
 let managedParameters = List.init 9 (fun index -> value index, "value" ^ string_of_int index) in
 let parameters = List.map (fun (value, ownership) -> parameter ownership value) managedParameters in
 let boundary = signature (List.map (fun (_, id) -> O.ConsumedParameter id) managedParameters) O.UnmanagedResult in
 let definition = functionDefinition "wide" boundary (block parameters [] unitValue) in
 match infer (semantics managedParameters) definition [] with
 | Error (U.VariantLimitExceeded (9, 256)) -> Ok ()
 | actual -> Error ("Expected recursive-group inference to bound its search, got " ^ showResult showGroups actual)
let tests = [
 "Recursive uniqueness inference verifies self calls as one group", testInfersSelfRecursiveUniqueness;
 "Recursive uniqueness inference resolves one concrete call demand", testInfersSelfRecursiveCallDemand;
 "Recursive uniqueness inference retains mutual boundary tradeoffs", testInfersMutualBoundaryTradeoffs;
 "Recursive uniqueness inference rejects invalid function groups", testRejectsInvalidFunctionGroup;
 "Recursive uniqueness inference bounds group-wide variants", testBoundsGroupSearch
]
