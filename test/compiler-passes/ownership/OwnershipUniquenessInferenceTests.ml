(* OwnershipUniquenessInferenceTests.fs - Proven function-boundary refinement laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module Identity = struct type t = string let compare = StringOrder.compare module Set = StringOrder.Set end
module U = InferOwnershipUniqueness.Make (Identity)
module V = U.Ownership
module S = Identity.Set
type testLeaf = Reuse of string * string
let binding name = AST.bindingId (Int32.to_int (List.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (Array.to_list (Text.scalars name))))
let inputValue : H.value = {H.id = H.ValueId 0; typ = AST.TList AST.TInt64}
let outputValue : H.value = {H.id = H.ValueId 1; typ = AST.TList AST.TInt64}
let unitValue : H.value = {H.id = H.ValueId 100; typ = AST.TUnit}
let ownershipIds mappings = H.ValueMap.of_list (List.map (fun ((value : H.value), ownership) -> value.H.id, ownership) mappings)
let semantics mappings : testLeaf V.semantics =
 let ownershipByValue = ownershipIds mappings in
 let scalarIds (operand : H.operand) = CheckedAST.BindingIdMap.bindings operand.H.inputs |> List.filter_map (fun (_, (value : H.value)) -> H.ValueMap.find_opt value.H.id ownershipByValue) |> S.of_list in
 {V.leaf = (fun (Reuse (input, output)) -> {O.inputs = [O.Consumed input]; outputs = [output]});
  leafUniqueness = (fun (Reuse (input, output)) -> {V.requiredInputs = S.singleton input; uniqueOutputs = S.singleton output});
  callOwnership = (fun _ -> None); scalarUses = scalarIds; scalarEscapes = scalarIds;
  blockArgument = (fun (value : H.value) -> match H.ValueMap.find_opt value.H.id ownershipByValue with Some ownership -> O.Managed ownership | None -> O.Unmanaged)}
let parameter name value : H.parameter = {H.binding = binding name; value}
let block parameters operations result : (testLeaf, string) O.block = {O.body = {H.parameters; operations; result}}
let functionDefinition signature body : (testLeaf, string) O.functionDef = {O.definition = {H.id = TestIds.functionIdForName "test"; name = "test"; body}; ownership = signature}
let infer semantics signature body = U.infer semantics (functionDefinition signature body) |> Result.map U.toList
let inferForDemand semantics uniqueArguments signature body = U.inferDemand semantics uniqueArguments (functionDefinition signature body)
let signature parameters result : string O.functionSignature = {O.parameters; result}
let svSignature (signature : string O.functionSignature) =
 let open StructuralValue in
 let id name value = Union (name, [Text value]) in
 let parameter = function O.UnmanagedParameter -> Union ("UnmanagedParameter", []) | O.BorrowedParameter value -> id "BorrowedParameter" value | O.ConsumedParameter value -> id "ConsumedParameter" value | O.UniqueParameter value -> id "UniqueParameter" value in
 let result = match signature.O.result with O.UnmanagedResult -> Union ("UnmanagedResult", []) | O.BorrowedResult value -> id "BorrowedResult" value | O.ProducedResult value -> id "ProducedResult" value | O.UniqueProducedResult value -> id "UniqueProducedResult" value in
 Record ["Parameters", Sequence (List.map parameter signature.O.parameters); "Result", result]
let showSignatures values = StructuralFormat.format (StructuralValue.Sequence (List.map svSignature values))
let showError = function
 | U.VariantLimitExceeded (count, maximum) -> Printf.sprintf "VariantLimitExceeded (%d, %d)" count maximum
 | U.RecursiveFunctionRequiresGroupInference id -> "RecursiveFunctionRequiresGroupInference " ^ StructuralFormat.format (AST.DiagnosticFormatting.func id)
 | U.NoVerifiedBoundary error -> "NoVerifiedBoundary (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")"
 | U.NoVerifiedFunctionGroup error -> "NoVerifiedFunctionGroup (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")"
let showResult show = function Ok result -> "Ok " ^ show result | Error error -> "Error (" ^ showError error ^ ")"
let showOptional = function None -> "None" | Some value -> "Some " ^ StructuralFormat.format (svSignature value)
let testRequiresAndReturnsUniqueReuse () =
 let semantics = semantics [inputValue, "input"; outputValue, "output"] in
 let body = block [parameter "input" inputValue] [O.Evaluate (H.Leaf (Reuse ("input", "output")))] outputValue in
 let initial = signature [O.ConsumedParameter "input"] (O.ProducedResult "output") in
 let expected = [signature [O.UniqueParameter "input"] (O.UniqueProducedResult "output")] in
 let actual = infer semantics initial body in
 if actual = Ok expected then Ok () else Error ("Expected the verified unique reuse boundary " ^ showSignatures expected ^ ", got " ^ showResult showSignatures actual)
let testInfersOnlyConcreteCallDemand () =
 let semantics = semantics [inputValue, "input"; outputValue, "output"] in
 let body = block [parameter "input" inputValue] [O.Evaluate (H.Leaf (Reuse ("input", "output")))] outputValue in
 let initial = signature [O.ConsumedParameter "input"] (O.ProducedResult "output") in
 let expected = signature [O.UniqueParameter "input"] (O.UniqueProducedResult "output") in
 let unavailable = inferForDemand semantics O.IntSet.empty initial body in let available = inferForDemand semantics (O.IntSet.singleton 0) initial body in
 if unavailable = Ok None && available = Ok (Some expected) then Ok () else Error ("Expected inference only for the usable call demand, got unavailable=" ^ showResult showOptional unavailable ^ ", available=" ^ showResult showOptional available)
let testRetainsInputOutputTradeoffs () =
 let semantics = semantics [inputValue, "value"] in let body = block [parameter "value" inputValue] [] inputValue in
 let initial = signature [O.ConsumedParameter "value"] (O.ProducedResult "value") in
 let expected = [signature [O.ConsumedParameter "value"] (O.ProducedResult "value"); signature [O.UniqueParameter "value"] (O.UniqueProducedResult "value")] in
 let actual = infer semantics initial body in
 if actual = Ok expected then Ok () else Error ("Expected incomparable transfer and unique boundaries " ^ showSignatures expected ^ ", got " ^ showResult showSignatures actual)
let testEscapeRevokesUniqueResult () =
 let semantics = semantics [inputValue, "input"; outputValue, "output"] in
 let escapeOperand : H.operand = {H.expression = CheckedAST.Local (binding "escape"); typ = AST.TUnit; inputs = CheckedAST.BindingIdMap.of_list [binding "output", outputValue]} in
 let body = block [parameter "input" inputValue] [O.Evaluate (H.Leaf (Reuse ("input", "output"))); O.Evaluate (H.ScalarBinding (unitValue, escapeOperand))] outputValue in
 let initial = signature [O.ConsumedParameter "input"] (O.ProducedResult "output") in
 let expected = [signature [O.UniqueParameter "input"] (O.ProducedResult "output")] in let actual = infer semantics initial body in
 if actual = Ok expected then Ok () else Error ("Expected scalar escape to remove only the result uniqueness promise, got " ^ showResult showSignatures actual)
let testRejectsUnprovableBoundary () =
 let semantics = semantics [inputValue, "value"] in let body = block [parameter "value" inputValue] [] inputValue in
 let invalid = signature [O.BorrowedParameter "value"] (O.ProducedResult "value") in
 match infer semantics invalid body with Error (U.NoVerifiedBoundary _) -> Ok () | actual -> Error ("Expected no verified ownership boundary, got " ^ showResult showSignatures actual)
let testBoundsVariantSearch () =
 let managedParameters = List.init 9 (fun index -> ({H.id = H.ValueId index; typ = AST.TList AST.TInt64} : H.value), "value" ^ string_of_int index) in
 let semantics = semantics managedParameters in let parameters = List.map (fun (value, ownership) -> parameter ownership value) managedParameters in
 let boundary = signature (List.map (fun (_, id) -> O.ConsumedParameter id) managedParameters) O.UnmanagedResult in
 let body = block parameters [] unitValue in
 match infer semantics boundary body with Error (U.VariantLimitExceeded (9, 256)) -> Ok () | actual -> Error ("Expected bounded uniqueness search, got " ^ showResult showSignatures actual)
let testDefersRecursiveInference () =
 let semantics = semantics [inputValue, "value"] in
 let recursiveCall : H.functionCall = {H.target = TestIds.functionIdForName "test"; arguments = [inputValue]; result = inputValue} in
 let body = block [parameter "value" inputValue] [O.Evaluate (H.Call recursiveCall)] inputValue in
 let boundary = signature [O.ConsumedParameter "value"] (O.ProducedResult "value") in
 match infer semantics boundary body with Error (U.RecursiveFunctionRequiresGroupInference id) when id = TestIds.functionIdForName "test" -> Ok () | actual -> Error ("Expected recursive uniqueness inference to require a group solver, got " ^ showResult showSignatures actual)
let tests = [
 "Uniqueness inference proves required reuse boundaries", testRequiresAndReturnsUniqueReuse;
 "Uniqueness inference resolves only concrete call demand", testInfersOnlyConcreteCallDemand;
 "Uniqueness inference retains input and output tradeoffs", testRetainsInputOutputTradeoffs;
 "Scalar escapes revoke inferred unique results", testEscapeRevokesUniqueResult;
 "Uniqueness inference rejects unprovable boundaries", testRejectsUnprovableBoundary;
 "Uniqueness inference bounds specialization variants", testBoundsVariantSearch;
 "Uniqueness inference defers recursive functions to a group solver", testDefersRecursiveInference
]
