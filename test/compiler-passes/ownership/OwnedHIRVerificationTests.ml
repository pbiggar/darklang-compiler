(* OwnedHIRVerificationTests.ml - Joint typed, effect, alias, and ownership boundary laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module Identity = struct type t = string let compare = StringOrder.compare module Set = StringOrder.Set end
module V = VerifyOwnedHIR.Make (Identity)
module Contracts = V.Ownership
type testLeaf = TestLeaf [@@warning "-37"]
let parameter : H.value = {H.id = H.ValueId 0; typ = AST.TInt64}
let alias : H.value = {H.id = H.ValueId 3; typ = AST.TInt64}
let call target result = O.Evaluate (H.Call {H.target = TestIds.functionIdForName target; arguments = [parameter]; result})
let block operations result : (testLeaf, string) O.block = {O.body = {H.parameters = [{H.binding = AST.bindingId 0; value = parameter}]; operations; result}}
let ownedFunction name operations result : (testLeaf, string) O.functionDef = {O.definition = {H.id = TestIds.functionIdForName name; name; body = block operations result}; ownership = {O.parameters = [O.BorrowedParameter "value"]; result = O.BorrowedResult "value"}}
let callContract (call : H.functionCall) : H.primitiveContract = {H.inputs = call.H.arguments; operands = []; outputs = [{H.value = call.H.result; alias = H.MayAliasInputs (parameter, [])}]; effects = H.EffectSet.singleton H.MayInvokeUserCode}
let hirContracts includeCallContract : testLeaf VerifyOwnedHIR.hirContracts = {VerifyOwnedHIR.leaf = (fun TestLeaf -> {H.inputs = []; operands = []; outputs = []; effects = H.EffectSet.empty}); callSignature = (fun _ -> None); callContract = (fun call -> if includeCallContract then Some (callContract call) else None)}
let ownership registered : testLeaf Contracts.semantics = {
 Contracts.leaf = (fun TestLeaf -> {O.inputs = []; outputs = []});
 leafUniqueness = (fun TestLeaf -> {Contracts.requiredInputs = Identity.Set.empty; uniqueOutputs = Identity.Set.empty});
 callOwnership = registered;
 scalarUses = (fun (operand : H.operand) -> CheckedAST.BindingIdMap.bindings operand.H.inputs |> List.map (fun _ -> "value") |> Identity.Set.of_list);
 scalarEscapes = (fun _ -> Identity.Set.empty);
 blockArgument = (fun (value : H.value) -> match value.H.id with H.ValueId 0 | H.ValueId 3 -> O.Managed "value" | _ -> O.Unmanaged)
}
let describe = function V.HIRVerificationFailed error -> "HIRVerificationFailed (" ^ VerifyHIR.errorToString error ^ ")" | V.OwnershipVerificationFailed error -> "OwnershipVerificationFailed (" ^ Contracts.errorToString (fun id -> StructuralValue.Text id) error ^ ")"
let display = function Ok () -> "Ok ()" | Error error -> "Error (" ^ describe error ^ ")"
let verify hir ownership functions () = match V.verifyFunctions hir ownership functions with Ok () -> Ok () | Error error -> Error ("Unexpected owned HIR verification failure: " ^ describe error)
let testDirectFunctionGroup () = let callee = ownedFunction "callee" [] parameter in let caller = ownedFunction "caller" [call "callee" alias] alias in verify (hirContracts true) (ownership (fun _ -> None)) [callee;caller] ()
let testRecursiveFunctionGroup () = let recursive = ownedFunction "recursive" [call "recursive" alias] alias in verify (hirContracts true) (ownership (fun _ -> None)) [recursive] ()
let testRequiresIndependentCallContract () = let recursive = ownedFunction "recursive" [call "recursive" alias] alias in let actual = V.verifyFunctions (hirContracts false) (ownership (fun _ -> None)) [recursive] in let expected = Error (V.HIRVerificationFailed (VerifyHIR.MissingCallContract (TestIds.functionIdForName "recursive"))) in if actual = expected then Ok () else Error ("Expected " ^ display expected ^ ", got " ^ display actual)
let testRejectsPositionalOwnershipMismatch () = let invalid = { (ownedFunction "invalid" [] parameter) with O.ownership = {O.parameters = [O.UnmanagedParameter]; result = O.UnmanagedResult}} in let actual = V.verifyFunctions (hirContracts true) (ownership (fun _ -> None)) [invalid] in let expected = Error (V.OwnershipVerificationFailed Contracts.InconsistentFunctionParameters) in if actual = expected then Ok () else Error ("Expected " ^ display expected ^ ", got " ^ display actual)
let testRejectsConflictingOwnershipRegistration () = let recursive = ownedFunction "recursive" [call "recursive" alias] alias in let registered (call : H.functionCall) = if call.H.target = TestIds.functionIdForName "recursive" then Some ({O.parameters = [O.ConsumedCallParameter]; result = O.ProducedCallResult} : O.callSignature) else None in let actual = V.verifyFunctions (hirContracts true) (ownership registered) [recursive] in let expected = Error (V.OwnershipVerificationFailed (Contracts.InconsistentRegisteredCallOwnership (TestIds.functionIdForName "recursive"))) in if actual = expected then Ok () else Error ("Expected " ^ display expected ^ ", got " ^ display actual)
let tests = [
 "Owned HIR derives direct-call ownership from function definitions", testDirectFunctionGroup;
 "Owned HIR derives recursive-call ownership from function definitions", testRecursiveFunctionGroup;
 "Owned HIR requires independent call effect and alias contracts", testRequiresIndependentCallContract;
 "Owned HIR rejects positional ownership mismatches", testRejectsPositionalOwnershipMismatch;
 "Owned HIR rejects conflicting ownership registrations", testRejectsConflictingOwnershipRegistration
]
