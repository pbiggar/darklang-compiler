(* OwnedFunctionGroupInferenceTests.fs - Program-level ownership inference laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module G = InferOwnedFunctionGroups
module Identity = struct type t = string let compare = StringOrder.compare module Set = StringOrder.Set end
module Inference = G.Make (Identity)
module V = Inference.Ownership
module U = Inference.Uniqueness
module S = Identity.Set
module FS = SpecializationIdentity.FunctionSet
module F = OwnershipTestFormatting
type testLeaf = TestLeaf [@@warning "-37"]
let value id : H.value = {H.id = H.ValueId id; typ = AST.TList AST.TInt64}
let unitValue id : H.value = {H.id = H.ValueId id; typ = AST.TUnit}
let binding name = AST.bindingId (Int32.to_int (Array.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (Text.scalars name)))
let parameter name value : H.parameter = {H.binding = binding name; value}
let signature parameters result : string O.functionSignature = {O.parameters; result}
let block parameters operations result : (testLeaf, string) O.block = {O.body = {H.parameters; operations; result}}
let definition name ownership body : (testLeaf, string) O.functionDef = {O.definition = {H.id = TestIds.functionIdForName name; name; body}; ownership}
let call target arguments result = O.Evaluate (H.Call {H.target = TestIds.functionIdForName target; arguments; result})
let callSignature parameters result : O.callSignature = {O.parameters; result}
let semantics mappings registeredCalls : testLeaf V.semantics =
 let ownershipByValue = H.ValueMap.of_list (List.map (fun ((value : H.value), ownership) -> value.H.id, ownership) mappings) in
 {V.leaf = (fun TestLeaf -> {O.inputs = []; outputs = []}); leafUniqueness = (fun TestLeaf -> {V.requiredInputs = S.empty; uniqueOutputs = S.empty});
  callOwnership = (fun call -> FunctionIdMap.tryFind call.H.target registeredCalls); scalarUses = (fun _ -> S.empty); scalarEscapes = (fun _ -> S.empty);
  blockArgument = (fun (value : H.value) -> match H.ValueMap.find_opt value.H.id ownershipByValue with Some ownership -> O.Managed ownership | None -> O.Unmanaged)}
let candidateSummary candidate = List.map (fun (boundary : string G.functionBoundary) -> boundary.InferRecursiveOwnership.name, boundary.InferRecursiveOwnership.ownership) (G.candidateBoundaries candidate)
let groupSummary group = G.isRecursive group, G.internalDependencies group, G.externalTargets group, List.map candidateSummary (G.candidates group)
let svSet values = StructuralValue.Union ("set", [StructuralValue.Sequence (List.map AST.DiagnosticFormatting.func (FS.elements values))])
let svCandidate values = StructuralValue.Sequence (List.map (fun (name, boundary) -> StructuralValue.Tuple [StructuralValue.Text name; F.signature (fun id -> StructuralValue.Text id) boundary]) values)
let svSummary (recursive, dependencies, targets, candidates) = StructuralValue.Tuple [StructuralValue.Scalar (string_of_bool recursive); svSet dependencies; svSet targets; StructuralValue.Sequence (List.map svCandidate candidates)]
let svInferenceError = function
 | U.VariantLimitExceeded (count, maximum) -> StructuralValue.Union ("VariantLimitExceeded", [StructuralValue.Scalar (string_of_int count); StructuralValue.Scalar (string_of_int maximum)])
 | U.RecursiveFunctionRequiresGroupInference id -> StructuralValue.Union ("RecursiveFunctionRequiresGroupInference", [AST.DiagnosticFormatting.func id])
 | U.NoVerifiedBoundary error -> StructuralValue.Scalar ("NoVerifiedBoundary (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")")
 | U.NoVerifiedFunctionGroup error -> StructuralValue.Scalar ("NoVerifiedFunctionGroup (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")")
let svError = function
 | Inference.FunctionGroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id) -> StructuralValue.Union ("FunctionGroupingFailed", [StructuralValue.Union ("DuplicateFunctionName", [AST.DiagnosticFormatting.func id])])
 | Inference.DemandTargetMissing id -> StructuralValue.Union ("DemandTargetMissing", [AST.DiagnosticFormatting.func id])
 | Inference.GroupInferenceFailed (names, cause) -> StructuralValue.Union ("GroupInferenceFailed", [StructuralValue.Record ["Head", StructuralValue.Text names.NonEmptyList.head; "Tail", StructuralValue.Sequence (List.map (fun name -> StructuralValue.Text name) names.NonEmptyList.tail)]; svInferenceError cause])
let showSummaries = function Ok values -> StructuralFormat.format (StructuralValue.Union ("Ok", [StructuralValue.Sequence (List.map svSummary values)])) | Error error -> StructuralFormat.format (StructuralValue.Union ("Error", [svError error]))
let showGroups actual = showSummaries (Result.map (List.map groupSummary) actual)
let testInfersAcyclicGroupsCalleeFirst () =
 let leafInput = value 0 in let leafBoundary = signature [O.ConsumedParameter "leafInput"] (O.ProducedResult "leafInput") in
 let leaf = definition "leaf" leafBoundary (block [parameter "input" leafInput] [] leafInput) in
 let entryInput = value 10 in let intermediate = value 11 in let entryResult = value 12 in
 let entryBoundary = signature [O.ConsumedParameter "entryInput"] (O.ProducedResult "entryResult") in
 let entry = definition "entry" entryBoundary (block [parameter "input" entryInput] [call "leaf" [entryInput] intermediate; call "external" [intermediate] entryResult] entryResult) in
 let transferredCall = callSignature [O.ConsumedCallParameter] O.ProducedCallResult in
 let actual = Inference.infer (semantics [leafInput, "leafInput"; entryInput, "entryInput"; intermediate, "intermediate"; entryResult, "entryResult"]
  (FunctionIdMap.ofList [TestIds.functionIdForName "leaf", transferredCall; TestIds.functionIdForName "external", transferredCall])) [entry; leaf] |> Result.map (List.map groupSummary) in
 let expected = Ok [false, FS.empty, FS.empty, [["leaf", leafBoundary]; ["leaf", signature [O.UniqueParameter "leafInput"] (O.UniqueProducedResult "leafInput")]];
  false, FS.singleton (TestIds.functionIdForName "leaf"), FS.singleton (TestIds.functionIdForName "external"), [["entry", entryBoundary]]] in
 if actual = expected then Ok () else Error ("Expected callee-first inferred groups with every boundary tradeoff " ^ showSummaries expected ^ ", got " ^ showSummaries actual)
let testInfersSelfAndMutuallyRecursiveGroups () =
 let selfValue = unitValue 20 in let mutualAValue = unitValue 21 in let mutualBValue = unitValue 22 in
 let unmanagedBoundary = signature [O.UnmanagedParameter] O.UnmanagedResult in
 let recursive name target bodyValue = definition name unmanagedBoundary (block [parameter "unit" bodyValue] [call target [bodyValue] bodyValue] bodyValue) in
 let self = recursive "self" "self" selfValue in let mutualA = recursive "mutualA" "mutualB" mutualAValue in let mutualB = recursive "mutualB" "mutualA" mutualBValue in
 let actual = Inference.infer (semantics [] FunctionIdMap.empty) [self; mutualA; mutualB] |> Result.map (List.map groupSummary) in
 let expected = Ok [true, FS.empty, FS.empty, [["self", unmanagedBoundary]]; true, FS.empty, FS.empty, [["mutualA", unmanagedBoundary; "mutualB", unmanagedBoundary]]] in
 if actual = expected then Ok () else Error ("Expected self and mutual SCCs to use group-wide inference " ^ showSummaries expected ^ ", got " ^ showSummaries actual)
let testReportsFailingGroupNames () =
 let parameters start count = List.init count (fun offset -> let index = start + offset in value index, "value" ^ string_of_int index) in
 let firstParameters = parameters 30 5 in let secondParameters = parameters 40 4 in
 let wideDefinition name target parameters result = let values = List.map fst parameters in
  definition name (signature (List.map (fun (_, id) -> O.ConsumedParameter id) parameters) O.UnmanagedResult) (block (List.map (fun (value, name) -> parameter name value) parameters) [call target values result] result) in
 let first = wideDefinition "first" "second" firstParameters (unitValue 50) in let second = wideDefinition "second" "first" secondParameters (unitValue 51) in
 match Inference.infer (semantics (firstParameters @ secondParameters) FunctionIdMap.empty) [first; second] with
 | Error (Inference.GroupInferenceFailed (names, U.VariantLimitExceeded (9, 256))) when NonEmptyList.toList names = ["first"; "second"] -> Ok ()
 | actual -> Error ("Expected the failing SCC names and inference failure, got " ^ showGroups actual)
let testReportsGroupingFailures () =
 let bodyValue = unitValue 60 in let duplicate = definition "duplicate" (signature [O.UnmanagedParameter] O.UnmanagedResult) (block [parameter "unit" bodyValue] [] bodyValue) in
 match Inference.infer (semantics [] FunctionIdMap.empty) [duplicate; duplicate] with
 | Error (Inference.FunctionGroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id)) when id = TestIds.functionIdForName "duplicate" -> Ok ()
 | actual -> Error ("Expected duplicate definitions to retain their grouping error, got " ^ showGroups actual)
let tests = [
 "Owned function groups infer acyclic candidates in callee-first order", testInfersAcyclicGroupsCalleeFirst;
 "Owned function groups infer self and mutual recursion as proof units", testInfersSelfAndMutuallyRecursiveGroups;
 "Owned function group inference reports the failing SCC", testReportsFailingGroupNames;
 "Owned function group inference preserves grouping failures", testReportsGroupingFailures
]
