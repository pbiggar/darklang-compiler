(* OwnershipVariantSelectionTests.fs - Deterministic call-site ownership selection laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module G = InferOwnedFunctionGroups
module Selection = SelectOwnershipVariants
module Identity = struct type t = string let compare = StringOrder.compare module Set = StringOrder.Set end
module Inference = G.Make (Identity)
module V = Inference.Ownership
module U = Inference.Uniqueness
module Selector = Selection.Make (Identity)
module S = Identity.Set
module IS = O.IntSet
module F = OwnershipTestFormatting
let ( let* ) = Result.bind
type testLeaf = Reuse of string * string
let value id : H.value = {H.id = H.ValueId id; typ = AST.TList AST.TInt64}
let binding name = AST.bindingId (Int32.to_int (Array.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (HostText.scalars name)))
let parameter name value : H.parameter = {H.binding = binding name; value}
let signature parameters result : string O.functionSignature = {O.parameters; result}
let block parameters operations result : (testLeaf, string) O.block = {O.body = {H.parameters; operations; result}}
let definition name ownership body : (testLeaf, string) O.functionDef = {O.definition = {H.id = TestIds.functionIdForName name; name; body}; ownership}
let call target arguments result = O.Evaluate (H.Call {H.target = TestIds.functionIdForName target; arguments; result})
let condition : H.operand = {H.expression = CheckedAST.BoolLiteral true; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty}
let semantics mappings : testLeaf V.semantics =
 let ownershipByValue = H.ValueMap.of_list (List.map (fun ((value : H.value), ownership) -> value.H.id, ownership) mappings) in
 {V.leaf = (fun (Reuse (input, output)) -> {O.inputs = [O.Consumed input]; outputs = [output]}); leafUniqueness = (fun (Reuse (input, output)) -> {V.requiredInputs = S.singleton input; uniqueOutputs = S.singleton output});
  callOwnership = (fun _ -> None); scalarUses = (fun _ -> S.empty); scalarEscapes = (fun _ -> S.empty);
  blockArgument = (fun (value : H.value) -> match H.ValueMap.find_opt value.H.id ownershipByValue with Some ownership -> O.Managed ownership | None -> O.Unmanaged)}
let transferredCall : O.callSignature = {O.parameters = [O.ConsumedCallParameter]; result = O.ProducedCallResult}
let siteWithBoundary target established uniqueArguments : Selection.callSite = {Selection.target; established; uniqueArguments}
let site target uniqueArguments = siteWithBoundary target transferredCall uniqueArguments
let svSelectionError = function
 | Selection.DuplicateFunctionName name -> StructuralValue.Union ("DuplicateFunctionName", [StructuralValue.Text name])
 | Selection.UnknownFunction name -> StructuralValue.Union ("UnknownFunction", [StructuralValue.Text name])
 | Selection.InvalidUniqueArgumentIndex (name, index) -> StructuralValue.Union ("InvalidUniqueArgumentIndex", [StructuralValue.Text name; StructuralValue.Scalar (string_of_int index)])
 | Selection.MissingEstablishedUniqueArgument (name, index) -> StructuralValue.Union ("MissingEstablishedUniqueArgument", [StructuralValue.Text name; StructuralValue.Scalar (string_of_int index)])
 | Selection.InconsistentEstablishedBoundary name -> StructuralValue.Union ("InconsistentEstablishedBoundary", [StructuralValue.Text name])
let showSelectionError error = HostStructuralFormat.format (svSelectionError error)
let showInferenceError = function
 | Inference.FunctionGroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id) -> "FunctionGroupingFailed (DuplicateFunctionName " ^ HostStructuralFormat.format (AST.DiagnosticFormatting.func id) ^ ")"
 | Inference.DemandTargetMissing id -> "DemandTargetMissing " ^ HostStructuralFormat.format (AST.DiagnosticFormatting.func id)
 | Inference.GroupInferenceFailed (names, cause) ->
   let cause = match cause with U.VariantLimitExceeded (count, maximum) -> Printf.sprintf "VariantLimitExceeded (%d, %d)" count maximum
    | U.RecursiveFunctionRequiresGroupInference id -> "RecursiveFunctionRequiresGroupInference " ^ HostStructuralFormat.format (AST.DiagnosticFormatting.func id)
    | U.NoVerifiedBoundary error -> "NoVerifiedBoundary (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")"
    | U.NoVerifiedFunctionGroup error -> "NoVerifiedFunctionGroup (" ^ V.errorToString (fun id -> StructuralValue.Text id) error ^ ")" in
   "GroupInferenceFailed (" ^ HostStructuralFormat.format (StructuralValue.Record ["Head", StructuralValue.Text names.NonEmptyList.head; "Tail", StructuralValue.Sequence (List.map (fun name -> StructuralValue.Text name) names.NonEmptyList.tail)]) ^ ", " ^ cause ^ ")"
let catalog semantics definitions =
 let* groups = Inference.infer semantics definitions |> Result.map_error (fun error -> "Inference failed: " ^ showInferenceError error) in
 Selection.create groups |> Result.map_error (fun error -> "Catalog creation failed: " ^ showSelectionError error)
let selected = function Selection.EstablishedBoundary _ -> Error "Selected the established boundary" | Selection.InferredVariant variant -> Ok variant
let select variants site = Selector.select variants site |> Result.map_error showSelectionError
let svBoundary (boundary : string G.functionBoundary) = StructuralValue.Record ["Id", AST.DiagnosticFormatting.func boundary.InferRecursiveOwnership.id; "Name", StructuralValue.Text boundary.InferRecursiveOwnership.name; "Ownership", F.signature (fun id -> StructuralValue.Text id) boundary.InferRecursiveOwnership.ownership]
let svCandidate candidate = match G.candidateBoundaries candidate with head :: tail -> StructuralValue.Union ("Candidate", [svBoundary head; StructuralValue.Sequence (List.map svBoundary tail)]) | [] -> failwith "Ownership variant candidate has no function boundaries"
let svIdentity identity =
 let boundaries = Selection.identityBoundaries identity in match boundaries with
 | (name, signature) :: tail -> StructuralValue.Union ("CandidateIdentity", [StructuralValue.Record ["Head", StructuralValue.Tuple [StructuralValue.Text name; F.callSignature signature]; "Tail", StructuralValue.Sequence (List.map (fun (name, signature) -> StructuralValue.Tuple [StructuralValue.Text name; F.callSignature signature]) tail)]])
 | [] -> failwith "Ownership variant candidate has no function boundaries"
let svSelection = function
 | Selection.EstablishedBoundary boundary -> StructuralValue.Union ("EstablishedBoundary", [F.callSignature boundary])
 | Selection.InferredVariant selected -> StructuralValue.Union ("InferredVariant", [StructuralValue.Record ["Identity", svIdentity (Selection.selectedIdentity selected); "Candidate", svCandidate (Selection.selectedCandidate selected); "TargetBoundary", svBoundary (Selection.selectedTargetBoundary selected); "CallSignature", F.callSignature (Selection.selectedCallSignature selected)]])
let svResult encode encodeError = function Ok value -> StructuralValue.Union ("Ok", [encode value]) | Error error -> StructuralValue.Union ("Error", [encodeError error])
let showActual actual = HostStructuralFormat.format (svResult svSelection svSelectionError actual)
let svPairs values = StructuralValue.Sequence (List.map (fun (name, signature) -> StructuralValue.Tuple [StructuralValue.Text name; F.signature (fun id -> StructuralValue.Text id) signature]) values)
let testSelectsByAvailableUniqueness () =
 let input = value 0 in let boundary = signature [O.ConsumedParameter "input"] (O.ProducedResult "input") in
 let identity = definition "identity" boundary (block [parameter "input" input] [] input) in
 let* variants = catalog (semantics [input, "input"]) [identity] in
 let* ordinary = select variants (site "identity" IS.empty) in let* ordinary = selected ordinary in
 let* unique = select variants (site "identity" (IS.singleton 0)) in let* unique = selected unique in
 let* repeated = select variants (site "identity" (IS.singleton 0)) in let* repeated = selected repeated in
 let ordinarySignature = Selection.selectedCallSignature ordinary in let uniqueSignature = Selection.selectedCallSignature unique in
 let identityIsStable = Selection.selectedIdentity unique = Selection.selectedIdentity repeated in
 let candidatesDiffer = Selection.selectedIdentity ordinary <> Selection.selectedIdentity unique in
 let expectedUnique : O.callSignature = {O.parameters = [O.UniqueCallParameter]; result = O.UniqueProducedCallResult} in
 if ordinarySignature = transferredCall && uniqueSignature = expectedUnique && identityIsStable && candidatesDiffer then Ok ()
 else Error ("Expected stable capability-aware selection, got ordinary=" ^ HostStructuralFormat.format (F.callSignature ordinarySignature) ^ ", unique=" ^ HostStructuralFormat.format (F.callSignature uniqueSignature) ^ ", stable=" ^ string_of_bool identityIsStable ^ ", distinct=" ^ string_of_bool candidatesDiffer)
let testFallsBackToEstablishedBoundary () =
 let input = value 10 in let output = value 11 in
 let reuse = definition "reuse" (signature [O.ConsumedParameter "input"] (O.ProducedResult "output")) (block [parameter "input" input] [O.Evaluate (H.Leaf (Reuse ("input", "output")))] output) in
 let* variants = catalog (semantics [input, "input"; output, "output"]) [reuse] in
 let* unavailable = select variants (site "reuse" IS.empty) in let* available = select variants (site "reuse" (IS.singleton 0)) in
 match unavailable, available with
 | Selection.EstablishedBoundary boundary, Selection.InferredVariant selected when boundary = transferredCall && Selection.selectedCallSignature selected = ({O.parameters = [O.UniqueCallParameter]; result = O.UniqueProducedCallResult} : O.callSignature) -> Ok ()
 | actual -> Error ("Expected established fallback followed by inferred reuse, got " ^ HostStructuralFormat.format (StructuralValue.Tuple [svSelection (fst actual); svSelection (snd actual)]))
let recursiveFixture () =
 let firstInput = value 20 in let firstRecursive = value 21 in let firstResult = value 22 in
 let secondInput = value 30 in let secondRecursive = value 31 in let secondResult = value 32 in
 let recursiveBranch target input result = block [] [call target [input] result] result in
 let baseBranch input = block [] [] input in
 let recursiveDefinition name target input recursiveResult result = definition name (signature [O.ConsumedParameter (name ^ "Input")] (O.ProducedResult (name ^ "Result"))) (block [parameter "input" input] [O.Evaluate (H.Branch (result, condition, recursiveBranch target input recursiveResult, baseBranch input))] result) in
 let first = recursiveDefinition "first" "second" firstInput firstRecursive firstResult in let second = recursiveDefinition "second" "first" secondInput secondRecursive secondResult in
 semantics [firstInput, "firstInput"; firstRecursive, "firstRecursive"; firstResult, "firstResult"; secondInput, "secondInput"; secondRecursive, "secondRecursive"; secondResult, "secondResult"], [first; second]
let testSelectsRecursiveGroupsAtomically () =
 let semantics, definitions = recursiveFixture () in let* variants = catalog semantics definitions in
 let* selection = select variants (site "first" (IS.singleton 0)) in let* selection = selected selection in
 let boundaries = List.map (fun (boundary : string G.functionBoundary) -> boundary.InferRecursiveOwnership.name, boundary.InferRecursiveOwnership.ownership) (G.candidateBoundaries (Selection.selectedCandidate selection)) in
 let expected = ["first", signature [O.UniqueParameter "firstInput"] (O.UniqueProducedResult "firstResult"); "second", signature [O.UniqueParameter "secondInput"] (O.UniqueProducedResult "secondResult")] in
 let identityNames = List.map fst (Selection.identityBoundaries (Selection.selectedIdentity selection)) in
 if boundaries = expected && identityNames = ["first"; "second"] then Ok () else Error ("Expected one atomic recursive candidate " ^ HostStructuralFormat.format (svPairs expected) ^ ", got " ^ HostStructuralFormat.format (svPairs boundaries))
let testRejectsInvalidCatalogAndCalls () =
 let input = value 40 in let boundary = signature [O.ConsumedParameter "input"] (O.ProducedResult "input") in let identity = definition "identity" boundary (block [parameter "input" input] [] input) in
 let* groups = Inference.infer (semantics [input, "input"]) [identity] |> Result.map_error (fun error -> "Inference failed: " ^ showInferenceError error) in
 match Selection.create (groups @ groups) with
 | Error (Selection.DuplicateFunctionName "identity") ->
   let* variants = Selection.create groups |> Result.map_error showSelectionError in
   (match Selector.select variants (site "missing" IS.empty) with
   | Error (Selection.UnknownFunction "missing") -> (match Selector.select variants (site "identity" (IS.singleton 1)) with
     | Error (Selection.InvalidUniqueArgumentIndex ("identity", 1)) ->
       let establishedUnique : O.callSignature = {O.parameters = [O.UniqueCallParameter]; result = O.ProducedCallResult} in
       (match Selector.select variants (siteWithBoundary "identity" establishedUnique IS.empty) with
       | Error (Selection.MissingEstablishedUniqueArgument ("identity", 0)) ->
         let borrowed : O.callSignature = {O.parameters = [O.BorrowedCallParameter]; result = O.BorrowedCallResult 0} in
         (match Selector.select variants (siteWithBoundary "identity" borrowed IS.empty) with Error (Selection.InconsistentEstablishedBoundary "identity") -> Ok () | actual -> Error ("Expected an inconsistent established boundary, got " ^ showActual actual))
       | actual -> Error ("Expected missing established uniqueness, got " ^ showActual actual))
     | actual -> Error ("Expected an invalid uniqueness index, got " ^ showActual actual))
   | actual -> Error ("Expected an unknown call target, got " ^ showActual actual))
 | Error error -> Error ("Expected duplicate catalog entries to fail, got Error (" ^ showSelectionError error ^ ")")
 | Ok _ -> Error "Expected duplicate catalog entries to fail"
let selectIn definitions semantics site = let* variants = catalog semantics definitions in select variants site
let showIdentityResults (first, second) = HostStructuralFormat.format (StructuralValue.Tuple [svResult svIdentity (fun error -> StructuralValue.Text error) first; svResult svIdentity (fun error -> StructuralValue.Text error) second])
let testCanonicalRecursiveIdentity () =
 let semantics, definitions = recursiveFixture () in
 let identity definitions target = let* selection = selectIn definitions semantics (site target (IS.singleton 0)) in let* selection = selected selection in Ok (Selection.selectedIdentity selection) in
 let first = identity definitions "first" in let second = identity (List.rev definitions) "second" in
 match first, second with Ok first, Ok second when first = second -> Ok () | actual -> Error ("Expected one candidate identity independent of definition order and entry member, got " ^ showIdentityResults actual)
let testIdentityIgnoresLocalOwnershipNames () =
 let selectIdentity localName = let input = value 50 in let identity = definition "identity" (signature [O.ConsumedParameter localName] (O.ProducedResult localName)) (block [parameter localName input] [] input) in
  let* selection = selectIn [identity] (semantics [input, localName]) (site "identity" (IS.singleton 0)) in let* selection = selected selection in Ok (Selection.selectedIdentity selection) in
 let first = selectIdentity "original" in let second = selectIdentity "renamed" in
 match first, second with Ok first, Ok second when first = second -> Ok () | actual -> Error ("Expected candidate identity to depend only on positional boundaries, got " ^ showIdentityResults actual)
let testPreservesPositionalTransfers () =
 let scalar : H.value = {H.id = H.ValueId 60; typ = AST.TInt64} in let borrowed = value 61 in let consumed = value 62 in
 let mixed = definition "mixed" (signature [O.UnmanagedParameter; O.BorrowedParameter "borrowed"; O.ConsumedParameter "consumed"] (O.ProducedResult "consumed")) (block [parameter "scalar" scalar; parameter "borrowed" borrowed; parameter "consumed" consumed] [] consumed) in
 let established : O.callSignature = {O.parameters = [O.UnmanagedCallParameter; O.BorrowedCallParameter; O.ConsumedCallParameter]; result = O.ProducedCallResult} in
 let select unique = let* selection = selectIn [mixed] (semantics [borrowed, "borrowed"; consumed, "consumed"]) (siteWithBoundary "mixed" established unique) in let* selection = selected selection in Ok (Selection.selectedCallSignature selection) in
 let expectedUnique : O.callSignature = {O.parameters = [O.UnmanagedCallParameter; O.BorrowedCallParameter; O.UniqueCallParameter]; result = O.UniqueProducedCallResult} in
 let first = select (IS.singleton 1) in let second = select (IS.singleton 2) in
 match first, second with Ok ordinary, Ok unique when ordinary = established && unique = expectedUnique -> Ok () | actual -> Error ("Expected uniqueness at the consumed argument's original position only, got " ^ HostStructuralFormat.format (StructuralValue.Tuple [svResult F.callSignature (fun error -> StructuralValue.Text error) (fst actual); svResult F.callSignature (fun error -> StructuralValue.Text error) (snd actual)]))
let testPreservesBorrowedResultSource () =
 let first = value 70 in let second = value 71 in let borrowSecond = definition "borrowSecond" (signature [O.BorrowedParameter "first"; O.BorrowedParameter "second"] (O.BorrowedResult "second")) (block [parameter "first" first; parameter "second" second] [] second) in
 let established : O.callSignature = {O.parameters = [O.BorrowedCallParameter; O.BorrowedCallParameter]; result = O.BorrowedCallResult 1} in
 let* variants = catalog (semantics [first, "first"; second, "second"]) [borrowSecond] in
 let valid = Selector.select variants (siteWithBoundary "borrowSecond" established (IS.of_list [0; 1])) in
 let invalid = Selector.select variants (siteWithBoundary "borrowSecond" {established with O.result = O.BorrowedCallResult 0} IS.empty) in
 match valid, invalid with Ok (Selection.InferredVariant chosen), Error (Selection.InconsistentEstablishedBoundary "borrowSecond") when Selection.selectedCallSignature chosen = established -> Ok () | actual -> Error ("Expected a borrowed result to keep its exact source parameter, got " ^ HostStructuralFormat.format (StructuralValue.Tuple [svResult svSelection svSelectionError (fst actual); svResult svSelection svSelectionError (snd actual)]))
let testDoesNotWeakenEstablishedUniqueResult () =
 let input = value 80 in let identity = definition "identity" (signature [O.ConsumedParameter "input"] (O.ProducedResult "input")) (block [parameter "input" input] [] input) in
 let established = {transferredCall with O.result = O.UniqueProducedCallResult} in
 let actual = selectIn [identity] (semantics [input, "input"]) (siteWithBoundary "identity" established IS.empty) in
 match actual with Ok (Selection.EstablishedBoundary retained) when retained = established -> Ok () | actual -> Error ("Expected fallback rather than weakening an established result guarantee, got " ^ HostStructuralFormat.format (svResult svSelection (fun error -> StructuralValue.Text error) actual))
let testRejectsTransferShapeMismatches () =
 let input = value 90 in let identity = definition "identity" (signature [O.ConsumedParameter "input"] (O.ProducedResult "input")) (block [parameter "input" input] [] input) in
 let invalidBoundaries : O.callSignature list = [{transferredCall with O.parameters = []}; {transferredCall with O.parameters = [O.UnmanagedCallParameter]}; {transferredCall with O.parameters = [O.BorrowedCallParameter]}; {transferredCall with O.result = O.UnmanagedCallResult}; {transferredCall with O.result = O.BorrowedCallResult 0}] in
 let* variants = catalog (semantics [input, "input"]) [identity] in
 List.fold_left (fun result boundary -> let* () = result in match Selector.select variants (siteWithBoundary "identity" boundary IS.empty) with Error (Selection.InconsistentEstablishedBoundary "identity") -> Ok () | actual -> Error ("Expected rejection of transfer shape " ^ HostStructuralFormat.format (F.callSignature boundary) ^ ", got " ^ showActual actual)) (Ok ()) invalidBoundaries
let testRejectsNegativeUniquePosition () =
 let input = value 100 in let identity = definition "identity" (signature [O.ConsumedParameter "input"] (O.ProducedResult "input")) (block [parameter "input" input] [] input) in
 let* variants = catalog (semantics [input, "input"]) [identity] in
 match Selector.select variants (site "identity" (IS.singleton (-1))) with Error (Selection.InvalidUniqueArgumentIndex ("identity", -1)) -> Ok () | actual -> Error ("Expected a negative unique argument position to fail, got " ^ showActual actual)
let tests = [
 "Ownership variants select by available argument uniqueness", testSelectsByAvailableUniqueness;
 "Ownership variants retain the established fallback", testFallsBackToEstablishedBoundary;
 "Ownership variants select recursive SCC candidates atomically", testSelectsRecursiveGroupsAtomically;
 "Ownership variant catalogs reject invalid calls", testRejectsInvalidCatalogAndCalls;
 "Ownership variants canonicalize recursive candidate identities", testCanonicalRecursiveIdentity;
 "Ownership variant identities ignore local ownership names", testIdentityIgnoresLocalOwnershipNames;
 "Ownership variants preserve positional unmanaged and borrowed transfers", testPreservesPositionalTransfers;
 "Ownership variants preserve borrowed result source parameters", testPreservesBorrowedResultSource;
 "Ownership variants preserve established unique result guarantees", testDoesNotWeakenEstablishedUniqueResult;
 "Ownership variants reject transfer shape mismatches", testRejectsTransferShapeMismatches;
 "Ownership variants reject negative uniqueness positions", testRejectsNegativeUniquePosition
]
