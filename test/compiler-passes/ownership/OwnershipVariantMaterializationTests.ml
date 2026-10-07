(* OwnershipVariantMaterializationTests.fs - Verify atomic specialization and call-boundary laws. *)
[@@@warning "-4-42"]
open Dark_compiler
module H = HIR
module O = OwnedIR
module M = MaterializeOwnershipVariants
module S = SelectOwnershipVariants
module G = InferOwnedFunctionGroups
module Inference = G.Make (ListLiveness.Identity)
module Selector = S.Make (ListLiveness.Identity)
module Materialize = M.Make (ListLiveness.Identity)
module V = Materialize.Verification
module Ownership = Materialize.Ownership
module FS = SpecializationIdentity.FunctionSet
module F = OwnershipTestFormatting
let ( let* ) = Result.bind
type leaf = Fresh of H.value [@@warning "-37"]
let value id : H.value = {H.id = H.ValueId id; typ = AST.TList AST.TInt64}
let functionId name = TestIds.functionIdForName name
let binding name = AST.bindingId (Int32.to_int (Array.fold_left (fun hash ch -> Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l (Text.scalars name)))
let signature parameters result : H.valueId O.functionSignature = {O.parameters; result}
let block parameters operations result : (leaf, H.valueId) O.block = {O.body = {H.parameters; operations; result}}
let parameter value : H.parameter = {H.binding = binding "input"; value}
let definition name ownership body : (leaf, H.valueId) O.functionDef = {O.definition = {H.id = functionId name; name; body}; ownership}
let call target argument result : H.functionCall = {H.target = functionId target; arguments = [argument]; result}
let ordinary : O.callSignature = {O.parameters = [O.ConsumedCallParameter]; result = O.ProducedCallResult}
let unique : O.callSignature = {O.parameters = [O.UniqueCallParameter]; result = O.UniqueProducedCallResult}
let semantics : leaf Ownership.semantics = {
 Ownership.leaf = (fun (Fresh output) -> {O.inputs = []; outputs = [output.H.id]});
 leafUniqueness = (fun (Fresh output) -> {Ownership.requiredInputs = H.ValueSet.empty; uniqueOutputs = H.ValueSet.singleton output.H.id});
 callOwnership = (fun _ -> None); scalarUses = (fun operand -> H.ValueSet.of_list (List.map (fun (_, (value : H.value)) -> value.H.id) (CheckedAST.BindingIdMap.bindings operand.H.inputs)));
 scalarEscapes = (fun _ -> H.ValueSet.empty); blockArgument = (fun (value : H.value) -> O.Managed value.H.id)
}
let hir names : leaf VerifyOwnedHIR.hirContracts = {VerifyOwnedHIR.leaf = (fun (Fresh output) -> {H.inputs = []; operands = []; outputs = [{H.value = output; alias = H.FreshManaged}]; effects = H.EffectSet.singleton H.MayAllocate}); callSignature = (fun _ -> None);
 callContract = (fun call -> match call.H.arguments with first :: rest when FS.mem call.H.target names -> Some {H.inputs = call.H.arguments; operands = []; outputs = [{H.value = call.H.result; alias = H.MayAliasInputs (first, rest)}]; effects = H.EffectSet.singleton H.MayInvokeUserCode} | _ -> None)}
let contracts definitions = hir (FS.of_list (List.map (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.id) definitions))
let svId (H.ValueId id) = StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])
let svSite (site : O.callSiteIdentity) = StructuralValue.Record ["Caller", AST.DiagnosticFormatting.func site.O.caller; "Result", svId site.O.result]
let svVerification = function V.HIRVerificationFailed error -> StructuralValue.Scalar ("HIRVerificationFailed (" ^ VerifyHIR.errorToString error ^ ")") | V.OwnershipVerificationFailed error -> StructuralValue.Scalar ("OwnershipVerificationFailed (" ^ Ownership.errorToString svId error ^ ")")
let svError = function
 | Materialize.GroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id) -> StructuralValue.Union ("GroupingFailed", [StructuralValue.Union ("DuplicateFunctionName", [AST.DiagnosticFormatting.func id])])
 | Materialize.InvalidOriginalProgram error -> StructuralValue.Union ("InvalidOriginalProgram", [svVerification error])
 | Materialize.InvalidMaterializedProgram error -> StructuralValue.Union ("InvalidMaterializedProgram", [svVerification error])
 | Materialize.MissingGroupMember name -> StructuralValue.Union ("MissingGroupMember", [StructuralValue.Text name])
 | Materialize.GroupMembershipMismatch name -> StructuralValue.Union ("GroupMembershipMismatch", [StructuralValue.Text name])
 | Materialize.BoundaryMismatch name -> StructuralValue.Union ("BoundaryMismatch", [StructuralValue.Text name])
 | Materialize.SymbolCollision name -> StructuralValue.Union ("SymbolCollision", [StructuralValue.Text name])
 | Materialize.MissingCallSite site -> StructuralValue.Union ("MissingCallSite", [svSite site])
 | Materialize.DuplicateCallSite site -> StructuralValue.Union ("DuplicateCallSite", [svSite site])
 | Materialize.StaleCallSite site -> StructuralValue.Union ("StaleCallSite", [svSite site])
 | Materialize.MixedRecursiveCandidate site -> StructuralValue.Union ("MixedRecursiveCandidate", [svSite site])
let report result = Result.map_error (fun error -> StructuralFormat.format (svError error)) result
let svCall (call : H.functionCall) = StructuralValue.Record ["Target", AST.DiagnosticFormatting.func call.H.target; "Arguments", StructuralValue.Sequence (List.map F.value call.H.arguments); "Result", F.value call.H.result]
let svIdentity identity =
 let pairs = List.map (fun (name, signature) -> StructuralValue.Tuple [StructuralValue.Text name; F.callSignature signature]) (S.identityBoundaries identity) in
 match pairs with head :: tail -> StructuralValue.Union ("CandidateIdentity", [StructuralValue.Record ["Head", head; "Tail", StructuralValue.Sequence tail]]) | [] -> failwith "Selected ownership candidate has no members"
let svMember (memberDefinition : (leaf, H.valueId) M.specializedFunction) = StructuralValue.Record ["Original", AST.DiagnosticFormatting.func memberDefinition.M.original; "Function", F.functionDef (fun (Fresh output) -> StructuralValue.Union ("Fresh", [F.value output])) svId memberDefinition.M.functionDef]
let svGroup group = StructuralValue.Record ["Identity", svIdentity group.M.identity; "Members", StructuralValue.Record ["Head", svMember group.M.members.NonEmptyList.head; "Tail", StructuralValue.Sequence (List.map svMember group.M.members.NonEmptyList.tail)]]
let svRewrite (rewrite : M.callRewrite) = StructuralValue.Record ["Site", svSite rewrite.M.site; "Original", svCall rewrite.M.original; "Specialized", svCall rewrite.M.specialized; "Ownership", F.callSignature rewrite.M.ownership]
let svPlan plan = StructuralValue.Record ["Originals", StructuralValue.Sequence (List.map (F.functionDef (fun (Fresh output) -> StructuralValue.Union ("Fresh", [F.value output])) svId) (M.originals plan)); "Groups", StructuralValue.Sequence (List.map svGroup (M.groups plan)); "Rewrites", StructuralValue.Sequence (List.map svRewrite (M.rewrites plan))]
let svResult result = match result with Ok plan -> StructuralValue.Union ("Ok", [svPlan plan]) | Error error -> StructuralValue.Union ("Error", [svError error])
let show result = StructuralFormat.format (svResult result)
let showPair (first, second) = StructuralFormat.format (StructuralValue.Tuple [svResult first; svResult second])
let inferenceError = function
 | Inference.FunctionGroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id) -> "FunctionGroupingFailed (DuplicateFunctionName " ^ StructuralFormat.format (AST.DiagnosticFormatting.func id) ^ ")"
 | Inference.DemandTargetMissing id -> "DemandTargetMissing " ^ StructuralFormat.format (AST.DiagnosticFormatting.func id)
 | Inference.GroupInferenceFailed (names, cause) ->
   let cause = match cause with Inference.Uniqueness.VariantLimitExceeded (count, maximum) -> Printf.sprintf "VariantLimitExceeded (%d, %d)" count maximum | Inference.Uniqueness.RecursiveFunctionRequiresGroupInference id -> "RecursiveFunctionRequiresGroupInference " ^ StructuralFormat.format (AST.DiagnosticFormatting.func id) | Inference.Uniqueness.NoVerifiedBoundary error -> "NoVerifiedBoundary (" ^ Ownership.errorToString svId error ^ ")" | Inference.Uniqueness.NoVerifiedFunctionGroup error -> "NoVerifiedFunctionGroup (" ^ Ownership.errorToString svId error ^ ")" in
   "GroupInferenceFailed (" ^ String.concat ", " (NonEmptyList.toList names) ^ ", " ^ cause ^ ")"
let selectionError = function S.DuplicateFunctionName name -> "DuplicateFunctionName " ^ name | S.UnknownFunction name -> "UnknownFunction " ^ name | S.InvalidUniqueArgumentIndex (name, index) -> Printf.sprintf "InvalidUniqueArgumentIndex (%s, %d)" name index | S.MissingEstablishedUniqueArgument (name, index) -> Printf.sprintf "MissingEstablishedUniqueArgument (%s, %d)" name index | S.InconsistentEstablishedBoundary name -> "InconsistentEstablishedBoundary " ^ name
let select definitions target uniqueArguments =
 let* groups = Inference.infer semantics definitions |> Result.map_error inferenceError in
 let* catalog = S.create groups |> Result.map_error selectionError in
 Selector.select catalog {S.target; established = ordinary; uniqueArguments} |> Result.map_error selectionError
let request caller call selection : H.valueId M.request = {M.caller = functionId caller; call; selection}
let run definitions requests = Materialize.materialize (contracts definitions) semantics FunctionIdMap.empty definitions requests
let identity () = let input = value 0 in definition "identity" (signature [O.ConsumedParameter input.H.id] (O.ProducedResult input.H.id)) (block [parameter input] [] input)
let caller name target baseId uniqueInput =
 let input = value baseId in let output = value (baseId + 1) in let boundary = if uniqueInput then O.UniqueParameter input.H.id else O.ConsumedParameter input.H.id in
 let invocation = call target input output in definition name (signature [boundary] (O.ProducedResult output.H.id)) (block [parameter input] [O.Evaluate (H.Call invocation)] output), invocation
let singleFixture () = let callee = identity () in let first, firstCall = caller "first" "identity" 10 true in let second, secondCall = caller "second" "identity" 20 true in callee, [callee; first; second], firstCall, secondCall
let testDeduplicatesAndVerifies () =
 let callee, definitions, firstCall, secondCall = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in
 let* plan = run definitions [request "first" firstCall chosen; request "second" secondCall chosen] |> report in
 let cloneCount = List.fold_left (fun count group -> count + NonEmptyList.length group.M.members) 0 (M.groups plan) in
 match M.rewrites plan with
 | [first; second] when cloneCount = 1 && first.M.specialized.H.target = second.M.specialized.H.target && first.M.ownership = unique && second.M.ownership = unique ->
   V.verifyFunctions (M.hirContracts plan (contracts definitions)) (Materialize.ownershipSemantics plan semantics) (M.functions plan) |> Result.map_error (fun error -> StructuralFormat.format (svVerification error))
 | _ -> Error ("Expected one verified clone shared by both call sites, got " ^ string_of_int cloneCount ^ " clones")
let testPreservesEstablishedCalls () =
 let callee, definitions, firstCall, secondCall = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in
 let* plan = run definitions [request "first" firstCall chosen; request "second" secondCall (S.EstablishedBoundary ordinary)] |> report in
 let secondBefore = List.find_opt (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.name = "second") definitions in
 let secondAfter = List.find_opt (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.name = "second") (M.functions plan) in
 if secondBefore = secondAfter && List.length (M.rewrites plan) = 1 then Ok () else Error "Established call changed while a neighboring call was specialized"
let testEmptyPlanPreservesProgram () =
 let _, definitions, firstCall, _ = singleFixture () in let* plan = run definitions [request "first" firstCall (S.EstablishedBoundary ordinary)] |> report in
 if M.functions plan = definitions && M.groups plan = [] && M.rewrites plan = [] then Ok () else Error "Established-only materialization changed the program"
let testRejectsUnprovenCallUniqueness () =
 let callee = identity () in let caller, invocation = caller "shared" "identity" 10 false in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in
 match run [callee; caller] [request "shared" invocation chosen] with Error (Materialize.InvalidMaterializedProgram (V.OwnershipVerificationFailed (Ownership.NonUniqueUse id))) when id = (value 10).H.id -> Ok () | actual -> Error ("Expected the caller's actual ownership state to reject unique consumption, got " ^ show actual)
let recursiveFixture self =
 let condition : H.operand = {H.expression = CheckedAST.BoolLiteral true; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty} in
 let recursive name target baseId = let input = value baseId in let recursiveResult = value (baseId + 1) in let result = value (baseId + 2) in let invocation = call target input recursiveResult in
  let body = block [parameter input] [O.Evaluate (H.Branch (result, condition, block [] [O.Evaluate (H.Call invocation)] recursiveResult, block [] [] input))] result in
  definition name (signature [O.ConsumedParameter input.H.id] (O.ProducedResult result.H.id)) body, invocation in
 let first, internalCall = recursive "first" (if self then "first" else "second") 0 in let second, _ = recursive "second" "first" 10 in let targets = if self then [first] else [first; second] in
 let entry, invocation = caller "entry" "first" 20 true in targets, targets @ [entry], internalCall, invocation
let recursivePlan self reverse = let targets, definitions, _, invocation = recursiveFixture self in let targets = if reverse then List.rev targets else targets in
 let* chosen = select targets "first" (O.IntSet.singleton 0) in run definitions [request "entry" invocation chosen] |> report
let testRecursiveAtomicity self () =
 let* plan = recursivePlan self false in match M.groups plan with
 | [group] -> let members = NonEmptyList.toList group.M.members in let symbols = FunctionIdMap.ofList (List.map (fun (memberDefinition : (leaf, H.valueId) M.specializedFunction) -> memberDefinition.M.original, memberDefinition.M.functionDef.O.definition.H.id) members) in
   let contract = Materialize.ownershipSemantics plan semantics in
   let check (memberDefinition : (leaf, H.valueId) M.specializedFunction) = let definition = memberDefinition.M.functionDef in
    let expectedTarget = if self || memberDefinition.M.original = functionId "second" then functionId "first" else functionId "second" in
    match definition.O.definition.H.body.O.body.H.operations with [O.Evaluate (H.Branch (_, _, yes, _))] -> (match yes.O.body.H.operations with [O.Evaluate (H.Call invocation)] -> FunctionIdMap.tryFind expectedTarget symbols = Some invocation.H.target && contract.Ownership.callOwnership invocation = Some unique | _ -> false) | _ -> false in
   if List.length members = (if self then 1 else 2) && List.for_all check members then Ok () else Error "Recursive calls and unique contracts did not move together as one complete group"
 | _ -> Error "Expected one materialized recursive group"
let testDeterministicRecursiveSymbols () = let first = recursivePlan false false in let second = recursivePlan false true in
 let format = function Ok plan -> StructuralValue.Union ("Ok", [svPlan plan]) | Error error -> StructuralValue.Union ("Error", [StructuralValue.Text error]) in
 match first, second with Ok first, Ok second when M.groups first = M.groups second && M.rewrites first = M.rewrites second -> Ok () | actual -> Error ("Expected identical recursive materialization independent of discovery order, got " ^ StructuralFormat.format (StructuralValue.Tuple [format (fst actual); format (snd actual)]))
let testMissingRecursiveMember () = let targets, definitions, _, invocation = recursiveFixture false in let* chosen = select targets "first" (O.IntSet.singleton 0) in
 let incomplete = List.filter (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.name <> "second") definitions in
 match run incomplete [request "entry" invocation chosen] with Error (Materialize.MissingGroupMember "second") -> Ok () | actual -> Error ("Expected a missing recursive member error, got " ^ show actual)
let testRejectsPartialRecursiveSelection () = let targets, definitions, internalCall, _ = recursiveFixture false in let* chosen = select targets "second" (O.IntSet.singleton 0) in
 match run definitions [request "first" internalCall chosen] with Error (Materialize.MixedRecursiveCandidate site) when site.O.caller = functionId "first" -> Ok () | actual -> Error ("Expected a per-edge recursive selection to be rejected, got " ^ show actual)
let testRejectsChangedGroup () = let targets, definitions, _, invocation = recursiveFixture false in let* chosen = select targets "first" (O.IntSet.singleton 0) in
 let changed = List.map (fun (definition : (leaf, H.valueId) O.functionDef) -> if definition.O.definition.H.name = "second" then let input = value 10 in {definition with O.definition = {definition.O.definition with H.body = block [parameter input] [] input}} else definition) definitions in
 match run changed [request "entry" invocation chosen] with Error (Materialize.GroupMembershipMismatch "first") -> Ok () | actual -> Error ("Expected stale SCC membership to be rejected, got " ^ show actual)
let testRejectsChangedBoundary () = let callee, definitions, firstCall, _ = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in
 let changed = List.map (fun (definition : (leaf, H.valueId) O.functionDef) -> if definition.O.definition.H.name = "identity" then {definition with O.ownership = signature [O.BorrowedParameter (value 0).H.id] (O.BorrowedResult (value 0).H.id)} else definition) definitions in
 match run changed [request "first" firstCall chosen] with Error (Materialize.BoundaryMismatch "identity") -> Ok () | actual -> Error ("Expected a changed transfer boundary to be rejected, got " ^ show actual)
let testRejectsCallSiteErrors () = let callee, definitions, firstCall, _ = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in
 let valid = request "first" firstCall chosen in let missing = {valid with M.caller = functionId "missing"} in let stale = {valid with M.call = {firstCall with H.arguments = []}} in
 let first = run definitions [missing] in let second = run definitions [stale] in let third = run definitions [valid; valid] in
 match first, second, third with Error (Materialize.MissingCallSite _), Error (Materialize.StaleCallSite _), Error (Materialize.DuplicateCallSite _) -> Ok () | actual -> Error ("Expected missing, stale and duplicate call sites to fail, got " ^ StructuralFormat.format (StructuralValue.Tuple [svResult (let first, _, _ = actual in first); svResult (let _, second, _ = actual in second); svResult (let _, _, third = actual in third)]))
let testRejectsCollisions () = let callee, definitions, firstCall, _ = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in let requests = [request "first" firstCall chosen] in
 let* plan = run definitions requests |> report in match M.rewrites plan with
 | [rewrite] -> let symbol = rewrite.M.specialized.H.target in let symbolName = (List.find (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.id = symbol) (M.functions plan)).O.definition.H.name in
   let shadow = {callee with O.definition = {callee.O.definition with H.id = symbol; name = symbolName}} in
   let reserved = Materialize.materialize (contracts definitions) semantics (FunctionIdMap.ofList [symbol, symbolName]) definitions requests in let declared = run (definitions @ [shadow]) requests in
   (match reserved, declared with Error (Materialize.SymbolCollision first), Error (Materialize.SymbolCollision second) when first = symbolName && second = symbolName -> Ok () | actual -> Error ("Expected reserved and existing definition collisions to fail, got " ^ showPair actual))
 | _ -> Error "Expected one rewrite"
let testRetainsIndependentContracts () = let callee, definitions, firstCall, _ = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in let source = contracts definitions in
 let* plan = run definitions [request "first" firstCall chosen] |> report in match M.rewrites plan with
 | [rewrite] -> if (M.hirContracts plan source).VerifyOwnedHIR.callContract rewrite.M.specialized = source.VerifyOwnedHIR.callContract rewrite.M.original && (Materialize.ownershipSemantics plan semantics).Ownership.callOwnership rewrite.M.specialized = Some unique then Ok () else Error "Specialization lost the independent effect/alias contract or ownership registration"
 | _ -> Error "Expected one rewrite"
let testMultipleRecursiveCandidates () = let targets, definitions, _, invocation = recursiveFixture false in let other, otherCall = caller "other" "second" 30 false in let definitions = definitions @ [other] in
 let* strong = select targets "first" (O.IntSet.singleton 0) in let* weak = select targets "second" O.IntSet.empty in let requests = [request "entry" invocation strong; request "other" otherCall weak] in
 let first = run definitions requests in let second = run (List.rev definitions) (List.rev requests) in
 match first, second with Ok first, Ok second -> let complete = List.for_all (fun group -> NonEmptyList.length group.M.members = 2) (M.groups first) in
  if List.length (M.groups first) = 2 && complete && M.groups first = M.groups second && M.rewrites first = M.rewrites second then Ok () else Error "Expected two complete, deterministic recursive variants rather than mixed member boundaries"
 | actual -> Error ("Expected both recursive candidates to verify independently, got " ^ showPair actual)
let testNestedExternalCall () =
 let callee = identity () in let input = value 20 in let yesResult = value 21 in let noResult = value 22 in let result = value 23 in let yesCall = call "identity" input yesResult in let noCall = call "identity" input noResult in
 let condition : H.operand = {H.expression = CheckedAST.BoolLiteral false; typ = AST.TBool; inputs = CheckedAST.BindingIdMap.empty} in let no = block [] [O.Evaluate (H.Call noCall)] noResult in
 let branch = definition "branch" (signature [O.UniqueParameter input.H.id] (O.ProducedResult result.H.id)) (block [parameter input] [O.Evaluate (H.Branch (result, condition, block [] [O.Evaluate (H.Call yesCall)] yesResult, no))] result) in
 let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in let* plan = run [callee; branch] [request "branch" yesCall chosen] |> report in
 match List.find_opt (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.name = "branch") (M.functions plan), M.rewrites plan with
 | Some rewritten, [rewrite] -> (match rewritten.O.definition.H.body.O.body.H.operations with [O.Evaluate (H.Branch (_, actualCondition, yes, actualNo))] when actualCondition = condition && actualNo = no && yes.O.body.H.operations = [O.Evaluate (H.Call rewrite.M.specialized)] -> Ok () | _ -> Error "Nested call rewrite changed a condition, the other branch, or its ownership transfer")
 | _ -> Error "Expected exactly one nested call rewrite"
(*
   Balanced local units do not restore provenance after an
   escaping scalar use. The old uniqueness proof is now stale.
*)
let testRejectsStaleBodyProof () = let callee, definitions, firstCall, _ = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in
 let changed = List.map (fun (definition : (leaf, H.valueId) O.functionDef) -> if definition.O.definition.H.name = "identity" then
  let input = value 0 in let ignored : H.value = {H.id = H.ValueId 2; typ = AST.TInt64} in let escape : H.operand = {H.expression = CheckedAST.Int64Literal 0L; typ = AST.TInt64; inputs = CheckedAST.BindingIdMap.of_list [binding "input", input]} in
  let body = block [parameter input] [O.Evaluate (H.ScalarBinding (ignored, escape)); O.Drop ignored.H.id] input in {definition with O.definition = {definition.O.definition with H.body}} else definition) definitions in
 let escaping = {semantics with Ownership.scalarEscapes = semantics.Ownership.scalarUses} in
 match Materialize.materialize (contracts changed) escaping FunctionIdMap.empty changed [request "first" firstCall chosen] with Error (Materialize.InvalidMaterializedProgram (V.OwnershipVerificationFailed (Ownership.NonUniqueUse id))) when id = (value 0).H.id -> Ok () | actual -> Error ("Expected changed body provenance to invalidate the old candidate proof, got " ^ show actual)
let testRejectsRegisteredCollisions () = let callee, definitions, firstCall, _ = singleFixture () in let* chosen = select [callee] "identity" (O.IntSet.singleton 0) in let requests = [request "first" firstCall chosen] in
 let* plan = run definitions requests |> report in match M.rewrites plan with
 | [rewrite] -> let symbol = rewrite.M.specialized.H.target in let symbolName = (List.find (fun (definition : (leaf, H.valueId) O.functionDef) -> definition.O.definition.H.id = symbol) (M.functions plan)).O.definition.H.name in
   let typed = { (contracts definitions) with VerifyOwnedHIR.callSignature = (fun name -> if name = symbol then Some ({H.parameters = [AST.TList AST.TInt64]; result = AST.TList AST.TInt64} : H.functionSignature) else None)} in
   let owned = {semantics with Ownership.callOwnership = (fun call -> if call.H.target = symbol then Some unique else None)} in
   let first = Materialize.materialize typed semantics FunctionIdMap.empty definitions requests in let second = Materialize.materialize (contracts definitions) owned FunctionIdMap.empty definitions requests in
   (match first, second with Error (Materialize.SymbolCollision first), Error (Materialize.SymbolCollision second) when first = symbolName && second = symbolName -> Ok () | actual -> Error ("Expected registered symbol collisions to be rejected, got " ^ showPair actual))
 | _ -> Error "Expected one rewrite"
let testRejectsMismatchedRequests () = let callee, definitions, firstCall, _ = singleFixture () in let other = {callee with O.definition = {callee.O.definition with H.id = functionId "other"; name = "other"}} in
 let* chosen = select [other] "other" (O.IntSet.singleton 0) in let first = run (definitions @ [other]) [request "first" firstCall chosen] in let second = run definitions [request "first" firstCall (S.EstablishedBoundary unique)] in let third = run (callee :: definitions) [] in
 match first, second, third with Error (Materialize.BoundaryMismatch "identity"), Error (Materialize.BoundaryMismatch "identity"), Error (Materialize.GroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id)) when id = functionId "identity" -> Ok () | actual -> Error ("Expected wrong target, established boundary and duplicate definition errors, got " ^ StructuralFormat.format (StructuralValue.Tuple [svResult (let first, _, _ = actual in first); svResult (let _, second, _ = actual in second); svResult (let _, _, third = actual in third)]))
let tests = [
 "Materialization deduplicates candidates and verifies all rewritten callers", testDeduplicatesAndVerifies;
 "Materialization preserves established calls", testPreservesEstablishedCalls;
 "Materialization leaves established-only programs unchanged", testEmptyPlanPreservesProgram;
 "Materialization rejects false caller uniqueness facts", testRejectsUnprovenCallUniqueness;
 "Materialization transfers self-recursive contracts atomically", testRecursiveAtomicity true;
 "Materialization transfers mutual-recursive contracts atomically", testRecursiveAtomicity false;
 "Materialization uses deterministic recursive symbols", testDeterministicRecursiveSymbols;
 "Materialization rejects missing recursive members", testMissingRecursiveMember;
 "Materialization rejects mixed recursive candidates", testRejectsPartialRecursiveSelection;
 "Materialization rejects changed SCC membership", testRejectsChangedGroup;
 "Materialization rejects changed ownership boundaries", testRejectsChangedBoundary;
 "Materialization rejects invalid call-site requests", testRejectsCallSiteErrors;
 "Materialization rejects symbol collisions", testRejectsCollisions;
 "Materialization preserves independent call contracts", testRetainsIndependentContracts;
 "Materialization separates complete recursive candidates deterministically", testMultipleRecursiveCandidates;
 "Materialization rewrites nested calls while preserving branch ownership", testNestedExternalCall;
 "Materialization rechecks stale candidate body proofs", testRejectsStaleBodyProof;
 "Materialization rejects registered symbol collisions", testRejectsRegisteredCollisions;
 "Materialization rejects mismatched request boundaries", testRejectsMismatchedRequests
]
