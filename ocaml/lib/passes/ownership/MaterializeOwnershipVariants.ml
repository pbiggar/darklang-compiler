(* MaterializeOwnershipVariants.fs - Clone verified ownership candidates and route selected calls. *)
[@@@warning "-4"]
module H = HIR
module O = OwnedIR
module S = SelectOwnershipVariants
module G = InferOwnedFunctionGroups
module R = InferRecursiveOwnership
module F = FunctionIdMap
module M = StringOrder.Map
module FS = SpecializationIdentity.FunctionSet
let ( let* ) = Result.bind
type 'id request = {caller : AST.functionId; call : H.functionCall; selection : 'id S.selection}
type ('leaf, 'id) specializedFunction = {original : AST.functionId; functionDef : ('leaf, 'id) O.functionDef}
type ('leaf, 'id) specializedGroup = {identity : S.candidateIdentity; members : ('leaf, 'id) specializedFunction NonEmptyList.t}
type callRewrite = {site : O.callSiteIdentity; original : H.functionCall; specialized : H.functionCall; ownership : O.callSignature}
(*
   Original definitions and clones have one authoritative home in the plan.
   Registries are derived from them, rather than retained as mutable caches.
*)
type ('leaf, 'id) plan = {originals : ('leaf, 'id) O.functionDef list; groups : ('leaf, 'id) specializedGroup list; rewrites : callRewrite list}
let groups plan = plan.groups
let rewrites plan = plan.rewrites
let originals plan = plan.originals
let functions plan = plan.originals @ List.concat_map (fun group -> List.map (fun memberDefinition -> memberDefinition.functionDef) (NonEmptyList.toList group.members)) plan.groups
let members plan = List.concat_map (fun group -> NonEmptyList.toList group.members) plan.groups
let typedSignature (definition : ('leaf, 'id) O.functionDef) : H.functionSignature = {H.parameters = List.map (fun (parameter : H.parameter) -> parameter.H.value.H.typ) definition.O.definition.H.body.O.body.H.parameters; result = definition.O.definition.H.body.O.body.H.result.H.typ}
let boundaryCall (definition : ('leaf, 'id) O.functionDef) : H.functionCall = {H.target = definition.O.definition.H.id; arguments = List.map (fun (parameter : H.parameter) -> parameter.H.value) definition.O.definition.H.body.O.body.H.parameters; result = definition.O.definition.H.body.O.body.H.result}
(*
   Clone effects and aliases come from the source function's independent HIR
   contract, instantiated with the actual call operands. Ownership is never
   used to invent either fact.
*)
let hirContracts plan (source : 'leaf VerifyOwnedHIR.hirContracts) =
 let registry = F.ofList (List.map (fun memberDefinition -> memberDefinition.functionDef.O.definition.H.id, memberDefinition) (members plan)) in
 {source with VerifyOwnedHIR.callSignature = (fun target -> match F.tryFind target registry with Some memberDefinition -> Some (typedSignature memberDefinition.functionDef) | None -> source.VerifyOwnedHIR.callSignature target);
  callContract = (fun call -> match F.tryFind call.H.target registry with Some memberDefinition -> source.VerifyOwnedHIR.callContract {call with H.target = memberDefinition.original} | None -> source.VerifyOwnedHIR.callContract call)}
(*
   A versioned, length-delimited encoding and full digest make names stable
   across processes, cultures, request order, and callee-local identities.
*)
let symbolSuffix identity =
 let field value = string_of_int (Array.length (HostText.utf16Units value)) ^ ":" ^ value in
 let parameter = function O.UnmanagedCallParameter -> "u" | O.BorrowedCallParameter -> "b" | O.ConsumedCallParameter -> "c" | O.UniqueCallParameter -> "q" in
 let result = function O.UnmanagedCallResult -> "u" | O.BorrowedCallResult index -> "b" ^ string_of_int index | O.ProducedCallResult -> "p" | O.UniqueProducedCallResult -> "q" in
 let encoded = S.identityBoundaries identity |> List.map (fun (name, (signature : O.callSignature)) ->
  let parameters = String.concat "" (List.map parameter signature.O.parameters) in field (field name ^ field parameters ^ field (result signature.O.result))) |> String.concat "" in
 "__ownership_" ^ HostHash.sha256Utf8 ("ownership-v1:" ^ encoded)
let rec calls (block : ('leaf, 'id) O.block) = List.concat_map (function
 | O.Evaluate (H.Call call) -> [call]
 | O.Evaluate (H.Branch (_, _, yes, no)) -> let yes = calls yes in let no = calls no in yes @ no
 | O.Evaluate (H.Leaf _ | H.ScalarBinding _) | O.Dup _ | O.Drop _ -> []) block.O.body.H.operations
let rec rewriteCalls rewrite (block : ('leaf, 'id) O.block) =
 let operations = List.map (function O.Evaluate (H.Call call) -> O.Evaluate (H.Call (rewrite call))
  | O.Evaluate (H.Branch (result, condition, yes, no)) -> let yes = rewriteCalls rewrite yes in let no = rewriteCalls rewrite no in O.Evaluate (H.Branch (result, condition, yes, no)) | step -> step) block.O.body.H.operations in
 {O.body = {block.O.body with H.operations}}
let site (request : 'id request) : O.callSiteIdentity = {O.caller = request.caller; result = request.call.H.result.H.id}
let compareFunction first second = Int64.unsigned_compare (AST.functionIdValue first) (AST.functionIdValue second)
module SiteOrder = struct type t = O.callSiteIdentity let compare (first : t) (second : t) = let order = compareFunction first.O.caller second.O.caller in if order <> 0 then order else let H.ValueId first = first.O.result in let H.ValueId second = second.O.result in Int.compare first second end
module SiteMap = Map.Make (SiteOrder)
module SiteSet = Set.Make (SiteOrder)
module IdentityMap = Map.Make (struct type t = S.candidateIdentity let compare = S.compareCandidateIdentity end)
module TargetMap = Map.Make (struct type t = S.candidateIdentity * AST.functionId let compare (first, target) (second, other) = let order = S.compareCandidateIdentity first second in if order <> 0 then order else compareFunction target other end)
let nameById target definitions = match F.tryFind target definitions with Some (definition : ('leaf, 'id) O.functionDef) -> definition.O.definition.H.name | None -> let value = AST.functionIdValue target in "function#" ^ Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value)
module Make (Identity : O.Identity) = struct
 module Verification = VerifyOwnedHIR.Make (Identity)
 module Verify = VerifyOwnership.Make (Identity)
 module Ownership = Verify.Ownership
 type materializationError = GroupingFailed of OwnedFunctionGroups.groupingError | InvalidOriginalProgram of Verification.verificationError | MissingGroupMember of string | GroupMembershipMismatch of string | BoundaryMismatch of string | MissingCallSite of O.callSiteIdentity | DuplicateCallSite of O.callSiteIdentity | StaleCallSite of O.callSiteIdentity | MixedRecursiveCandidate of O.callSiteIdentity | SymbolCollision of string | InvalidMaterializedProgram of Verification.verificationError
 let verifiedCallSignature boundary = match Verify.callSignatureOfFunction boundary with Ok signature -> signature | Error _ -> Crash.crash "Materialized ownership boundary has no valid call signature"
 let ownershipSemantics plan (source : 'leaf Ownership.semantics) =
  let registry = F.ofList (List.map (fun memberDefinition -> memberDefinition.functionDef.O.definition.H.id, verifiedCallSignature memberDefinition.functionDef.O.ownership) (members plan)) in
  {source with Ownership.callOwnership = (fun call -> match F.tryFind call.H.target registry with Some signature -> Some signature | None -> source.Ownership.callOwnership call)}
 let equal first second = Identity.compare first second = 0
 let sameParameter first second = match first, second with O.UnmanagedParameter, O.UnmanagedParameter -> true
  | O.BorrowedParameter first, O.BorrowedParameter second | O.ConsumedParameter first, O.ConsumedParameter second | O.UniqueParameter first, O.UniqueParameter second -> equal first second | _ -> false
 let sameResult first second = match first, second with O.UnmanagedResult, O.UnmanagedResult -> true
  | O.BorrowedResult first, O.BorrowedResult second | O.ProducedResult first, O.ProducedResult second | O.UniqueProducedResult first, O.UniqueProducedResult second -> equal first second | _ -> false
 let refines (original : Identity.t O.functionSignature) (candidate : Identity.t O.functionSignature) =
  let parameter first second = sameParameter first second || match first, second with O.ConsumedParameter first, O.UniqueParameter second -> equal first second | _ -> false in
  let rec parameters first second = match first, second with [], [] -> true | first :: rest, second :: other -> parameter first second && parameters rest other | _ -> false in
  parameters original.O.parameters candidate.O.parameters && (sameResult original.O.result candidate.O.result || match original.O.result, candidate.O.result with O.ProducedResult first, O.UniqueProducedResult second -> equal first second | _ -> false)
 let validateRequests definitions discovered semantics requests =
  let definitionsByName = M.of_list (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.name, definition) definitions) in
  let definitionsById = F.ofList (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id, definition) definitions) in
  let groupNames = F.ofList (List.concat_map (fun group -> let members = OwnedFunctionGroups.functions group in let ids = FS.of_list (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id) members) in List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id, ids) members) discovered) in
  let callSites = SiteMap.of_list (List.concat_map (fun (definition : ('leaf, Identity.t) O.functionDef) -> List.map (fun (call : H.functionCall) -> ({O.caller = definition.O.definition.H.id; result = call.H.result.H.id} : O.callSiteIdentity), call) (calls definition.O.definition.H.body)) definitions) in
  let validateCandidate request selected =
   let target = S.selectedTargetBoundary selected in let requestTargetName = nameById request.call.H.target definitionsById in
   let boundaries = G.candidateBoundaries (S.selectedCandidate selected) in
   let* () = List.fold_left (fun result (boundary : Identity.t G.functionBoundary) -> let* () = result in match M.find_opt boundary.R.name definitionsByName with
    | None -> Error (MissingGroupMember boundary.R.name) | Some original when not (refines original.O.ownership boundary.R.ownership) -> Error (BoundaryMismatch boundary.R.name) | Some _ -> Ok ()) (Ok ()) boundaries in
   let names = StringOrder.Set.of_list (List.map (fun (boundary : Identity.t G.functionBoundary) -> boundary.R.name) boundaries) in
   let ids = FS.of_list (List.filter_map (fun name -> Option.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id) (M.find_opt name definitionsByName)) (StringOrder.Set.elements names)) in
   let targetId = Option.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id) (M.find_opt target.R.name definitionsByName) in
   if targetId <> Some request.call.H.target then Error (BoundaryMismatch requestTargetName)
   else if not (match F.tryFind request.call.H.target groupNames with Some members -> FS.equal members ids | None -> false) then Error (GroupMembershipMismatch target.R.name)
   else if FS.mem request.caller ids then Error (MixedRecursiveCandidate (site request)) else Ok () in
  let* _ = List.fold_left (fun result request -> let* seen = result in let callSite = site request in
   if SiteSet.mem callSite seen then Error (DuplicateCallSite callSite) else match SiteMap.find_opt callSite callSites with
   | None -> Error (MissingCallSite callSite) | Some actual when actual <> request.call -> Error (StaleCallSite callSite)
   | Some _ ->
     let valid = match request.selection with S.InferredVariant selected -> validateCandidate request selected
     | S.EstablishedBoundary expected ->
       let actual = match F.tryFind request.call.H.target definitionsById with Some definition -> Verify.callSignatureOfFunction definition.O.ownership |> Result.map Option.some | None -> Ok (semantics.Ownership.callOwnership request.call) in
       match actual with Ok (Some actual) when actual = expected -> Ok () | _ -> Error (BoundaryMismatch (nameById request.call.H.target definitionsById)) in
     let* () = valid in Ok (SiteSet.add callSite seen)) (Ok SiteSet.empty) requests in Ok ()
 let cloneGroups hir semantics reservedFunctions definitions requests =
  let definitionsByName = M.of_list (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.name, definition) definitions) in
  let definitionsById = F.ofList (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id, definition) definitions) in
  let selections = IdentityMap.of_list (List.filter_map (fun request -> match request.selection with S.EstablishedBoundary _ -> None | S.InferredVariant selected -> Some (S.selectedIdentity selected, S.selectedCandidate selected)) requests) in
  let cloneNames = List.concat_map (fun (identity, candidate) -> let suffix = symbolSuffix identity in List.map (fun (boundary : Identity.t G.functionBoundary) -> boundary.R.name ^ suffix) (G.candidateBoundaries candidate)) (IdentityMap.bindings selections) in
  let cloneIds = AST.allocateFunctionIds (Seq.append (F.keys reservedFunctions) (List.to_seq (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.H.id) definitions))) (List.to_seq cloneNames) in
  let* _, groups = List.fold_left (fun result (identity, candidate) -> let* generatedIds, groups = result in
   let suffix = symbolSuffix identity in let boundaries = List.stable_sort (fun (first : Identity.t G.functionBoundary) second -> StringOrder.compare first.R.name second.R.name) (G.candidateBoundaries candidate) in
   let symbols = F.ofList (List.map (fun (boundary : Identity.t G.functionBoundary) ->
    let originalId = match M.find_opt boundary.R.name definitionsByName with Some definition -> definition.O.definition.H.id | None -> Crash.crash "Ownership boundary definition is absent" in originalId, M.find (boundary.R.name ^ suffix) cloneIds) boundaries) in
   let rewrite (call : H.functionCall) = match F.tryFind call.H.target symbols with Some symbol -> {call with H.target = symbol} | None -> call in
   let* generatedIds, members = List.fold_left (fun result (boundary : Identity.t G.functionBoundary) -> let* generatedIds, members = result in
    match M.find_opt boundary.R.name definitionsByName with None -> Error (MissingGroupMember boundary.R.name) | Some original ->
     let name = boundary.R.name ^ suffix in let cloneId = M.find name cloneIds in
     let clone = {O.ownership = boundary.R.ownership; definition = {H.id = cloneId; name; body = rewriteCalls rewrite original.O.definition.H.body}} in
     if M.mem name definitionsByName || F.exists (fun _ reservedName -> reservedName = name) reservedFunctions || F.containsKey cloneId reservedFunctions || F.containsKey cloneId definitionsById || FS.mem cloneId generatedIds || Option.is_some (hir.VerifyOwnedHIR.callSignature clone.O.definition.H.id) || Option.is_some (semantics.Ownership.callOwnership (boundaryCall clone)) then Error (SymbolCollision name)
     else Ok (FS.add cloneId generatedIds, {original = original.O.definition.H.id; functionDef = clone} :: members)) (Ok (generatedIds, [])) boundaries in
   let members = match NonEmptyList.tryFromList (List.rev members) with Some members -> members | None -> Crash.crash "Selected ownership candidate has no members" in
   Ok (generatedIds, {identity; members} :: groups)) (Ok (FS.empty, [])) (IdentityMap.bindings selections) in Ok (List.rev groups)
(*
   Requests address calls in the supplied original definitions. A recursive
   edge cannot be selected independently: clones route every internal edge
   through their complete candidate, and keep outside calls established.
   The caller reserves all other symbols in the compilation unit. No changes
   escape this pass unless the complete materialized program verifies.
*)
 let materializeWithRequests hir semantics reservedFunctions definitions requests =
  let* discovered = OwnedFunctionGroups.discover definitions |> Result.map_error (fun error -> GroupingFailed error) in
  let* () = validateRequests definitions discovered semantics requests in
  let* () = Verification.verifyFunctions hir semantics definitions |> Result.map_error (fun error -> InvalidOriginalProgram error) in
  let* groups = cloneGroups hir semantics reservedFunctions definitions requests in
  let targets = TargetMap.of_list (List.concat_map (fun group -> List.map (fun (memberDefinition : (_, _) specializedFunction) -> (group.identity, memberDefinition.original), memberDefinition.functionDef.O.definition.H.id) (NonEmptyList.toList group.members)) groups) in
  let rewrites = List.filter_map (fun request -> match request.selection with S.EstablishedBoundary _ -> None | S.InferredVariant selected ->
   let target = match TargetMap.find_opt (S.selectedIdentity selected, request.call.H.target) targets with Some target -> target | None -> Crash.crash "Validated ownership selection has no materialized target" in
   Some {site = site request; original = request.call; specialized = {request.call with H.target}; ownership = S.selectedCallSignature selected}) requests |> List.stable_sort (fun first second -> SiteOrder.compare first.site second.site) in
  let bySite = SiteMap.of_list (List.map (fun rewrite -> rewrite.site, rewrite.specialized) rewrites) in
  let rewrittenCallers = FS.of_list (List.map (fun rewrite -> rewrite.site.O.caller) rewrites) in
  let originals = List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> if FS.mem definition.O.definition.H.id rewrittenCallers then
   let rewrite (call : H.functionCall) = match SiteMap.find_opt {O.caller = definition.O.definition.H.id; result = call.H.result.H.id} bySite with Some specialized -> specialized | None -> call in
   {definition with O.definition = {definition.O.definition with H.body = rewriteCalls rewrite definition.O.definition.H.body}} else definition) definitions in
  let plan = {originals; groups; rewrites} in
  let* () = Verification.verifyFunctions (hirContracts plan hir) (ownershipSemantics plan semantics) (functions plan) |> Result.map_error (fun error -> InvalidMaterializedProgram error) in Ok plan
(*
   Empty demand keeps the original program and its validation boundary without
   rebuilding and re-verifying an identical derived program.
*)
 let materialize hir semantics reservedFunctions definitions requests =
  if requests = [] then
   let* _ = OwnedFunctionGroups.discover definitions |> Result.map_error (fun error -> GroupingFailed error) in
   let* () = Verification.verifyFunctions hir semantics definitions |> Result.map_error (fun error -> InvalidOriginalProgram error) in Ok {originals = definitions; groups = []; rewrites = []}
  else materializeWithRequests hir semantics reservedFunctions definitions requests
end
