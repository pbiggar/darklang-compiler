(* SelectOwnershipVariants.ml - Choose inferred ownership variants at direct call sites. *)
[@@@warning "-4"]
module O = OwnedIR
module G = InferOwnedFunctionGroups
module M = StringOrder.Map
module S = O.IntSet
let ( let* ) = Result.bind
(*
   Canonical group boundaries exclude callee-local identities and discovery
   order so equivalent requests share one materialization/cache identity.
*)
type candidateIdentity = CandidateIdentity of (string * O.callSignature) NonEmptyList.t
type 'id catalog = Catalog of 'id G.group M.t
type callSite = {target : string; established : O.callSignature; uniqueArguments : S.t}
type 'id selectedVariant = {identity : candidateIdentity; candidate : 'id G.candidate; targetBoundary : 'id G.functionBoundary; callSignature : O.callSignature}
type 'id selection = EstablishedBoundary of O.callSignature | InferredVariant of 'id selectedVariant
type selectionError = DuplicateFunctionName of string | UnknownFunction of string | InvalidUniqueArgumentIndex of string * int | MissingEstablishedUniqueArgument of string * int | InconsistentEstablishedBoundary of string
let selectedIdentity selected = selected.identity
let selectedCandidate selected = selected.candidate
let selectedTargetBoundary selected = selected.targetBoundary
let selectedCallSignature selected = selected.callSignature
let identityBoundaries (CandidateIdentity boundaries) = NonEmptyList.toList boundaries
let nonEmpty context values = match NonEmptyList.tryFromList values with Some values -> values | None -> Crash.crash context
let compareCallSignature (first : O.callSignature) (second : O.callSignature) =
 let parameter = function O.UnmanagedCallParameter -> 0 | O.BorrowedCallParameter -> 1 | O.ConsumedCallParameter -> 2 | O.UniqueCallParameter -> 3 in
 let result = function O.UnmanagedCallResult -> 0, 0 | O.BorrowedCallResult index -> 1, index | O.ProducedCallResult -> 2, 0 | O.UniqueProducedCallResult -> 3, 0 in
 let rec parameters first second = match first, second with [], [] -> 0 | [], _ -> -1 | _, [] -> 1 | first :: rest, second :: other -> let order = Int.compare (parameter first) (parameter second) in if order <> 0 then order else parameters rest other in
 let order = parameters first.O.parameters second.O.parameters in if order <> 0 then order else Stdlib.compare (result first.O.result) (result second.O.result)
let compareCandidateIdentity first second =
 let rec compare first second = match first, second with
 | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
 | (name, signature) :: rest, (otherName, otherSignature) :: other ->
   let order = StringOrder.compare name otherName in if order <> 0 then order else
   let order = compareCallSignature signature otherSignature in if order <> 0 then order else compare rest other in
 compare (identityBoundaries first) (identityBoundaries second)
let firstCandidate group = match G.candidates group with head :: _ -> head | [] -> Crash.crash "Inferred ownership group has no candidates"
let functionNames group = List.map (fun (boundary : 'id G.functionBoundary) -> boundary.InferRecursiveOwnership.name) (G.candidateBoundaries (firstCandidate group))
(*
   Index inferred groups by every function they contain. A recursive group maps
   each member name back to the same group, so later selection always returns a
   complete group candidate.
*)
let create groups =
 let addName group result name = let* catalog = result in match M.find_opt name catalog with Some _ -> Error (DuplicateFunctionName name) | None -> Ok (M.add name group catalog) in
 let* catalog = List.fold_left (fun result group -> List.fold_left (addName group) result (functionNames group)) (Ok M.empty) groups in Ok (Catalog catalog)
type parameterTransfer = UnmanagedTransfer | BorrowedTransfer | ConsumedTransfer
type resultTransfer = UnmanagedReturn | BorrowedReturn of int | ProducedReturn
let parameterTransfer = function O.UnmanagedCallParameter -> UnmanagedTransfer | O.BorrowedCallParameter -> BorrowedTransfer | O.ConsumedCallParameter | O.UniqueCallParameter -> ConsumedTransfer
let resultTransfer = function O.UnmanagedCallResult -> UnmanagedReturn | O.BorrowedCallResult index -> BorrowedReturn index | O.ProducedCallResult | O.UniqueProducedCallResult -> ProducedReturn
let rec sameParameterTransfers first second = match first, second with [], [] -> true | first :: rest, second :: other -> parameterTransfer first = parameterTransfer second && sameParameterTransfers rest other | _ -> false
let sameTransferShape (first : O.callSignature) (second : O.callSignature) = sameParameterTransfers first.O.parameters second.O.parameters && resultTransfer first.O.result = resultTransfer second.O.result
let preservesEstablishedResult (established : O.callSignature) (candidate : O.callSignature) = match established.O.result, candidate.O.result with
 | O.UniqueProducedCallResult, O.UniqueProducedCallResult -> true
 | O.UniqueProducedCallResult, _ -> false
 | O.UnmanagedCallResult, O.UnmanagedCallResult | O.BorrowedCallResult _, O.BorrowedCallResult _ | O.ProducedCallResult, O.ProducedCallResult | O.ProducedCallResult, O.UniqueProducedCallResult -> true
 | _ -> false
let requiredUniqueArguments (signature : O.callSignature) = S.of_list (List.mapi (fun index ownership -> match ownership with O.UniqueCallParameter -> Some index | _ -> None) signature.O.parameters |> List.filter_map Fun.id)
type 'id applicableCandidate = {identity : candidateIdentity; candidate : 'id G.candidate; boundary : 'id G.functionBoundary; signature : O.callSignature; requiredUniqueArguments : S.t}
let resultPreference (signature : O.callSignature) = match signature.O.result with O.UniqueProducedCallResult -> 0 | O.UnmanagedCallResult | O.BorrowedCallResult _ | O.ProducedCallResult -> 1
let preference (candidate : 'id applicableCandidate) = resultPreference candidate.signature, S.cardinal candidate.requiredUniqueArguments, candidate.identity
let comparePreference first second =
 let result, count, identity = preference first in let otherResult, otherCount, otherIdentity = preference second in
 let order = Int.compare result otherResult in if order <> 0 then order else let order = Int.compare count otherCount in if order <> 0 then order else compareCandidateIdentity identity otherIdentity
module Make (Identity : O.Identity) = struct
 module Verify = VerifyOwnership.Make (Identity)
 let callSignature (boundary : Identity.t G.functionBoundary) = match Verify.callSignatureOfFunction boundary.InferRecursiveOwnership.ownership with Ok signature -> signature | Error _ -> Crash.crash "Verifier-proven ownership candidate has an invalid call boundary"
 let candidateIdentity candidate = CandidateIdentity (nonEmpty "Ownership variant candidate has no function boundaries"
  (List.map (fun (boundary : Identity.t G.functionBoundary) -> boundary.InferRecursiveOwnership.name, callSignature boundary) (G.candidateBoundaries candidate) |> List.stable_sort (fun (first, _) (second, _) -> StringOrder.compare first second)))
 let targetCandidate target candidate =
  match List.find_opt (fun (boundary : Identity.t G.functionBoundary) -> boundary.InferRecursiveOwnership.name = target) (G.candidateBoundaries candidate) with
  | None -> Crash.crash "Inferred ownership candidates disagree on their function group"
  | Some boundary -> let signature = callSignature boundary in {identity = candidateIdentity candidate; candidate; boundary; signature; requiredUniqueArguments = requiredUniqueArguments signature}
(*
   Select the best inferred group candidate applicable to the call's proven
   unique arguments. Uniqueness may strengthen a consumed parameter or produced
   result, but the established borrow/consume/produce shape cannot change.
   When no inferred candidate applies, the established verified boundary is
   retained. Recursive candidates remain atomic in `SelectedVariant.Candidate`.
*)
 let select (Catalog catalog) site = match M.find_opt site.target catalog with
 | None -> Error (UnknownFunction site.target)
 | Some group ->
   let parameterCount = List.length site.established.O.parameters in
   match List.find_opt (fun index -> index < 0 || index >= parameterCount) (S.elements site.uniqueArguments) with
   | Some index -> Error (InvalidUniqueArgumentIndex (site.target, index))
   | None -> match S.elements (S.diff (requiredUniqueArguments site.established) site.uniqueArguments) with
     | index :: _ -> Error (MissingEstablishedUniqueArgument (site.target, index))
     | [] ->
       let candidates = List.map (targetCandidate site.target) (G.candidates group) in
       match candidates with
       | head :: _ when not (sameTransferShape site.established head.signature) -> Error (InconsistentEstablishedBoundary site.target)
       | [] -> Crash.crash "Inferred ownership group has no candidates"
       | _ ->
         let applicable = List.filter (fun candidate -> sameTransferShape site.established candidate.signature && preservesEstablishedResult site.established candidate.signature && S.subset candidate.requiredUniqueArguments site.uniqueArguments) candidates |> List.stable_sort comparePreference in
         match applicable with [] -> Ok (EstablishedBoundary site.established) | selected :: _ -> Ok (InferredVariant {identity = selected.identity; candidate = selected.candidate; targetBoundary = selected.boundary; callSignature = selected.signature})
end
