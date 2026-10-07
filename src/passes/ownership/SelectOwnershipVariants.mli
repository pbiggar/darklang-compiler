(* SelectOwnershipVariants.mli - Choose inferred ownership variants at direct call sites. *)
type candidateIdentity
type 'id catalog

type callSite = {
  target : string;
  established : OwnedIR.callSignature;
  uniqueArguments : OwnedIR.IntSet.t;
}

type 'id selectedVariant

type 'id selection =
  | EstablishedBoundary of OwnedIR.callSignature
  | InferredVariant of 'id selectedVariant

type selectionError =
  | DuplicateFunctionName of string
  | UnknownFunction of string
  | InvalidUniqueArgumentIndex of string * int
  | MissingEstablishedUniqueArgument of string * int
  | InconsistentEstablishedBoundary of string

val selectedIdentity : 'id selectedVariant -> candidateIdentity

val selectedCandidate :
  'id selectedVariant -> 'id InferOwnedFunctionGroups.candidate

val selectedTargetBoundary :
  'id selectedVariant -> 'id InferOwnedFunctionGroups.functionBoundary

val selectedCallSignature : 'id selectedVariant -> OwnedIR.callSignature

val identityBoundaries :
  candidateIdentity -> (string * OwnedIR.callSignature) list

val compareCandidateIdentity : candidateIdentity -> candidateIdentity -> int

val create :
  'id InferOwnedFunctionGroups.group list ->
  ('id catalog, selectionError) result

module Make (Identity : OwnedIR.Identity) : sig
  val select :
    Identity.t catalog ->
    callSite ->
    (Identity.t selection, selectionError) result
end
