(* InferOwnedFunctionGroups.mli - Infer uniqueness variants for owned-HIR call groups. *)
type 'id functionBoundary = 'id InferRecursiveOwnership.functionBoundary
type 'id candidate
type 'id group
type ('leaf, 'id) program
val candidateBoundaries : 'id candidate -> 'id functionBoundary list
val candidates : 'id group -> 'id candidate list
val isRecursive : 'id group -> bool
val internalDependencies : 'id group -> SpecializationIdentity.FunctionSet.t
val externalTargets : 'id group -> SpecializationIdentity.FunctionSet.t
val isInternalRecursiveCall : ('leaf, 'id) program -> AST.functionId -> AST.functionId -> bool
module Make (Identity : OwnedIR.Identity) : sig
 module Ownership : module type of OwnedIR.Make (Identity)
 module Uniqueness : module type of InferOwnershipUniqueness.Make (Identity)
 type inferenceError = FunctionGroupingFailed of OwnedFunctionGroups.groupingError | DemandTargetMissing of AST.functionId | GroupInferenceFailed of string AST.nonEmptyList * Uniqueness.inferenceError
 val prepareWithTrace : (string -> float -> unit) option -> ('leaf, Identity.t) OwnedIR.functionDef list -> (('leaf, Identity.t) program, inferenceError) result
 val prepare : ('leaf, Identity.t) OwnedIR.functionDef list -> (('leaf, Identity.t) program, inferenceError) result
 val inferDemandWithTrace : (string -> float -> unit) option -> 'leaf Ownership.semantics -> ('leaf, Identity.t) program -> AST.functionId -> OwnedIR.IntSet.t -> (Identity.t group option, inferenceError) result
 val inferDemand : 'leaf Ownership.semantics -> ('leaf, Identity.t) program -> AST.functionId -> OwnedIR.IntSet.t -> (Identity.t group option, inferenceError) result
 val inferWithTrace : (string -> float -> unit) option -> 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef list -> (Identity.t group list, inferenceError) result
 val infer : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef list -> (Identity.t group list, inferenceError) result
end
