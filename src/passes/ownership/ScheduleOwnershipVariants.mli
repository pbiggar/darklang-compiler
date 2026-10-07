(* ScheduleOwnershipVariants.mli - Drive verified ownership specialization to a bounded fixed point. *)
type limits = {maxIterations : int; maxGeneratedGroups : int; maxRewrittenCalls : int}
val defaultLimits : limits
type iteration = {number : int; addedCalls : OwnedIR.callSiteIdentity list; generatedGroups : int}
type ('leaf, 'id) cacheDescriptor = {identity : SelectOwnershipVariants.candidateIdentity; sources : ('leaf, 'id) OwnedIR.functionDef list; boundaries : (string * OwnedIR.callSignature) list; internalDependencies : SpecializationIdentity.FunctionSet.t; externalTargets : SpecializationIdentity.FunctionSet.t}
type ('leaf, 'id) plan
val materialization : ('leaf, 'id) plan -> ('leaf, 'id) MaterializeOwnershipVariants.plan
val functions : ('leaf, 'id) plan -> ('leaf, 'id) OwnedIR.functionDef list
val hirContracts : ('leaf, 'id) plan -> 'leaf VerifyOwnedHIR.hirContracts -> 'leaf VerifyOwnedHIR.hirContracts
val iterations : ('leaf, 'id) plan -> iteration list
val cacheDescriptors : ('leaf, 'id) plan -> ('leaf, 'id) cacheDescriptor list
module Make (Identity : OwnedIR.Identity) : sig
 module Ownership : module type of OwnedIR.Make (Identity)
 module Inference : module type of InferOwnedFunctionGroups.Make (Identity)
 module Verification : module type of VerifyOwnedHIR.Make (Identity)
 module Materialize : module type of MaterializeOwnershipVariants.Make (Identity)
 type schedulingError = InvalidLimits of limits | InvalidFunctionBoundary of AST.functionId * Ownership.verificationError | InferenceFailed of Inference.inferenceError | CatalogFailed of SelectOwnershipVariants.selectionError | AnalysisFailed of Verification.verificationError | SelectionFailed of OwnedIR.callSiteIdentity * SelectOwnershipVariants.selectionError | MaterializationFailed of Materialize.materializationError | IterationLimitExceeded of int | GeneratedGroupLimitExceeded of int | RewrittenCallLimitExceeded of int | MissingOriginalCall of OwnedIR.callSiteIdentity
 val ownershipSemantics : ('leaf, Identity.t) plan -> 'leaf Ownership.semantics -> 'leaf Ownership.semantics
 val scheduleWithTrace : (string -> float -> unit) option -> limits -> 'leaf VerifyOwnedHIR.hirContracts -> 'leaf Ownership.semantics -> string FunctionIdMap.t -> ('leaf, Identity.t) OwnedIR.functionDef list -> (('leaf, Identity.t) plan, schedulingError) result
 val schedule : limits -> 'leaf VerifyOwnedHIR.hirContracts -> 'leaf Ownership.semantics -> string FunctionIdMap.t -> ('leaf, Identity.t) OwnedIR.functionDef list -> (('leaf, Identity.t) plan, schedulingError) result
end
