(* InferOwnershipUniqueness.fs - Derive verifier-proven uniqueness boundary variants. *)
val maximumVariants : int
val refinableModeCount : 'id OwnedIR.functionSignature -> int
val withinVariantLimit : int -> bool
val signatureSequence : 'id OwnedIR.functionSignature -> 'id OwnedIR.functionSignature Seq.t
val signatures : 'id OwnedIR.functionSignature -> 'id OwnedIR.functionSignature list
val demandedSignatures : OwnedIR.IntSet.t -> 'id OwnedIR.functionSignature -> 'id OwnedIR.functionSignature Seq.t
val boundaryRelation : 'id OwnedIR.functionSignature -> 'id OwnedIR.functionSignature -> bool * bool
module Make (Identity : OwnedIR.Identity) : sig
 module Ownership : module type of OwnedIR.Make (Identity)
 type candidates
 type inferenceError = VariantLimitExceeded of int * int | RecursiveFunctionRequiresGroupInference of AST.functionId | NoVerifiedBoundary of Ownership.verificationError | NoVerifiedFunctionGroup of Ownership.verificationError
 val toList : candidates -> Identity.t OwnedIR.functionSignature list
 val infer : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef -> (candidates, inferenceError) result
 val inferDemand : 'leaf Ownership.semantics -> OwnedIR.IntSet.t -> ('leaf, Identity.t) OwnedIR.functionDef -> (Identity.t OwnedIR.functionSignature option, inferenceError) result
end
