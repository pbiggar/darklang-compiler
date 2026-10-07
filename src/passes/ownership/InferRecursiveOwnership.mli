(* InferRecursiveOwnership.mli - Solve uniqueness boundaries across visible function groups. *)
type 'id functionBoundary = {id : AST.functionId; name : string; ownership : 'id OwnedIR.functionSignature}
type 'id groupBoundary
type 'id candidates
val toList : 'id candidates -> 'id groupBoundary list
val boundaryToList : 'id groupBoundary -> 'id functionBoundary list
module Make (Identity : OwnedIR.Identity) : sig
 module Ownership : module type of OwnedIR.Make (Identity)
 module Uniqueness : module type of InferOwnershipUniqueness.Make (Identity)
 val infer : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef AST.nonEmptyList -> (Identity.t candidates, Uniqueness.inferenceError) result
 val inferDemand : 'leaf Ownership.semantics -> AST.functionId -> OwnedIR.IntSet.t -> ('leaf, Identity.t) OwnedIR.functionDef AST.nonEmptyList -> (Identity.t groupBoundary option, Uniqueness.inferenceError) result
end
