(* ElaborateFunctionOwnership.mli - Infer conservative boundaries and place whole-function ownership steps. *)
module Ownership : module type of OwnedIR.Make (ListLiveness.Identity)
type ('leaf, 'block) dialect = {
 body : 'block -> ('leaf, 'block) HIR.operation HIR.block;
 leafOwnership : 'leaf -> HIR.valueId OwnedIR.contract;
 leafUniqueness : 'leaf -> Ownership.uniquenessContract;
 isManaged : HIR.value -> bool;
 externalCallOwnership : HIR.functionCall -> OwnedIR.callSignature option
}
type elaborationError = UnknownCallOwnership of AST.functionId | InconsistentCallParameters of AST.functionId | InvalidFunctionBoundary of AST.functionId * Ownership.verificationError
type 'leaf analysis
val functions : 'leaf analysis -> ('leaf, HIR.valueId) OwnedIR.functionDef list
val semantics : 'leaf analysis -> 'leaf Ownership.semantics
(* Timing callbacks receive monotonic elapsed milliseconds. *)
val elaborateFunctionsWithTrace : (string -> float -> unit) option -> ('leaf, 'block) dialect -> 'block HIR.functionDef list -> ('leaf analysis, elaborationError) result
val elaborateFunctions : ('leaf, 'block) dialect -> 'block HIR.functionDef list -> ('leaf analysis, elaborationError) result
