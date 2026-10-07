(* MaterializeOwnershipVariants.fs - Clone verified ownership candidates and route selected calls. *)
type 'id request = {caller : AST.functionId; call : HIR.functionCall; selection : 'id SelectOwnershipVariants.selection}
type ('leaf, 'id) specializedFunction = {original : AST.functionId; functionDef : ('leaf, 'id) OwnedIR.functionDef}
type ('leaf, 'id) specializedGroup = {identity : SelectOwnershipVariants.candidateIdentity; members : ('leaf, 'id) specializedFunction AST.nonEmptyList}
type callRewrite = {site : OwnedIR.callSiteIdentity; original : HIR.functionCall; specialized : HIR.functionCall; ownership : OwnedIR.callSignature}
type ('leaf, 'id) plan
val groups : ('leaf, 'id) plan -> ('leaf, 'id) specializedGroup list
val rewrites : ('leaf, 'id) plan -> callRewrite list
val originals : ('leaf, 'id) plan -> ('leaf, 'id) OwnedIR.functionDef list
val functions : ('leaf, 'id) plan -> ('leaf, 'id) OwnedIR.functionDef list
val hirContracts : ('leaf, 'id) plan -> 'leaf VerifyOwnedHIR.hirContracts -> 'leaf VerifyOwnedHIR.hirContracts
module Make (Identity : OwnedIR.Identity) : sig
 module Verification : module type of VerifyOwnedHIR.Make (Identity)
 module Ownership : module type of OwnedIR.Make (Identity)
 type materializationError = GroupingFailed of OwnedFunctionGroups.groupingError | InvalidOriginalProgram of Verification.verificationError | MissingGroupMember of string | GroupMembershipMismatch of string | BoundaryMismatch of string | MissingCallSite of OwnedIR.callSiteIdentity | DuplicateCallSite of OwnedIR.callSiteIdentity | StaleCallSite of OwnedIR.callSiteIdentity | MixedRecursiveCandidate of OwnedIR.callSiteIdentity | SymbolCollision of string | InvalidMaterializedProgram of Verification.verificationError
 val ownershipSemantics : ('leaf, Identity.t) plan -> 'leaf Ownership.semantics -> 'leaf Ownership.semantics
 val materialize : 'leaf VerifyOwnedHIR.hirContracts -> 'leaf Ownership.semantics -> string FunctionIdMap.t -> ('leaf, Identity.t) OwnedIR.functionDef list -> Identity.t request list -> (('leaf, Identity.t) plan, materializationError) result
end
