(* VerifyOwnedHIR.fs - Jointly verify typed HIR and its independent ownership boundary. *)
type 'leaf hirContracts = {leaf : 'leaf -> HIR.primitiveContract; callSignature : AST.functionId -> HIR.functionSignature option; callContract : HIR.functionCall -> HIR.primitiveContract option}
module Make (Identity : OwnedIR.Identity) : sig
 module Ownership : module type of OwnedIR.Make (Identity)
 type verificationError = HIRVerificationFailed of VerifyHIR.verificationError | OwnershipVerificationFailed of Ownership.verificationError
 val verifyFunctions : 'leaf hirContracts -> 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef list -> (unit, verificationError) result
 val analyzeFunctions : 'leaf hirContracts -> 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef list -> (OwnedIR.callSiteFacts list, verificationError) result
end
