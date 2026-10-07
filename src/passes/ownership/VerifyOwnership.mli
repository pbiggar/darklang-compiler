(* VerifyOwnership.mli - Verify ownership and derive call facts from the same state transitions. *)
module Make (Identity : OwnedIR.Identity) : sig
 module Ownership : module type of OwnedIR.Make (Identity)
 val callSignatureOfFunction : Identity.t OwnedIR.functionSignature -> (OwnedIR.callSignature, Ownership.verificationError) result
 val verifyFunction : 'leaf Ownership.semantics -> Identity.t OwnedIR.functionSignature -> ('leaf, Identity.t) OwnedIR.block -> (unit, Ownership.verificationError) result
 val analyzeFunction : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef -> (OwnedIR.callSiteFacts list, Ownership.verificationError) result
 val verifyFunctions : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef list -> (unit, Ownership.verificationError) result
 val analyzeFunctions : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.functionDef list -> (OwnedIR.callSiteFacts list, Ownership.verificationError) result
 val verifyClosed : 'leaf Ownership.semantics -> ('leaf, Identity.t) OwnedIR.block -> (unit, Ownership.verificationError) result
end
