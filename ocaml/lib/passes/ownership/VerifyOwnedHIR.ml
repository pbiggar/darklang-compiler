(*
   HIR contracts remain authoritative for types, effects, and aliases;
   ownership contracts account only for unit transfer and uniqueness.
   Typed identities and independent primitive contracts must verify before
   ownership facts may drive selection or identify calls for materialization.
*)
(* VerifyOwnedHIR.fs - Jointly verify typed HIR and its independent ownership boundary. *)
[@@@warning "-4"]
type 'leaf hirContracts = {leaf : 'leaf -> HIR.primitiveContract; callSignature : AST.functionId -> HIR.functionSignature option; callContract : HIR.functionCall -> HIR.primitiveContract option}
module Make (Identity : OwnedIR.Identity) = struct
 module Ownership = OwnedIR.Make (Identity)
 module Verification = VerifyOwnership.Make (Identity)
 type verificationError = HIRVerificationFailed of VerifyHIR.verificationError | OwnershipVerificationFailed of Ownership.verificationError
 let hirBody (block : ('leaf, Identity.t) OwnedIR.block) = {HIR.parameters = block.OwnedIR.body.HIR.parameters; operations = List.filter_map (function OwnedIR.Evaluate operation -> Some operation | OwnedIR.Dup _ | OwnedIR.Drop _ -> None) block.OwnedIR.body.HIR.operations; result = block.OwnedIR.body.HIR.result}
 let withVerifiedHIR (hir : 'leaf hirContracts) functions analyze =
  let dialect : ('leaf, ('leaf, Identity.t) OwnedIR.block) VerifyHIR.dialect = {VerifyHIR.body = hirBody; leaf = hir.leaf; callSignature = hir.callSignature; callContract = hir.callContract} in
  let definitions = List.map (fun (definition : ('leaf, Identity.t) OwnedIR.functionDef) -> definition.OwnedIR.definition) functions in
  match VerifyHIR.verifyFunctions dialect definitions with Error error -> Error (HIRVerificationFailed error) | Ok () -> Result.map_error (fun error -> OwnershipVerificationFailed error) (analyze ())
 let verifyFunctions hir ownership functions = withVerifiedHIR hir functions (fun () -> Verification.verifyFunctions ownership functions)
 let analyzeFunctions hir ownership functions = withVerifiedHIR hir functions (fun () -> Verification.analyzeFunctions ownership functions)
end
