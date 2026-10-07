(* VerifyListOwnership.mli - Independently verify region ownership, layouts, and typed edges. *)
val verifyBlockOwnership : ListRegion.ownedBlock -> (unit, string) result
val verify : ListRegion.ownedRegion -> (unit, string) result
