(* Complete original ownership contracts. *)
val testBorrowedCallMaterializesOwnedLocal : unit -> (unit,string) result
val testReturnedBorrowedCallMaterializesOwnership : unit -> (unit,string) result
val testCallReturningClosureGetsAutoDecAfterUse : unit -> (unit,string) result
val testClosureCallReturningClosureGetsAutoDecAfterUse : unit -> (unit,string) result
val testPureEnumBindingDoesNotGetAutomaticDec : unit -> (unit,string) result
val testGenericPureEnumBindingDoesNotGetAutomaticDec : unit -> (unit,string) result
val testProgramRcFreshTempsFollowExistingProgramTemps : unit -> (unit,string) result
val testProgramRcRejectsDriftedOwnershipContract : unit -> (unit,string) result
val testBareSumTypeRefsAreCanonicalizedForRcSourceTypes : unit -> (unit,string) result
