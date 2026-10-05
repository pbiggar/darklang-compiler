(* Complete original ownership contracts. *)
val testMapHelperAccumulatorReturnDoesNotRetainOwnedAccumulator : unit -> (unit,string) result
val testMapHelperSelfTailCallReleasesReplacedAccumulator : unit -> (unit,string) result
val testBorrowedProjectionSelfTailCallArgsAreRetained : unit -> (unit,string) result
val testBorrowedProjectionSelfRecursiveCallArgsAreRetained : unit -> (unit,string) result
val testBorrowedProjectionAliasSelfRecursiveCallArgsAreRetained : unit -> (unit,string) result
val testBorrowedProjectionIfBranchSelfRecursiveCallArgsAreRetained : unit -> (unit,string) result
val testBorrowedProjectionFromParameterSelfRecursiveCallStaysBorrowed : unit -> (unit,string) result
val testMapHelperClosureProducingCallRetainsBorrowedSource : unit -> (unit,string) result
val testMapHelperClosureSourceToValueKeepsSourceBorrowed : unit -> (unit,string) result
val testClosurePushBackRetainsImmediateClosureCallResult : unit -> (unit,string) result
