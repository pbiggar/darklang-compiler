(* Production SSA borrowed-parameter and closure-result ownership checks. *)
val testBorrowedProjectionFromParameterSelfRecursiveCallStaysBorrowed :
  unit -> (unit, string) result

val testMapHelperClosureSourceToValueKeepsSourceBorrowed :
  unit -> (unit, string) result

val testClosurePushBackRetainsImmediateClosureCallResult :
  unit -> (unit, string) result
