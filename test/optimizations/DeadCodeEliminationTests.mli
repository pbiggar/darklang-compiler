(* Original function-reachability assertions. *)
type testResult = (unit, string) result

val testArgMovesFunctionAddressIsReachable : unit -> testResult
val testListDisplayHelperIsReachableByCanonicalIdentity : unit -> testResult
val testFilteredFunctionsPreserveReachableSetAndInputOrder : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
