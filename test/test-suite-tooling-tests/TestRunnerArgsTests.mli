(* TestRunnerArgsTests.mli - Original runner argument unit tests. *)
type testResult = (unit, string) result

val tests : (string * (unit -> testResult)) list
