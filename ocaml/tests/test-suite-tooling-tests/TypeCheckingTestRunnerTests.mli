(* Original type checking runner expectation test. *)
type testResult = (unit, string) result
val testExpectErrorRejectsParseErrors : unit -> testResult
val tests : (string * (unit -> testResult)) list
