(*
   LIRExecutionDSLTests.mli - Unit tests for executable LIR fixtures.
   Validates typed parsing, expectation rules, and native x64 execution.
*)
type testResult = (unit, string) result

val testParsesAndRunsExitCase : unit -> testResult
val testRequiresAnExpectation : unit -> testResult
val testParsesAndRunsCodegenErrorCase : unit -> testResult
val testRejectsMixedOutcomeKinds : unit -> testResult
val tests : (string * (unit -> testResult)) list
