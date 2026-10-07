(*
   ParallelMoveDSLTests.mli - Unit tests for parallel-move fixture parsing and execution.
   Validates the DSL boundary without relying on the fixtures that it loads.
*)
type testResult = (unit, string) result

val testParsesAndRunsMultipleMoveCases : unit -> testResult
val testRejectsVirtualDestination : unit -> testResult
val tests : (string * (unit -> testResult)) list
