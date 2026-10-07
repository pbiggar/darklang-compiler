(*
   SyntaxDSLTests.fs - Unit tests for the syntax fixture parser and runner.
   Keeps the DSL implementation honest without expressing its own behavior in the DSL.
*)
type testResult=(unit,string) result
val testParsesMultipleSyntaxCases : unit -> testResult
val testRejectsLegacySyntaxSelector : unit -> testResult
val testRunsFormattingAndRoundtripChecks : unit -> testResult
val tests : (string * (unit -> testResult)) list
