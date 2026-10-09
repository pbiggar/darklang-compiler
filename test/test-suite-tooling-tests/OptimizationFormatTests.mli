(*
   OptimizationFormatTests.mli - Unit tests for optimization test parsing
   Verifies the optimization test file parser accepts repository test syntax
   across common line-ending formats.
*)
type testResult = (unit, string) result

val testParseCRLFOptimizationFile : unit -> testResult
val testUnknownOptimizationSectionFails : unit -> testResult
val testParseStdlibFunctionOptimization : unit -> testResult
val testStdlibFunctionRejectsNonANFStage : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
