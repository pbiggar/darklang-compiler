(*
   ScriptHelperTests.fs - Repository policy tests for compiler and shell tooling
   Enforces selected source and script invariants that are cheap to check structurally.
*)
type testResult=(unit,string) result
val testCompilerAvoidsFailwith : unit -> testResult
val testTestToolingAvoidsFailwith : unit -> testResult
val testCompilerAvoidsOptionGet : unit -> testResult
val testInstallerFormatsAssetListWithStableDelimiter : unit -> testResult
val testShellcheckScansAllTrackedBashScripts : unit -> testResult
val testDumpLirFuncDoesNotSuppressCompilerFailures : unit -> testResult
val tests : (string * (unit -> testResult)) list
