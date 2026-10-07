(* ProgramCliTests.mli - Compiler CLI target-selection tests. *)
type testResult=(unit,string) result
val testExplicitLinuxX86_64Target : unit -> testResult
val testUnknownTargetRejected : unit -> testResult
val testCrossTargetRunRejected : unit -> testResult
val testEmitResultModeIsExplicit : unit -> testResult
val testPackageServerIsExplicit : unit -> testResult
val testPackageServerRejectsNonHttpUrl : unit -> testResult
val testScopedIRDumpOptions : unit -> testResult
val testIRDumpModifiersRequireDumpSelection : unit -> testResult
val testEmptyIRDumpValuesRejected : unit -> testResult
val testBatchCompileParsesIndependentOutputs : unit -> testResult
val testBatchCompileRejectsMissingOutput : unit -> testResult
val testBatchManifestKeepGoingParses : unit -> testResult
val testBatchCompileAllowsCompilerOwnedSources : unit -> testResult
val tests : (string * (unit -> testResult)) list
