(* TestRunnerArgs.mli - Preserve the test runner's command line contract. *)
type testTarget = Host | Explicit of Dark_compiler.Platform.target

val parseFilterArg : string array -> string option
val parseTargetArg : string array -> (testTarget, string) result
val hasCoverageArg : string array -> bool
val hasVerificationArg : string array -> bool
val hasVerboseArg : string array -> bool
val hasParserPrettyRoundtripArg : string array -> bool
val hasRoundtripAllDarkArg : string array -> bool
val hasAllTestTimingsArg : string array -> bool
val hasQuietArg : string array -> bool
val hasAiArg : string array -> bool
val parseTimingsJsonArg : string array -> (string option, string) result
val parseCodegenProfileJsonArg : string array -> (string option, string) result
val defaultE2EBatchSize : int
val parseE2EBatchSizeArg : string array -> (int, string) result
val matchesFilter : string option -> string -> bool
val hasHelpArg : string array -> bool
