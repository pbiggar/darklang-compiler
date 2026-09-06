// TestRunnerArgs.fs - Helper functions for test runner CLI arguments
//
// Provides parsing helpers shared by the test runner and its unit tests.

module TestRunnerArgs

let private parsePrefixedArg (prefix: string) (args: string array) : string option =
    args
    |> Array.tryFind (fun arg -> arg.StartsWith(prefix))
    |> Option.map (fun arg -> arg.Substring(prefix.Length))

// Parse command line for --filter=PATTERN option
let parseFilterArg (args: string array) : string option =
    parsePrefixedArg "--filter=" args

// Check if --coverage flag is present (show inline coverage after tests)
let hasCoverageArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--coverage")

// Check if --verification flag is present (enable verification/stress tests)
let hasVerificationArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--verification")

// Check if --verbose flag is present (print failing tests immediately)
let hasVerboseArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--verbose" || arg = "-v")

// Check if --parser-pretty-roundtrip is present (legacy compatibility no-op)
let hasParserPrettyRoundtripArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--parser-pretty-roundtrip")

// Check if --roundtrip-all-dark is present (include all upstream .dark files in corpus roundtrip)
let hasRoundtripAllDarkArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--roundtrip-all-dark")

// Check if --all-test-timings is present (print timing for every test)
let hasAllTestTimingsArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--all-test-timings")

// Check if quiet mode is present (compact success/failure output)
let hasQuietArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--quiet")

// Check if AI mode is present (compact output with test-count progress)
let hasAiArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--ai")

// Parse --timings-json=PATH option
let parseTimingsJsonArg (args: string array) : Result<string option, string> =
    match parsePrefixedArg "--timings-json=" args with
    | None -> Ok None
    | Some path when path.Trim() = "" -> Error "--timings-json requires a non-empty path"
    | Some path -> Ok (Some path)

// Parse --codegen-profile-json=PATH option
let parseCodegenProfileJsonArg (args: string array) : Result<string option, string> =
    match parsePrefixedArg "--codegen-profile-json=" args with
    | None -> Ok None
    | Some path when path.Trim() = "" -> Error "--codegen-profile-json requires a non-empty path"
    | Some path -> Ok (Some path)

// Parse --json-benchmark=PATH option
let parseJsonBenchmarkArg (args: string array) : Result<string option, string> =
    match parsePrefixedArg "--json-benchmark=" args with
    | None -> Ok None
    | Some path when path.Trim() = "" -> Error "--json-benchmark requires a non-empty path"
    | Some path -> Ok (Some path)

// Parse --e2e-batch-size=N. One preserves singular execution for comparison;
// larger values batch compatible value-equality tests.
let parseE2EBatchSizeArg (args: string array) : Result<int option, string> =
    match parsePrefixedArg "--e2e-batch-size=" args with
    | None -> Ok None
    | Some value ->
        match System.Int32.TryParse value with
        | true, size when size >= 1 && size <= TestDSL.E2ETestRunner.maxSupportedBatchSize -> Ok (Some size)
        | _ -> Error "--e2e-batch-size requires an integer from 1 through 32"

// Check if a test name matches the filter (case-insensitive substring match)
let matchesFilter (filter: string option) (testName: string) : bool =
    match filter with
    | None -> true
    | Some pattern -> testName.ToLowerInvariant().Contains(pattern.ToLowerInvariant())

// Check if --help flag is present
let hasHelpArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--help" || arg = "-h")
