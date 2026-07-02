// TestRunnerArgs.fs - Helper functions for test runner CLI arguments
//
// Provides parsing helpers shared by the test runner and its unit tests.

module TestRunnerArgs

// Parse command line for --filter=PATTERN option
let parseFilterArg (args: string array) : string option =
    args
    |> Array.tryFind (fun arg -> arg.StartsWith("--filter="))
    |> Option.map (fun arg -> arg.Substring(9))

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

// Check if AI mode is present (compact output with sparse progress updates)
let hasAiArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--ai")

// Parse --ai-progress-seconds=N option
let parseAiProgressSecondsArg (args: string array) : Result<int option, string> =
    let arg =
        args
        |> Array.tryFind (fun arg -> arg.StartsWith("--ai-progress-seconds="))
    match arg with
    | None -> Ok None
    | Some value ->
        let text = value.Substring("--ai-progress-seconds=".Length)
        match System.Int32.TryParse(text) with
        | true, seconds when seconds > 0 -> Ok (Some seconds)
        | _ -> Error "--ai-progress-seconds must be a positive integer"

// Parse --timings-json=PATH option
let parseTimingsJsonArg (args: string array) : string option =
    args
    |> Array.tryFind (fun arg -> arg.StartsWith("--timings-json="))
    |> Option.map (fun arg -> arg.Substring(15))

// Check if a test name matches the filter (case-insensitive substring match)
let matchesFilter (filter: string option) (testName: string) : bool =
    match filter with
    | None -> true
    | Some pattern -> testName.ToLowerInvariant().Contains(pattern.ToLowerInvariant())

// Check if --help flag is present
let hasHelpArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--help" || arg = "-h")
