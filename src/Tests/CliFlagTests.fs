// CliFlagTests.fs - Unit tests for CLI flag parsing
//
// Verifies that IR dump flags are accepted and mapped into CLI options.

module CliFlagTests

open Program

/// Test result type
type TestResult = Result<unit, string>

/// Test that IR dump flags parse and set the expected options
let testDumpIrFlags () : TestResult =
    let args = [| "--dump-anf"; "--dump-mir"; "--dump-lir"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected dump flags to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DumpANF then
            Error "Expected DumpANF to be true"
        else if not opts.DumpMIR then
            Error "Expected DumpMIR to be true"
        else if not opts.DumpLIR then
            Error "Expected DumpLIR to be true"
        else
            Ok ()

/// Run all CLI flag unit tests
let runAll () : TestResult =
    let tests = [
        ("IR dump flags", testDumpIrFlags)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
