// TestRunnerArgsTests.fs - Unit tests for test runner CLI argument parsing.
//
// Covers behavior owned by the F# test runner entrypoint rather than the shell wrapper.

module TestRunnerArgsTests

open TestRunnerArgs

type TestResult = Result<unit, string>

let private expectEqual expected actual : TestResult =
    if actual = expected then
        Ok ()
    else
        Error $"Expected {expected}, got {actual}"

let testTimingsJsonParsesPath () : TestResult =
    match parseTimingsJsonArg [| "--timings-json=/tmp/timings.json" |] with
    | Ok value -> expectEqual (Some "/tmp/timings.json") value
    | Error msg -> Error $"Expected valid timings JSON path, got error: {msg}"

let testTimingsJsonRejectsEmptyPath () : TestResult =
    match parseTimingsJsonArg [| "--timings-json=" |] with
    | Ok value -> Error $"Expected invalid timings JSON path, got {value}"
    | Error "--timings-json requires a non-empty path" -> Ok ()
    | Error msg -> Error $"Unexpected error: {msg}"

let testCodegenProfileJsonParsesPath () : TestResult =
    match parseCodegenProfileJsonArg [| "--codegen-profile-json=/tmp/codegen.json" |] with
    | Ok value -> expectEqual (Some "/tmp/codegen.json") value
    | Error msg -> Error $"Expected valid codegen profile path, got error: {msg}"

let testJsonBenchmarkParsesPath () : TestResult =
    match parseJsonBenchmarkArg [| "--json-benchmark=/tmp/json-benchmark.json" |] with
    | Ok value -> expectEqual (Some "/tmp/json-benchmark.json") value
    | Error msg -> Error $"Expected valid JSON benchmark path, got error: {msg}"

let tests = [
    ("timings JSON parses path", testTimingsJsonParsesPath)
    ("timings JSON rejects empty path", testTimingsJsonRejectsEmptyPath)
    ("codegen profile JSON parses path", testCodegenProfileJsonParsesPath)
    ("JSON benchmark parses path", testJsonBenchmarkParsesPath)
]
