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

let testAiProgressSecondsParsesPositiveInteger () : TestResult =
    match parseAiProgressSecondsArg [| "--ai"; "--ai-progress-seconds=5" |] with
    | Ok value -> expectEqual (Some 5) value
    | Error msg -> Error $"Expected valid AI progress seconds, got error: {msg}"

let testAiProgressSecondsRejectsZero () : TestResult =
    match parseAiProgressSecondsArg [| "--ai-progress-seconds=0" |] with
    | Ok value -> Error $"Expected invalid AI progress seconds, got {value}"
    | Error "--ai-progress-seconds must be a positive integer" -> Ok ()
    | Error msg -> Error $"Unexpected error: {msg}"

let testTimingsJsonParsesPath () : TestResult =
    match parseTimingsJsonArg [| "--timings-json=/tmp/timings.json" |] with
    | Ok value -> expectEqual (Some "/tmp/timings.json") value
    | Error msg -> Error $"Expected valid timings JSON path, got error: {msg}"

let testTimingsJsonRejectsEmptyPath () : TestResult =
    match parseTimingsJsonArg [| "--timings-json=" |] with
    | Ok value -> Error $"Expected invalid timings JSON path, got {value}"
    | Error "--timings-json requires a non-empty path" -> Ok ()
    | Error msg -> Error $"Unexpected error: {msg}"

let tests = [
    ("AI progress seconds parses positive integer", testAiProgressSecondsParsesPositiveInteger)
    ("AI progress seconds rejects zero", testAiProgressSecondsRejectsZero)
    ("timings JSON parses path", testTimingsJsonParsesPath)
    ("timings JSON rejects empty path", testTimingsJsonRejectsEmptyPath)
]
