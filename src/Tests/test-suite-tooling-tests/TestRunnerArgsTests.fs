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

let tests = [
    ("AI progress seconds parses positive integer", testAiProgressSecondsParsesPositiveInteger)
    ("AI progress seconds rejects zero", testAiProgressSecondsRejectsZero)
]
