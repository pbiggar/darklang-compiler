// ProfilerTests.fs - Unit tests for profiling utilities
//
// Ensures profiling events are recorded when enabled.

module ProfilerTests

open System
open TestDSL.Profiler
open CompilerProfiler

/// Test result type
type TestResult = Result<unit, string>

let testProfileRecording () : TestResult =
    let original = Environment.GetEnvironmentVariable("TEST_PROFILE")
    let wasEnabled = original = "1"
    if not wasEnabled then
        Environment.SetEnvironmentVariable("TEST_PROFILE", "1")
        clear ()
    let _value = time "unit_test" [ ("phase", "test") ] (fun () -> 1 + 1)
    let events = snapshot ()
    if not wasEnabled then
        Environment.SetEnvironmentVariable("TEST_PROFILE", original)
    if List.isEmpty events then
        Error "Expected profiling events to be recorded"
    else
        Ok ()

let testCompilerFunctionRecording () : TestResult =
    let original = Environment.GetEnvironmentVariable("TEST_PROFILE")
    let wasEnabled = original = "1"
    if not wasEnabled then
        Environment.SetEnvironmentVariable("TEST_PROFILE", "1")
        CompilerProfiler.clear ()
    CompilerProfiler.recordFunction "unit_pass" "unit_function" [ "i64"; "bool" ] [ ("scope", "unit") ]
    let events = CompilerProfiler.snapshot ()
    if not wasEnabled then
        Environment.SetEnvironmentVariable("TEST_PROFILE", original)
    if List.isEmpty events then
        Error "Expected compiler profiler events to be recorded"
    else
        Ok ()

/// Run all profiler tests
let runAll () : TestResult =
    let tests = [
        ("profile recording", testProfileRecording)
        ("compiler function recording", testCompilerFunctionRecording)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
