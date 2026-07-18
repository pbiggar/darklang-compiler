// ProgressBarTests.fs - Unit tests for the test runner progress bar
//
// Ensures the progress bar handles over-completion without crashing.

module ProgressBarTests

open System
open System.Globalization
open System.IO

type TestResult = Result<unit, string>

let private captureProgressError (run: unit -> unit) : string =
    let originalError = Console.Error
    use buffer = new StringWriter(CultureInfo.InvariantCulture)
    Console.SetError(buffer)
    try
        run ()
        Console.Error.Flush()
        buffer.ToString()
    finally
        Console.SetError(originalError)

let testProgressBarHandlesOverCompletion () : TestResult =
    let state = ProgressBar.create "Progress" 1
    let result =
        try
            ProgressBar.increment state true
            ProgressBar.increment state true
            Ok ()
        with ex ->
            Error $"ProgressBar threw exception: {ex.Message}"
    result

let testProgressBarClampsOverCompletionDisplay () : TestResult =
    let state = ProgressBar.create "Progress" 1
    let output =
        captureProgressError (fun () ->
            ProgressBar.increment state true
            ProgressBar.increment state true)

    if output.Contains("2/1") then
        Error $"Expected over-completion display to clamp to total, got: {output}"
    elif output.Contains("1/1") then
        Ok ()
    else
        Error $"Expected progress output to include completed count, got: {output}"

let tests = [
    ("handles over-completion", testProgressBarHandlesOverCompletion)
    ("clamps over-completion display", testProgressBarClampsOverCompletionDisplay)
]
