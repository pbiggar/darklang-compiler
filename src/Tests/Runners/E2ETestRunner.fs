// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open System.IO
open System.Diagnostics
open TestDSL.E2EFormat

/// Result of running an E2E test
type E2ETestResult = {
    Success: bool
    Message: string
    Stdout: string option
    Stderr: string option
    ExitCode: int option
}

/// Run E2E test by compiling and executing using the compiler library
let runE2ETest (test: E2ETest) : E2ETestResult =
    try
        // Call compiler as library (no process spawn for compilation)
        let execResult = CompilerLibrary.compileAndRun test.Source

        // Compare results
        let stdoutMatches =
            match test.ExpectedStdout with
            | None -> true
            | Some expected -> execResult.Stdout.Trim() = expected.Trim()

        let stderrMatches =
            match test.ExpectedStderr with
            | None -> true
            | Some expected -> execResult.Stderr.Trim() = expected.Trim()

        let exitCodeMatches = execResult.ExitCode = test.ExpectedExitCode

        let success = stdoutMatches && stderrMatches && exitCodeMatches

        { Success = success
          Message = if success then "Test passed" else "Output mismatch"
          Stdout = Some execResult.Stdout
          Stderr = Some execResult.Stderr
          ExitCode = Some execResult.ExitCode }
    with
    | ex ->
        { Success = false
          Message = $"Test execution failed: {ex.Message}"
          Stdout = None
          Stderr = None
          ExitCode = None }
