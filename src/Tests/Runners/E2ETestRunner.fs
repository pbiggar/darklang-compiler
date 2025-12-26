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
        // verbosity = 0 to suppress instrumentation during tests
        let options : CompilerLibrary.CompilerOptions = {
            DisableFreeList = test.DisableFreeList
        }
        let execResult = CompilerLibrary.compileAndRunWithOptions 0 options test.Source

        // Handle error expectation
        if test.ExpectCompileError then
            // Test expects compilation to fail
            // Compilation failure is indicated by non-zero exit code
            let gotError = execResult.ExitCode <> 0
            if not gotError then
                { Success = false
                  Message = "Expected compilation error but compilation succeeded"
                  Stdout = Some execResult.Stdout
                  Stderr = Some execResult.Stderr
                  ExitCode = Some execResult.ExitCode }
            else
                // Check if expected error message is present (substring match)
                match test.ExpectedErrorMessage with
                | Some expectedMsg ->
                    // Check if message is in stderr (where compiler errors are printed)
                    let output = execResult.Stderr
                    if output.Contains(expectedMsg) then
                        { Success = true
                          Message = "Compilation failed with expected error message"
                          Stdout = Some execResult.Stdout
                          Stderr = Some execResult.Stderr
                          ExitCode = Some execResult.ExitCode }
                    else
                        { Success = false
                          Message = $"Expected error message '{expectedMsg}' not found in stderr"
                          Stdout = Some execResult.Stdout
                          Stderr = Some execResult.Stderr
                          ExitCode = Some execResult.ExitCode }
                | None ->
                    { Success = true
                      Message = "Compilation failed as expected"
                      Stdout = Some execResult.Stdout
                      Stderr = Some execResult.Stderr
                      ExitCode = Some execResult.ExitCode }
        else
            // Normal test - compare results
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
