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

/// Cached stdlib result - initialized once before tests run
let mutable private stdlibCache : CompilerLibrary.StdlibResult option = None

/// Initialize the stdlib cache (call once before running tests)
let initializeStdlibCache () : Result<unit, string> =
    match stdlibCache with
    | Some _ -> Ok ()  // Already initialized
    | None ->
        match CompilerLibrary.compileStdlib() with
        | Error e -> Error e
        | Ok stdlib ->
            stdlibCache <- Some stdlib
            Ok ()

/// Get the cached stdlib (fails if not initialized)
let private getStdlibCache () : CompilerLibrary.StdlibResult option =
    stdlibCache

/// Run E2E test with cached stdlib
let runE2ETestWithCache (stdlib: CompilerLibrary.StdlibResult) (test: E2ETest) : E2ETestResult =
    try
        let options : CompilerLibrary.CompilerOptions = {
            DisableFreeList = test.DisableFreeList
        }
        let execResult = CompilerLibrary.compileAndRunWithStdlib 0 options stdlib test.Source

        // Handle error expectation
        if test.ExpectCompileError then
            let gotError = execResult.ExitCode <> 0
            if not gotError then
                { Success = false
                  Message = "Expected compilation error but compilation succeeded"
                  Stdout = Some execResult.Stdout
                  Stderr = Some execResult.Stderr
                  ExitCode = Some execResult.ExitCode }
            else
                match test.ExpectedErrorMessage with
                | Some expectedMsg ->
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

/// Run E2E test by compiling and executing using the compiler library
/// Uses cached stdlib if available, otherwise falls back to normal compilation
let runE2ETest (test: E2ETest) : E2ETestResult =
    match stdlibCache with
    | Some stdlib -> runE2ETestWithCache stdlib test
    | None ->
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
