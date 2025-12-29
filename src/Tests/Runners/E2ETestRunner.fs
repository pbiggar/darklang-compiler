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

/// Compile stdlib once (call at test startup, pass result to runE2ETest)
let compileStdlib () : Result<CompilerLibrary.StdlibResult, string> =
    CompilerLibrary.compileStdlib()

/// Run E2E test with pre-compiled stdlib
let runE2ETest (stdlib: CompilerLibrary.StdlibResult) (test: E2ETest) : E2ETestResult =
    try
        let options : CompilerLibrary.CompilerOptions = {
            DisableFreeList = test.DisableFreeList
            DisableANFOpt = test.DisableANFOpt
            DisableTCO = test.DisableTCO
            DisableMIROpt = test.DisableMIROpt
            DisableLIROpt = test.DisableLIROpt
            DisableDCE = test.DisableDCE
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

