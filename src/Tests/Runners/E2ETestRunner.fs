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

/// Run E2E test by compiling and executing
let runE2ETest (test: E2ETest) (compilerPath: string) : E2ETestResult =
    // Create temp directory for this test
    let guid = Guid.NewGuid().ToString("N")
    let tempDir = Path.Combine(Path.GetTempPath(), $"dark_e2e_{guid}")
    Directory.CreateDirectory(tempDir) |> ignore

    try
        // Write source to file
        let sourceFile = Path.Combine(tempDir, "test.dark")
        File.WriteAllText(sourceFile, test.Source)

        // Compile with -r flag (compile and run)
        // Note: -r mode doesn't accept -o flag
        let compileArgs = $"\"{compilerPath}\" -r \"{sourceFile}\""

        let compileInfo = ProcessStartInfo("dotnet", compileArgs)
        compileInfo.RedirectStandardOutput <- true
        compileInfo.RedirectStandardError <- true
        compileInfo.UseShellExecute <- false
        compileInfo.WorkingDirectory <- tempDir

        let compileProc = Process.Start(compileInfo)
        let compileStdout = compileProc.StandardOutput.ReadToEnd()
        let compileStderr = compileProc.StandardError.ReadToEnd()
        compileProc.WaitForExit()

        // Check exit code matches expected
        let actualExitCode = compileProc.ExitCode
        let exitCodeMatches = actualExitCode = test.ExpectedExitCode

        if not exitCodeMatches then
            //Exit code mismatch (could be compilation failure or wrong program result)
            { Success = false
              Message = if actualExitCode = 1 then "Compilation failed" else "Exit code mismatch"
              Stdout = Some compileStdout
              Stderr = Some compileStderr
              ExitCode = Some actualExitCode }
        else
            // Compilation succeeded, check execution result
            // The -r flag runs the program, so stdout/stderr/exitcode come from compilation step
            let actualExitCode = compileProc.ExitCode
            let actualStdout = compileStdout
            let actualStderr = compileStderr

            // Compare results
            let stdoutMatches =
                match test.ExpectedStdout with
                | None -> true
                | Some expected -> actualStdout.Trim() = expected.Trim()

            let stderrMatches =
                match test.ExpectedStderr with
                | None -> true
                | Some expected -> actualStderr.Trim() = expected.Trim()

            let exitCodeMatches = actualExitCode = test.ExpectedExitCode

            let success = stdoutMatches && stderrMatches && exitCodeMatches

            { Success = success
              Message = if success then "Test passed" else "Output mismatch"
              Stdout = Some actualStdout
              Stderr = Some actualStderr
              ExitCode = Some actualExitCode }
    finally
        // Clean up
        try
            if Directory.Exists tempDir then
                Directory.Delete(tempDir, true)
        with
        | _ -> () // Ignore cleanup errors
