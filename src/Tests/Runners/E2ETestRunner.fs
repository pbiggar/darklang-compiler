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
    CompileTime: TimeSpan
    RuntimeTime: TimeSpan
}

/// Compile stdlib once (call at test startup, pass result to runE2ETest)
/// Uses pre-compiled stdlib - all functions compiled to LIR upfront for maximum speed
let compileStdlib () : Result<CompilerLibrary.StdlibResult, string> =
    CompilerLibrary.compileStdlib()

/// Precompile a single preamble and populate the cache
let precompilePreamble (stdlib: CompilerLibrary.StdlibResult) (sourceFile: string) (preamble: string) (funcLineMap: Map<string, int>) : Result<unit, string> =
    let preambleHash = preamble.GetHashCode()
    let cacheKey = (sourceFile, preambleHash)
    if stdlib.PreambleCache.ContainsKey cacheKey then
        Ok ()
    else
        match CompilerLibrary.compilePreamble stdlib preamble sourceFile funcLineMap with
        | Error err -> Error $"Preamble precompile error ({sourceFile}): {err}"
        | Ok ctx ->
            stdlib.PreambleCache.TryAdd(cacheKey, ctx) |> ignore
            Ok ()

/// Precompile all distinct preambles (by file + preamble text) and populate the cache
let precompilePreambles (stdlib: CompilerLibrary.StdlibResult) (tests: E2ETest list) : Result<unit, string> =
    let preambleGroups = tests |> List.groupBy (fun test -> (test.SourceFile, test.Preamble))

    let rec compileAll remaining =
        match remaining with
        | [] -> Ok ()
        | ((sourceFile, preamble), group) :: rest ->
            let funcLineMap =
                group
                |> List.tryHead
                |> Option.map (fun test -> test.FunctionLineMap)
                |> Option.defaultValue Map.empty
            match precompilePreamble stdlib sourceFile preamble funcLineMap with
            | Error err -> Error err
            | Ok () -> compileAll rest

    compileAll preambleGroups

/// Run E2E test with pre-compiled stdlib
let runE2ETest (stdlib: CompilerLibrary.StdlibResult) (test: E2ETest) : E2ETestResult =
    try
        let options : CompilerLibrary.CompilerOptions = {
            DisableFreeList = test.DisableFreeList
            DisableANFOpt = test.DisableANFOpt
            DisableInlining = test.DisableInlining
            DisableTCO = test.DisableTCO
            DisableMIROpt = test.DisableMIROpt
            DisableLIROpt = test.DisableLIROpt
            DisableDCE = test.DisableDCE
            EnableCoverage = false  // Coverage integration handled separately
            DumpANF = false
            DumpMIR = false
            DumpLIR = false
        }
        let execResult = CompilerLibrary.compileAndRunWithStdlibCachedTimed 0 options stdlib test.Source test.Preamble test.SourceFile test.FunctionLineMap

        // Handle error expectation
        if test.ExpectCompileError then
            let gotError = execResult.ExitCode <> 0
            if not gotError then
                { Success = false
                  Message = "Expected compilation error but compilation succeeded"
                  Stdout = Some execResult.Stdout
                  Stderr = Some execResult.Stderr
                  ExitCode = Some execResult.ExitCode
                  CompileTime = execResult.CompileTime
                  RuntimeTime = execResult.RuntimeTime }
            else
                match test.ExpectedErrorMessage with
                | Some expectedMsg ->
                    let output = execResult.Stderr
                    if output.Contains(expectedMsg) then
                        { Success = true
                          Message = "Compilation failed with expected error message"
                          Stdout = Some execResult.Stdout
                          Stderr = Some execResult.Stderr
                          ExitCode = Some execResult.ExitCode
                          CompileTime = execResult.CompileTime
                          RuntimeTime = execResult.RuntimeTime }
                    else
                        { Success = false
                          Message = $"Expected error message '{expectedMsg}' not found in stderr"
                          Stdout = Some execResult.Stdout
                          Stderr = Some execResult.Stderr
                          ExitCode = Some execResult.ExitCode
                          CompileTime = execResult.CompileTime
                          RuntimeTime = execResult.RuntimeTime }
                | None ->
                    { Success = true
                      Message = "Compilation failed as expected"
                      Stdout = Some execResult.Stdout
                      Stderr = Some execResult.Stderr
                      ExitCode = Some execResult.ExitCode
                      CompileTime = execResult.CompileTime
                      RuntimeTime = execResult.RuntimeTime }
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
              ExitCode = Some execResult.ExitCode
              CompileTime = execResult.CompileTime
              RuntimeTime = execResult.RuntimeTime }
    with
    | ex ->
        { Success = false
          Message = $"Test execution failed: {ex.Message}"
          Stdout = None
          Stderr = None
          ExitCode = None
          CompileTime = TimeSpan.Zero
          RuntimeTime = TimeSpan.Zero }
