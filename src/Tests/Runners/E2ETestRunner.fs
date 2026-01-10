// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open System.Threading.Tasks
open TestDSL.E2EFormat
open StdlibTestHarness

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
    StdlibTestHarness.compileStdlib()

/// Preamble cache key: (source file, preamble text)
type PreambleKey = string * string

/// Map of precompile tasks keyed by preamble
type PreambleTaskMap = Map<PreambleKey, Task<Result<unit, string>>>

/// Precompile a single preamble and populate the cache
let precompilePreamble (stdlib: CompilerLibrary.StdlibResult) (sourceFile: string) (preamble: string) (funcLineMap: Map<string, int>) : Result<unit, string> =
    let preambleHash = preamble.GetHashCode()
    let cacheKey = (sourceFile, preambleHash)
    if stdlib.PreambleCache.ContainsKey cacheKey then
        Ok ()
    else
        let lazyCtx =
            stdlib.PreambleCache.GetOrAdd(
                cacheKey,
                fun _ ->
                    Lazy<Result<CompilerLibrary.PreambleContext, string>>(
                        (fun () -> CompilerLibrary.compilePreamble stdlib preamble sourceFile funcLineMap),
                        System.Threading.LazyThreadSafetyMode.ExecutionAndPublication))
        match lazyCtx.Value with
        | Error err -> Error $"Preamble precompile error ({sourceFile}): {err}"
        | Ok _ -> Ok ()

/// Start precompiling all distinct preambles (by file + preamble text)
let startPreamblePrecompileTasks (stdlib: CompilerLibrary.StdlibResult) (tests: E2ETest array) : PreambleTaskMap =
    tests
    |> Array.toList
    |> List.groupBy (fun test -> (test.SourceFile, test.Preamble))
    |> List.map (fun ((sourceFile, preamble), group) ->
        let funcLineMap =
            group
            |> List.tryHead
            |> Option.map (fun test -> test.FunctionLineMap)
            |> Option.defaultValue Map.empty
        let task =
            Task.Run(fun () ->
                precompilePreamble stdlib sourceFile preamble funcLineMap)
        ((sourceFile, preamble), task))
    |> Map.ofList

/// Await precompile result for a single test's preamble (or Ok if none)
let awaitPreamblePrecompile (tasks: PreambleTaskMap) (test: E2ETest) : Result<unit, string> =
    match Map.tryFind (test.SourceFile, test.Preamble) tasks with
    | None -> Ok ()
    | Some task -> task.Result

/// Precompile all distinct preambles (by file + preamble text) and populate the cache
let precompilePreambles (stdlib: CompilerLibrary.StdlibResult) (tests: E2ETest list) : Result<unit, string> =
    let tasks = startPreamblePrecompileTasks stdlib (List.toArray tests)
    let rec awaitAll (remaining: (PreambleKey * Task<Result<unit, string>>) list) =
        match remaining with
        | [] -> Ok ()
        | (_, task) :: rest ->
            match task.Result with
            | Error err -> Error err
            | Ok () -> awaitAll rest
    awaitAll (tasks |> Map.toList)

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
        let execResult =
            CompilerLibrary.compileAndRunWithStdlibCachedTimed 0 options stdlib test.Source test.Preamble test.SourceFile test.FunctionLineMap

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
