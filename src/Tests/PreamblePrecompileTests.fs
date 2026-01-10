// PreamblePrecompileTests.fs - Unit tests for precompiling E2E preambles
//
// Ensures precompilation populates the stdlib preamble cache.

module PreamblePrecompileTests

open CompilerLibrary
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner
open StdlibTestHarness

/// Test result type
type TestResult = Result<unit, string>

let private makeTest (name: string) (source: string) (preamble: string) (sourceFile: string) : E2ETest =
    {
        Name = name
        Source = source
        Preamble = preamble
        ExpectedStdout = None
        ExpectedStderr = None
        ExpectedExitCode = 0
        ExpectCompileError = false
        ExpectedErrorMessage = None
        DisableFreeList = false
        DisableANFOpt = false
        DisableInlining = false
        DisableTCO = false
        DisableMIROpt = false
        DisableLIROpt = false
        DisableDCE = false
        SourceFile = sourceFile
        FunctionLineMap = Map.empty
    }

let testPrecompilePopulatesCache (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = StdlibTestHarness.resetCaches sharedStdlib
    let preamble = "def add(x: Int64, y: Int64) : Int64 = x + y"
    let tests = [
        makeTest "precompile-1" "add(1, 2)" preamble "precompile.e2e"
        makeTest "precompile-2" "add(3, 4)" preamble "precompile.e2e"
    ]

    match precompilePreambles stdlib tests with
    | Error err -> Error $"Precompile failed: {err}"
    | Ok () ->
        let cacheKey = ("precompile.e2e", preamble.GetHashCode())
        if stdlib.PreambleCache.ContainsKey cacheKey then
            Ok ()
        else
            Error "Expected preamble cache entry to be populated"

let tests : (string * (StdlibResult -> TestResult)) list = [
    ("precompile populates cache", testPrecompilePopulatesCache)
]

let testsWithStdlib (sharedStdlib: StdlibResult) : (string * (unit -> TestResult)) list =
    tests |> List.map (fun (name, test) -> (name, fun () -> test sharedStdlib))

/// Run all precompile tests
let runAllWithStdlib (sharedStdlib: StdlibResult) : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests (testsWithStdlib sharedStdlib)

let runAll () : TestResult =
    match StdlibTestHarness.compileStdlib() with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib -> runAllWithStdlib stdlib
