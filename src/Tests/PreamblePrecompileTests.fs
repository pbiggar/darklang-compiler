// PreamblePrecompileTests.fs - Unit tests for precompiling E2E preambles
//
// Ensures precompilation populates the stdlib preamble cache.

module PreamblePrecompileTests

open CompilerLibrary
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner

/// Test result type
type TestResult = Result<unit, string>

let private withFreshCaches (stdlib: StdlibResult) : StdlibResult =
    { stdlib with
        SpecCache = SpecializationCache()
        CompiledFuncCache = createCompiledFunctionCache ()
        ANFFuncCache = ANFFunctionCache()
        PreambleCache = PreambleCache()
        CodegenCache = CodegenCache() }

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
    let stdlib = withFreshCaches sharedStdlib
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

/// Run all precompile tests
let runAllWithStdlib (sharedStdlib: StdlibResult) : TestResult =
    let tests = [
        ("precompile populates cache", fun () -> testPrecompilePopulatesCache sharedStdlib)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests

let runAll () : TestResult =
    match TestDSL.E2ETestRunner.compileStdlib() with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib -> runAllWithStdlib stdlib
