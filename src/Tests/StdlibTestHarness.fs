// StdlibTestHarness.fs - Shared stdlib compile helpers for test suites
//
// Centralizes stdlib compilation and cache reset helpers for test scenarios.

module StdlibTestHarness

open CompilerLibrary

let private sharedStdlib : Lazy<Result<StdlibResult, string>> =
    Lazy<Result<StdlibResult, string>>(fun () -> CompilerLibrary.compileStdlib())

/// Return a shared stdlib compilation result for tests that want a cached instance.
let getSharedStdlibResult () : Result<StdlibResult, string> =
    sharedStdlib.Value

/// Compile stdlib for tests that need an explicit compilation step.
let compileStdlib () : Result<StdlibResult, string> =
    CompilerLibrary.compileStdlib()

/// Reset caches for tests that require cold-cache behavior.
let resetCaches (stdlib: StdlibResult) : StdlibResult =
    { stdlib with
        SpecCache = SpecializationCache()
        CompiledFuncCache = createCompiledFunctionCache ()
        ANFFuncCache = ANFFunctionCache()
        PreambleCache = PreambleCache()
        CodegenCache = CodegenCache() }

/// Run a test with stdlib provided by a supplied getter.
let withStdlib
    (getStdlib: unit -> Result<StdlibResult, string>)
    (runner: StdlibResult -> Result<unit, string>)
    : Result<unit, string> =
    match getStdlib () with
    | Error err -> Error err
    | Ok stdlib -> runner stdlib
