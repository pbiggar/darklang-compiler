// StdlibTestHarnessTests.fs - Unit tests for stdlib test harness plumbing
//
// Exercises shared stdlib compilation and cache reset helpers.

module StdlibTestHarnessTests

open CompilerLibrary
open StdlibTestHarness

/// Test result type
type TestResult = Result<unit, string>

let testSharedStdlibIsStable () : TestResult =
    match StdlibTestHarness.getSharedStdlibResult () with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib ->
        match StdlibTestHarness.getSharedStdlibResult () with
        | Error err -> Error $"Stdlib compile failed: {err}"
        | Ok stdlibAgain ->
            if obj.ReferenceEquals(stdlib, stdlibAgain) then
                Ok ()
            else
                Error "Expected shared stdlib result to return the same instance"

let testResetCachesCreatesFreshInstances () : TestResult =
    match StdlibTestHarness.getSharedStdlibResult () with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib ->
        let resetStdlib = StdlibTestHarness.resetCaches stdlib
        if obj.ReferenceEquals(stdlib.SpecCache, resetStdlib.SpecCache) then
            Error "Expected SpecCache to be reset"
        elif obj.ReferenceEquals(stdlib.CompiledFuncCache, resetStdlib.CompiledFuncCache) then
            Error "Expected CompiledFuncCache to be reset"
        elif obj.ReferenceEquals(stdlib.ANFFuncCache, resetStdlib.ANFFuncCache) then
            Error "Expected ANFFuncCache to be reset"
        elif obj.ReferenceEquals(stdlib.PreambleCache, resetStdlib.PreambleCache) then
            Error "Expected PreambleCache to be reset"
        elif obj.ReferenceEquals(stdlib.CodegenCache, resetStdlib.CodegenCache) then
            Error "Expected CodegenCache to be reset"
        else
            Ok ()

let tests : (string * (unit -> TestResult)) list = [
    ("shared stdlib stable", testSharedStdlibIsStable)
    ("reset caches", testResetCachesCreatesFreshInstances)
]
