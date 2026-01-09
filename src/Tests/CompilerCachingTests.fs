// CompilerCachingTests.fs - Unit tests for compiler caching behavior
//
// Ensures specialized functions are cached after compilation so they are not
// recompiled for every test expression.

module CompilerCachingTests

open CompilerLibrary
open CompilerProfiler

/// Test result type
type TestResult = Result<unit, string>

let private getMetaValue (key: string) (meta: (string * string) list) : string option =
    meta |> List.tryFind (fun (k, _) -> k = key) |> Option.map snd

let private countFunctionEvents (passName: string) (functionName: string) (events: ProfileEvent list) : int =
    events
    |> List.filter (fun ev ->
        ev.Name = "function_compiled"
        && getMetaValue "pass" ev.Meta = Some passName
        && getMetaValue "function" ev.Meta = Some functionName)
    |> List.length

/// Test that specialized functions are cached after compilation
let testSpecializedFunctionCaching () : TestResult =
    match CompilerLibrary.compileStdlib () with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib ->
        let source = "Stdlib.List.length<Int64>(Stdlib.List.map<Int64, Int64>([1, 2, 3], (x: Int64) => x + 1))"
        match Parser.parseString source with
        | Error err -> Error $"Parse error: {err}"
        | Ok userAst ->
            match AST_to_ANF.convertUserOnlyCached stdlib.SpecCache stdlib.ANFFuncCache "" Map.empty stdlib.GenericFuncDefs stdlib.ANFResult userAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                if Set.isEmpty userOnly.SpecializedFuncNames then
                    Error "Expected specialized functions but none were generated"
                else
                    match CompilerLibrary.compilePreamble stdlib "" "cache_test" Map.empty with
                    | Error err -> Error $"Preamble compilation failed: {err}"
                    | Ok preambleCtx ->
                        let compileResult =
                            CompilerLibrary.compileTestWithPreamble 0 CompilerLibrary.defaultOptions stdlib preambleCtx "cache_test" 0 "specialized function caching" source
                        if not compileResult.Success then
                            let errorMessage = compileResult.ErrorMessage |> Option.defaultValue "(no message)"
                            Error $"Compilation failed: {errorMessage}"
                        else
                            let missing =
                                userOnly.SpecializedFuncNames
                                |> Set.filter (fun name -> not (stdlib.CompiledFuncCache.ContainsKey name))
                            if Set.isEmpty missing then
                                Ok ()
                            else
                            let missingList = System.String.Join(", ", Set.toList missing)
                            Error $"Missing cached specialized functions: {missingList}"

/// Test that preamble functions are compiled once across tests in the same file
let testPreambleFunctionCachedOnce () : TestResult =
    let original = System.Environment.GetEnvironmentVariable("TEST_PROFILE")
    let wasEnabled = original = "1"
    if not wasEnabled then
        System.Environment.SetEnvironmentVariable("TEST_PROFILE", "1")
        CompilerProfiler.clear ()
    let result =
        match CompilerLibrary.compileStdlib () with
        | Error err -> Error $"Stdlib compile failed: {err}"
        | Ok stdlib ->
            let preamble = "def foo(x: Int64): Int64 = x + 1"
            let sourceFile = "cache_preamble_test.e2e"
            match CompilerLibrary.compilePreamble stdlib preamble sourceFile Map.empty with
            | Error err -> Error $"Preamble compilation failed: {err}"
            | Ok preambleCtx ->
                let first =
                    CompilerLibrary.compileTestWithPreamble
                        0
                        CompilerLibrary.defaultOptions
                        stdlib
                        preambleCtx
                        sourceFile
                        preamble.Length
                        "preamble cache test 1"
                        "foo(1)"
                let second =
                    CompilerLibrary.compileTestWithPreamble
                        0
                        CompilerLibrary.defaultOptions
                        stdlib
                        preambleCtx
                        sourceFile
                        preamble.Length
                        "preamble cache test 2"
                        "foo(2)"
                if not first.Success then
                    let message = first.ErrorMessage |> Option.defaultValue "(no message)"
                    Error $"First compilation failed: {message}"
                elif not second.Success then
                    let message = second.ErrorMessage |> Option.defaultValue "(no message)"
                    Error $"Second compilation failed: {message}"
                else
                    let events = CompilerProfiler.snapshot ()
                    let count = countFunctionEvents "anf_to_mir" "foo" events
                    if count = 1 then
                        Ok ()
                    else
                        Error $"Expected preamble function 'foo' to be compiled once, but saw {count} compilations"
    if not wasEnabled then
        System.Environment.SetEnvironmentVariable("TEST_PROFILE", original)
    result

/// Run all compiler caching unit tests
let runAll () : TestResult =
    let tests = [
        ("specialized function caching", testSpecializedFunctionCaching)
        ("preamble function caching", testPreambleFunctionCachedOnce)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
