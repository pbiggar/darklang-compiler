// CompilerCachingTests.fs - Unit tests for compiler caching behavior
//
// Ensures specialized functions are cached after compilation so they are not
// recompiled for every test expression.

module CompilerCachingTests

open CompilerLibrary

/// Test result type
type TestResult = Result<unit, string>

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
                        let compileResult = CompilerLibrary.compileTestWithPreamble 0 CompilerLibrary.defaultOptions stdlib preambleCtx source
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

/// Run all compiler caching unit tests
let runAll () : TestResult =
    let tests = [
        ("specialized function caching", testSpecializedFunctionCaching)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
