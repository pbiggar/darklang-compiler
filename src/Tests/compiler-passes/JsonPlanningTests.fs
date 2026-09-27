// JsonPlanningTests.fs - Check that JSON planning preserves unrelated programs.

module JsonPlanningTests

open AST

type TestResult = Result<unit, string>

let testNonJsonProgramIsUnchanged
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    WrittenParsing.parse LibParser.Validation.Script "1L + 2L"
    |> Result.bind (fun program ->
        WrittenChecking.checkSourceUnitsWithBase
            stdlib.Context.WrittenEnvironment false true [program])
    |> Result.bind (fun (_, typedProgram, _) ->
        let env =
            CheckingTypes.mergeTypeCheckEnv
                stdlib.Context.TypeCheckEnv
                (WrittenChecking.typeCheckEnvironment typedProgram)
        let planned = JsonPlanning.rewriteProgram env typedProgram
        if planned = typedProgram then Ok ()
        else Error "Expected JSON planning to leave a program without JSON intrinsics unchanged")

let tests (stdlib: CompilationContexts.StdlibResult) = [
    ("JSON planning leaves non-JSON programs unchanged", testNonJsonProgramIsUnchanged stdlib)
]
