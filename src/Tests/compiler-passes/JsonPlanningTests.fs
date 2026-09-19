// JsonPlanningTests.fs - structural tests for typed JSON specialization.

module JsonPlanningTests

open AST

type TestResult = Result<unit, string>

let private plannedSource
    (stdlib: CompilationContexts.StdlibResult)
    (source: string)
    : Result<string, string> =
    PackageCatalog.parseProgram false source
    |> Result.bind (fun program ->
        TypeChecking.checkPublicProgramWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            true
            CompilerOptions.defaultWarningSettings
            program
        |> Result.mapError CheckingDiagnostics.typeErrorToString)
    |> Result.map (fun (_, typedProgram, env) ->
        let planned = JsonPlanning.rewriteProgram env typedProgram
        let functionNames =
            planned
            |> CheckedAST.programSymbols
            |> CheckedAST.functionNames
            |> Map.values
            |> String.concat "\n"
        sprintf "%A\n%s" planned functionNames)

let testTypedDecodingUsesSharedViews
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    let source =
        "type JsonPlanningPerson = { name: String, scores: List<Int64> }\n"
        + "Stdlib.Json.parse<JsonPlanningPerson> \"{\\\"name\\\":\\\"A\\\",\\\"scores\\\":[1,2]}\""

    plannedSource stdlib source
    |> Result.bind (fun planned ->
        if not (planned.Contains "Darklang.Stdlib.Json.__parseRoot") then
            Error "Expected typed JSON parsing to begin with the shared reader"
        elif planned.Contains "Darklang.Stdlib.AltJson.__parseRaw" then
            Error "Typed JSON parsing still routes through the legacy RawJson tree parser"
        elif planned.Contains "Darklang.Stdlib.AltJson.InternalRawJson" then
            Error "Typed JSON decoding still generates functions over InternalRawJson"
        elif planned.Contains "__dark_json_view_" then
            Error "Typed JSON decoding still generates per-program view-list accessors"
        elif not (planned.Contains "Darklang.Stdlib.Json.__objectFieldMap") then
            Error "Expected record decoding to build its field index in one source-order pass"
        elif planned.Contains "Darklang.Stdlib.Json.__matchingViews" then
            Error "Record decoding still scans the object once per declared field"
        elif not (planned.Contains "Darklang.Stdlib.Json.__viewIsDuplicate") then
            Error "Expected record decoding to use allocation-free duplicate markers"
        elif not (planned.Contains "Darklang.Stdlib.Json.__arrayNext") then
            Error "Expected list decoding to consume the shared streaming array cursor"
        else
            Ok ())

let testTypedEncodingUsesSharedWriter
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    let source =
        "type JsonPlanningOutput = { name: String, scores: List<Int64> }\n"
        + "Stdlib.Json.serialize<JsonPlanningOutput> (JsonPlanningOutput { name = \"A\", scores = [1L, 2L] })"

    plannedSource stdlib source
    |> Result.bind (fun planned ->
        if not (planned.Contains "Darklang.Stdlib.Json.__writerEmpty") then
            Error "Expected typed JSON encoding to initialize the shared writer"
        elif not (planned.Contains "Darklang.Stdlib.Json.__writerFinish") then
            Error "Expected typed JSON encoding to finish the shared writer"
        elif not (planned.Contains "Darklang.Stdlib.Json.__writerFieldName") then
            Error "Expected record encoding to delegate field syntax to the shared writer"
        elif planned.Contains "StringConcat" then
            Error "Typed JSON encoding still generates string-concatenation plans"
        else
            Ok ())

let testNonJsonProgramIsUnchanged
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    PackageCatalog.parseProgram false "1L + 2L"
    |> Result.bind (fun program ->
        TypeChecking.checkPublicProgramWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            true
            CompilerOptions.defaultWarningSettings
            program
        |> Result.mapError CheckingDiagnostics.typeErrorToString)
    |> Result.bind (fun (_, typedProgram, env) ->
        let planned = JsonPlanning.rewriteProgram env typedProgram
        if planned = typedProgram then Ok ()
        else Error "Expected JSON planning to leave a program without JSON intrinsics unchanged")

let tests (stdlib: CompilationContexts.StdlibResult) = [
    ("typed JSON decoding uses shared value views", testTypedDecodingUsesSharedViews stdlib)
    ("typed JSON encoding uses the shared writer", testTypedEncodingUsesSharedWriter stdlib)
    ("JSON planning leaves non-JSON programs unchanged", testNonJsonProgramIsUnchanged stdlib)
]
