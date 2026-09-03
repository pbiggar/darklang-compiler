// JsonPlanningTests.fs - structural tests for typed JSON specialization.

module JsonPlanningTests

open AST

type TestResult = Result<unit, string>

let private plannedSource
    (stdlib: CompilerLibrary.StdlibResult)
    (source: string)
    : Result<string, string> =
    CompilerLibrary.parseProgram false source
    |> Result.bind (fun program ->
        TypeChecking.checkPublicProgramWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            true
            CompilerLibrary.defaultWarningSettings
            program
        |> Result.mapError TypeChecking.typeErrorToString)
    |> Result.map (fun (_, typedProgram, env) ->
        JsonPlanning.rewriteProgram env typedProgram
        |> ASTPrettyPrinter.formatProgram ASTPrettyPrinter.InterpreterSyntax)

let testTypedDecodingUsesSharedViews
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    let source =
        "type JsonPlanningPerson = { name: String, scores: List<Int64> }\n"
        + "Stdlib.Json.parse<JsonPlanningPerson>(\"{\\\"name\\\":\\\"A\\\",\\\"scores\\\":[1,2]}\")"

    plannedSource stdlib source
    |> Result.bind (fun planned ->
        if not (planned.Contains "Stdlib.Json.__parseRoot") then
            Error "Expected typed JSON parsing to begin with the shared reader"
        elif planned.Contains "Stdlib.AltJson.__parseRaw" then
            Error "Typed JSON parsing still routes through the legacy RawJson tree parser"
        elif planned.Contains "Stdlib.AltJson.InternalRawJson" then
            Error "Typed JSON decoding still generates functions over InternalRawJson"
        elif planned.Contains "__dark_json_view_" then
            Error "Typed JSON decoding still generates per-program view-list accessors"
        elif not (planned.Contains "Stdlib.Json.__objectFieldMap") then
            Error "Expected record decoding to build its field index in one source-order pass"
        elif planned.Contains "Stdlib.Json.__matchingViews" then
            Error "Record decoding still scans the object once per declared field"
        elif not (planned.Contains "Stdlib.Json.__viewIsDuplicate") then
            Error "Expected record decoding to use allocation-free duplicate markers"
        elif not (planned.Contains "Stdlib.Json.__arrayNext") then
            Error "Expected list decoding to consume the shared streaming array cursor"
        else
            Ok ())

let testTypedEncodingUsesSharedWriter
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    let source =
        "type JsonPlanningOutput = { name: String, scores: List<Int64> }\n"
        + "Stdlib.Json.serialize<JsonPlanningOutput>(JsonPlanningOutput { name = \"A\", scores = [1L, 2L] })"

    plannedSource stdlib source
    |> Result.bind (fun planned ->
        if not (planned.Contains "Stdlib.Json.__writerEmpty") then
            Error "Expected typed JSON encoding to initialize the shared writer"
        elif not (planned.Contains "Stdlib.Json.__writerFinish") then
            Error "Expected typed JSON encoding to finish the shared writer"
        elif not (planned.Contains "Stdlib.Json.__writerFieldName") then
            Error "Expected record encoding to delegate field syntax to the shared writer"
        elif planned.Contains " ++ " then
            Error "Typed JSON encoding still generates string-concatenation plans"
        else
            Ok ())

let tests (stdlib: CompilerLibrary.StdlibResult) = [
    ("typed JSON decoding uses shared value views", testTypedDecodingUsesSharedViews stdlib)
    ("typed JSON encoding uses the shared writer", testTypedEncodingUsesSharedWriter stdlib)
]
