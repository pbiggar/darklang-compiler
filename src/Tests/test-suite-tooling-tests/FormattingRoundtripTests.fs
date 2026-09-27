// FormattingRoundtripTests.fs - Focused parser/pretty roundtrip regression tests.
//
// Loads minimal expressions from data files so new regression cases do not
// require recompiling tests.

module FormattingRoundtripTests

open System.IO
open TestDSL.FormattingRoundtripFormat

type TestResult = Result<unit, string>

let private roundtripSyntax (testCase: FormattingRoundtripCase) : TestResult =
    let parse source =
        WrittenParsing.parse LibParser.Validation.Script source
        |> Result.map LibParser.Validation.ValidatedSourceFile.toWrittenTypes
    match parse testCase.Source with
    | Error err ->
        Error (
            $"Initial parse failed.\nFile: {testCase.SourceFile}\nTest: {testCase.Name}\n"
            + $"Source: {testCase.Source}\nError: {err}"
        )
    | Ok ast0 ->
        let printed0 = WrittenFormatter.format testCase.Source ast0
        match parse printed0 with
        | Error err ->
            Error (
                $"Re-parse failed.\nFile: {testCase.SourceFile}\nTest: {testCase.Name}\n"
                + $"Source: {testCase.Source}\nPretty: {printed0}\nError: {err}"
            )
        | Ok ast1 ->
            let printed1 = WrittenFormatter.format printed0 ast1
            if WrittenFormatter.syntaxKey ast0 <> WrittenFormatter.syntaxKey ast1 then
                Error (
                    "AST changed after roundtrip.\n"
                    + $"File: {testCase.SourceFile}\n"
                    + $"Test: {testCase.Name}\n"
                    + $"Source: {testCase.Source}\n"
                    + $"Pretty (pass 1): {printed0}\n"
                    + $"Pretty (pass 2): {printed1}"
                )
            elif printed0 <> printed1 then
                Error (
                    "Pretty-printer output was not idempotent.\n"
                    + $"File: {testCase.SourceFile}\n"
                    + $"Test: {testCase.Name}\n"
                    + $"Source: {testCase.Source}\n"
                    + $"Pretty (pass 1): {printed0}\n"
                    + $"Pretty (pass 2): {printed1}"
                )
            else
                Ok ()

let private parseFailureTest (path: string) (msg: string) : string * (unit -> TestResult) =
    let label = Path.GetFileName path
    ($"parse {label}", fun () -> Error msg)

let tests (testFiles: string array) : (string * (unit -> TestResult)) list =
    testFiles
    |> Array.sort
    |> Array.toList
    |> List.collect (fun path ->
        match parseFormattingRoundtripFile path with
        | Ok parsed ->
            parsed
            |> List.map (fun caseData -> (caseData.Name, fun () -> roundtripSyntax caseData))
        | Error msg ->
            [ parseFailureTest path msg ])
