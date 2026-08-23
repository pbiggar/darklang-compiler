// FormattingRoundtripTests.fs - Focused parser/pretty roundtrip regression tests.
//
// Loads minimal expressions from data files so new regression cases do not
// require recompiling tests.

module FormattingRoundtripTests

open System.IO
open AST
open ASTPrettyPrinter
open TestDSL.FormattingRoundtripFormat

type TestResult = Result<unit, string>

let private roundtripInterpreterSyntax (testCase: FormattingRoundtripCase) : TestResult =
    match InterpreterParser.parseString false testCase.Source with
    | Error err ->
        Error (
            $"Initial parse failed.\nFile: {testCase.SourceFile}\nTest: {testCase.Name}\n"
            + $"Source: {testCase.Source}\nError: {err}"
        )
    | Ok ast0 ->
        let printed0 = ASTPrettyPrinter.formatProgram InterpreterSyntax ast0
        match InterpreterParser.parseString false printed0 with
        | Error err ->
            Error (
                $"Re-parse failed.\nFile: {testCase.SourceFile}\nTest: {testCase.Name}\n"
                + $"Source: {testCase.Source}\nPretty: {printed0}\nError: {err}"
            )
        | Ok ast1 ->
            let printed1 = ASTPrettyPrinter.formatProgram InterpreterSyntax ast1
            if ast0 <> ast1 then
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
            |> List.map (fun caseData -> (caseData.Name, fun () -> roundtripInterpreterSyntax caseData))
        | Error msg ->
            [ parseFailureTest path msg ])
