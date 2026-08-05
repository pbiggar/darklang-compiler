// SyntaxTestRunner.fs - Executes syntax acceptance, formatting, and roundtrip fixtures.
//
// Compares parsed ASTs structurally, with normalization only for generated interpreter lambda seeds.

module TestDSL.SyntaxTestRunner

open System.IO
open System.Text.RegularExpressions
open AST
open TestDSL.Common
open TestDSL.PassTestRunner
open TestDSL.SyntaxFormat

let private parse (syntax: SyntaxKind) (source: string) : Result<AST.Program, string> =
    match syntax with
    | Compiler -> Parser.parseString false source
    | Interpreter -> InterpreterParser.parseString false source

let private prettySyntax = function
    | Compiler -> ASTPrettyPrinter.CompilerSyntax
    | Interpreter -> ASTPrettyPrinter.InterpreterSyntax

let private formatProgram (syntax: SyntaxKind) (program: AST.Program) : Result<string, string> =
    try
        Ok (ASTPrettyPrinter.formatProgram (prettySyntax syntax) program)
    with ex ->
        Error ex.Message

let private normalizedAst (program: AST.Program) : string =
    let debug = sprintf "%A" program
    Regex.Replace(debug, "__interp_lambda_[0-9]+_", "__interp_lambda_N_")

let private astsEqual (left: AST.Program) (right: AST.Program) : bool =
    left = right || normalizedAst left = normalizedAst right

let private success : PassTestResult =
    { Success = true; Message = "Test passed"; Expected = None; Actual = None }

let private failure message expected actual : PassTestResult =
    { Success = false; Message = message; Expected = expected; Actual = actual }

let private checkFormat (test: SyntaxTest) (ast: AST.Program) : PassTestResult =
    match test.FormatAs, test.ExpectedFormat with
    | None, None -> success
    | Some syntax, Some expected ->
        match formatProgram syntax ast with
        | Error msg -> failure $"Formatting failed: {msg}" (Some expected) None
        | Ok actual when normalizeLineEndings (actual.Trim()) = normalizeLineEndings (expected.Trim()) -> success
        | Ok actual -> failure "Formatted syntax did not match" (Some expected) (Some actual)
    | _ -> failure "Invalid syntax fixture formatting state" None None

let private checkRoundtrips (test: SyntaxTest) (ast: AST.Program) : PassTestResult =
    let rec loop remaining =
        match remaining with
        | [] -> success
        | syntax :: rest ->
            match formatProgram syntax ast with
            | Error msg -> failure $"Roundtrip formatting failed: {msg}" None None
            | Ok formatted ->
                match parse syntax formatted with
                | Error msg -> failure $"Roundtrip reparse failed: {msg}" (Some formatted) None
                | Ok reparsed when astsEqual ast reparsed -> loop rest
                | Ok reparsed ->
                    failure
                        "AST changed after syntax roundtrip"
                        (Some (normalizedAst ast))
                        (Some (normalizedAst reparsed))
    loop test.RoundtripAs

let runSyntaxTest (test: SyntaxTest) : PassTestResult =
    match parse test.ParseAs test.Source, test.ExpectedError with
    | Error msg, Some expected when msg.Contains expected -> success
    | Error msg, Some expected ->
        failure "Parse error did not contain expected text" (Some expected) (Some msg)
    | Ok _, Some expected ->
        failure "Expected syntax parsing to fail" (Some expected) (Some "Parsing succeeded")
    | Error msg, None -> failure $"Syntax parsing failed: {msg}" None None
    | Ok ast, None ->
        match checkFormat test ast with
        | { Success = false } as result -> result
        | _ -> checkRoundtrips test ast

let loadSyntaxTests (path: string) : Result<SyntaxTest list, string> =
    if not (File.Exists path) then
        Error $"Syntax test file not found: {path}"
    else
        try
            File.ReadAllText path
            |> parseSyntaxFileContent path
        with ex ->
            Error $"Failed to read syntax test file {path}: {ex.Message}"

let tests (testFiles: string array) : (string * (unit -> Result<unit, string>)) list =
    let testsForFile path =
        match loadSyntaxTests path with
        | Error msg ->
            [ ($"parse {Path.GetFileName path}", fun () -> Error msg) ]
        | Ok cases ->
            cases
            |> List.map (fun test ->
                (test.Name,
                 fun () ->
                    let result = runSyntaxTest test
                    if result.Success then Ok ()
                    else
                        match result.Expected, result.Actual with
                        | Some expected, Some actual ->
                            Error $"{result.Message}\nExpected:\n{expected}\nActual:\n{actual}"
                        | _ -> Error result.Message))

    testFiles
    |> Array.sort
    |> Array.toList
    |> List.collect testsForFile
