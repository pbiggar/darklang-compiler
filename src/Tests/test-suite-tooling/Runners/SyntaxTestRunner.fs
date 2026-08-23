// SyntaxTestRunner.fs - Runs canonical interpreter syntax fixtures.
module TestDSL.SyntaxTestRunner
open System.IO
open System.Text.RegularExpressions
open AST
open TestDSL.Common
open TestDSL.PassTestRunner
open TestDSL.SyntaxFormat

let private parse source = InterpreterParser.parseString false source
let private format program = ASTPrettyPrinter.formatProgram ASTPrettyPrinter.InterpreterSyntax program
let private normalized (program: Program) = Regex.Replace(sprintf "%A" program, "__interp_lambda_[0-9]+_", "__interp_lambda_N_")
let private result success message expected actual : PassTestResult = { Success = success; Message = message; Expected = expected; Actual = actual }

let runSyntaxTest test =
    match parse test.Source, test.ExpectedError with
    | Error error, Some expected when error.Contains expected -> result true "Test passed" None None
    | Error error, Some expected -> result false "Parse error did not contain expected text" (Some expected) (Some error)
    | Ok _, Some expected -> result false "Expected syntax parsing to fail" (Some expected) (Some "Parsing succeeded")
    | Error error, None -> result false $"Syntax parsing failed: {error}" None None
    | Ok ast, None ->
        let formatted = format ast
        match test.ExpectedFormat with
        | Some expected when normalizeLineEndings (formatted.Trim()) <> normalizeLineEndings (expected.Trim()) -> result false "Formatted syntax did not match" (Some expected) (Some formatted)
        | _ when not test.Roundtrip -> result true "Test passed" None None
        | _ ->
            match parse formatted with
            | Error error -> result false $"Roundtrip reparse failed: {error}" (Some formatted) None
            | Ok reparsed when normalized ast = normalized reparsed -> result true "Test passed" None None
            | Ok reparsed -> result false "AST changed after syntax roundtrip" (Some (normalized ast)) (Some (normalized reparsed))

let loadSyntaxTests path =
    if not (File.Exists path) then Error $"Syntax test file not found: {path}"
    else try File.ReadAllText path |> parseSyntaxFileContent path with ex -> Error $"Failed to read syntax test file {path}: {ex.Message}"

let tests (testFiles: string array) =
    testFiles |> Array.sort |> Array.toList |> List.collect (fun path ->
        match loadSyntaxTests path with
        | Error msg -> [ ($"parse {Path.GetFileName path}", fun () -> Error msg) ]
        | Ok cases -> cases |> List.map (fun test -> test.Name, fun () ->
            let outcome = runSyntaxTest test
            if outcome.Success then Ok () else Error (match outcome.Expected, outcome.Actual with | Some expected, Some actual -> $"{outcome.Message}\nExpected:\n{expected}\nActual:\n{actual}" | _ -> outcome.Message)))
