// OptimizationFormatTests.fs - Unit tests for optimization test parsing
//
// Verifies the optimization test file parser accepts repository test syntax
// across common line-ending formats.

module OptimizationFormatTests

open System
open System.IO
open TestDSL.OptimizationFormat

type TestResult = Result<unit, string>

let private withTempFile (content: string) (test: string -> TestResult) : TestResult =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid()}.opt")
    File.WriteAllText(path, content)
    try
        test path
    finally
        if File.Exists(path) then
            File.Delete(path)

let testParseCRLFOptimizationFile () : TestResult =
    let content =
        [
            "---NAME---"
            "fold_add"
            "---INPUT---"
            "1 + 2"
            "---EXPECTED---"
            "return 3"
        ]
        |> String.concat "\r\n"

    withTempFile content (fun path ->
        match parseTestFile ANF path with
        | Ok [ test ] when test.Name = "fold_add" && test.Source = "1 + 2" && test.ExpectedIR = "return 3" ->
            Ok ()
        | Ok tests ->
            Error $"Expected one parsed CRLF optimization test, got {List.length tests}"
        | Error msg ->
            Error $"Expected CRLF optimization test file to parse, got: {msg}")

let testUnknownOptimizationSectionFails () : TestResult =
    let content =
        [
            "---NAME---"
            "fold_add"
            "---INPUT---"
            "1 + 2"
            "---EXPECTED---"
            "return 3"
            "---OUTPUT---"
            "ignored"
        ]
        |> String.concat "\n"

    withTempFile content (fun path ->
        match parseTestFile ANF path with
        | Error msg when msg.Contains("Unknown optimization section: OUTPUT") ->
            Ok ()
        | Ok _ ->
            Error "Expected unknown optimization section to fail"
        | Error msg ->
            Error $"Expected unknown section error, got: {msg}")

let tests = [
    ("parse CRLF optimization file", testParseCRLFOptimizationFile)
    ("unknown optimization section fails", testUnknownOptimizationSectionFails)
]

let runAll () : TestResult =
    let rec run remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> run rest
            | Error msg -> Error $"{name} test failed: {msg}"
    run tests
