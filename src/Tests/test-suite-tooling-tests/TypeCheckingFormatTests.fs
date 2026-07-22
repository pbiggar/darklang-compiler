// TypeCheckingFormatTests.fs - Unit tests for type checking test parsing
//
// Verifies parser behavior for the line-based type checking test format.

module TypeCheckingFormatTests

open System
open System.Globalization
open System.IO
open TestDSL.TypeCheckingFormat

type TestResult = Result<unit, string>

let private withTempFile (content: string) (test: string -> TestResult) : TestResult =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid()}.typecheck")
    File.WriteAllText(path, content)
    try
        test path
    finally
        if File.Exists(path) then
            File.Delete(path)

let testParsesSlashSlashInsideStringLiteral () : TestResult =
    let content = "\"https://darklang.com\" : string  // string containing URL"

    withTempFile content (fun path ->
        match parseTypeCheckingTestFile path with
        | Ok [ test ] when test.Source = "\"https://darklang.com\"" ->
            Ok ()
        | Ok [ test ] ->
            Error $"Expected source to preserve // inside string literal, got: {test.Source}"
        | Ok tests ->
            Error $"Expected one parsed type checking test, got {List.length tests}"
        | Error msg ->
            Error $"Expected type checking test with // inside string literal to parse, got: {msg}")

let testParsesTypeKeywordsWithCultureInvariantCasing () : TestResult =
    let content = "1 : INT"
    let originalCulture = CultureInfo.CurrentCulture
    let originalUICulture = CultureInfo.CurrentUICulture

    try
        CultureInfo.CurrentCulture <- CultureInfo.GetCultureInfo("tr-TR")
        CultureInfo.CurrentUICulture <- CultureInfo.GetCultureInfo("tr-TR")

        withTempFile content (fun path ->
            match parseTypeCheckingTestFile path with
            | Ok [ test ] ->
                match test.Expectation with
                | ExpectType AST.TInt64 -> Ok ()
                | other -> Error $"Expected INT to parse as int64, got: {other}"
            | Ok tests ->
                Error $"Expected one parsed type checking test, got {List.length tests}"
            | Error msg ->
                Error $"Expected uppercase INT to parse under Turkish culture, got: {msg}")
    finally
        CultureInfo.CurrentCulture <- originalCulture
        CultureInfo.CurrentUICulture <- originalUICulture

let tests = [
    ("parse // inside type checking string literal", testParsesSlashSlashInsideStringLiteral)
    ("parse type keywords with culture-invariant casing", testParsesTypeKeywordsWithCultureInvariantCasing)
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
