// TypeCheckingTestRunner.fs - Runner for type checking tests
//
// Loads type checking test files, parses expressions, runs type checker,
// and compares results with expectations.

module TestDSL.TypeCheckingTestRunner

open System.IO
open TestDSL.TypeCheckingFormat
open TypeChecking
open AST
open Parser

/// Result of running a type checking test
type TypeCheckingTestResult = {
    Success: bool
    Message: string
    ExpectedType: Type option
    ActualType: Type option
    ExpectedError: bool
    ActualError: string option
}

/// Run a single type checking test
let runTypeCheckingTest (test: TypeCheckingTest) : TypeCheckingTestResult =
    // Parse the source
    let parseResult = parseString test.Source

    match parseResult with
    | Error parseErr ->
        // Parse error - check if error was expected
        match test.Expectation with
        | ExpectError ->
            { Success = true
              Message = "Parse error as expected"
              ExpectedType = None
              ActualType = None
              ExpectedError = true
              ActualError = Some parseErr }
        | ExpectType expectedType ->
            { Success = false
              Message = $"Parse failed but type {typeToString expectedType} was expected"
              ExpectedType = Some expectedType
              ActualType = None
              ExpectedError = false
              ActualError = Some parseErr }

    | Ok program ->
        // Type check the program
        let typeCheckResult = checkProgram program

        match typeCheckResult, test.Expectation with
        | Ok actualType, ExpectType expectedType ->
            // Both succeeded - check if types match
            if actualType = expectedType then
                { Success = true
                  Message = "Type matches"
                  ExpectedType = Some expectedType
                  ActualType = Some actualType
                  ExpectedError = false
                  ActualError = None }
            else
                { Success = false
                  Message = $"Type mismatch: expected {typeToString expectedType}, got {typeToString actualType}"
                  ExpectedType = Some expectedType
                  ActualType = Some actualType
                  ExpectedError = false
                  ActualError = None }

        | Ok actualType, ExpectError ->
            // Type check succeeded but error was expected
            { Success = false
              Message = $"Type check succeeded with {typeToString actualType} but error was expected"
              ExpectedType = None
              ActualType = Some actualType
              ExpectedError = true
              ActualError = None }

        | Error typeErr, ExpectError ->
            // Type error as expected
            { Success = true
              Message = "Type error as expected"
              ExpectedType = None
              ActualType = None
              ExpectedError = true
              ActualError = Some (typeErrorToString typeErr) }

        | Error typeErr, ExpectType expectedType ->
            // Type check failed but success was expected
            { Success = false
              Message = $"Type check failed but {typeToString expectedType} was expected: {typeErrorToString typeErr}"
              ExpectedType = Some expectedType
              ActualType = None
              ExpectedError = false
              ActualError = Some (typeErrorToString typeErr) }

/// Run all tests from a test file
let runTypeCheckingTestFile (path: string) : Result<TypeCheckingTestResult list, string> =
    match parseTypeCheckingTestFile path with
    | Error e -> Error e
    | Ok tests ->
        Ok (List.map runTypeCheckingTest tests)
