// TypeCheckingTestRunnerTests.fs - Unit tests for type checking test execution.
//
// Covers runner behavior that determines whether parsed test definitions pass
// or fail after invoking the compiler parser and type checker.

module TypeCheckingTestRunnerTests

open TestDSL.TypeCheckingFormat
open TestDSL.TypeCheckingTestRunner

type TestResult = Result<unit, string>

let testExpectErrorRejectsParseErrors () : TestResult =
    let test =
        { Name = "parse errors do not satisfy type error expectation"
          Source = "let"
          Expectation = ExpectError }

    let result = runTypeCheckingTest test

    if result.Success then
        Error "Expected parse error to fail an error expectation"
    elif result.ExpectedError <> true then
        Error "Expected result to preserve the error expectation"
    elif result.ActualError = None then
        Error "Expected parse error details to be reported"
    else
        Ok ()

let tests = [
    ("ExpectError rejects parse errors", testExpectErrorRejectsParseErrors)
]
