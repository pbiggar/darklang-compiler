(*
   TypeCheckingTestRunnerTests.ml - Unit tests for type checking test execution.
   Covers runner behavior that determines whether parsed test definitions pass
   or fail after invoking the compiler parser and type checker.
*)
(* Original type checking runner expectation test. *)
open TypeCheckingFormat
open TypeCheckingTestRunner
type testResult = (unit, string) result
let testExpectErrorRejectsParseErrors () =
 let test = {name = "parse errors do not satisfy type error expectation"; source = "let"; expectation = ExpectError} in
 let result = runTypeCheckingTest test in
 if result.success then Error "Expected parse error to fail an error expectation"
 else if result.expectedError <> true then Error "Expected result to preserve the error expectation"
 else if result.actualError = None then Error "Expected parse error details to be reported"
 else Ok ()
let tests = ["ExpectError rejects parse errors", testExpectErrorRejectsParseErrors]
