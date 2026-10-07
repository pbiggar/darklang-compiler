(*
   TypeCheckingTestRunner.ml - Runner for type checking tests
   Loads type checking test files, parses expressions, runs type checker,
   and compares results with expectations.
*)
(* Execute parsed type expectations through the direct source checker. *)
open Dark_compiler
open TypeCheckingFormat
(*
   Result of running a type checking test
*)
type typeCheckingTestResult = {success : bool; message : string; expectedType : AST.semanticType option; actualType : AST.semanticType option; expectedError : bool; actualError : string option}
(*
   Run a single type checking test
   Parse the source
   Type checking tests should only accept errors produced after parsing.
   Type check the program
   Both succeeded - check if types match
   Type check succeeded but error was expected
   Type error as expected
   Type check failed but success was expected
*)
let runTypeCheckingTest (test : typeCheckingTest) =
 let format = CheckingDiagnostics.typeToString in
 match WrittenParsing.parse Validation.Script test.source with
 | Error error -> (match test.expectation with
  | ExpectError -> {success = false; message = "Parse failed but type error was expected"; expectedType = None; actualType = None; expectedError = true; actualError = Some error}
  | ExpectType expected -> {success = false; message = "Parse failed but type " ^ format expected ^ " was expected"; expectedType = Some expected; actualType = None; expectedError = false; actualError = Some error})
 | Ok program -> match WrittenChecking.checkSourceUnits false false [program], test.expectation with
  | Ok (actual, _), ExpectType expected -> {success = actual = expected; message = (if actual = expected then "Type matches" else "Type mismatch: expected " ^ format expected ^ ", got " ^ format actual); expectedType = Some expected; actualType = Some actual; expectedError = false; actualError = None}
  | Ok (actual, _), ExpectError -> {success = false; message = "Type check succeeded with " ^ format actual ^ " but error was expected"; expectedType = None; actualType = Some actual; expectedError = true; actualError = None}
  | Error error, ExpectError -> {success = true; message = "Type error as expected"; expectedType = None; actualType = None; expectedError = true; actualError = Some error}
  | Error error, ExpectType expected -> {success = false; message = "Type check failed but " ^ format expected ^ " was expected: " ^ error; expectedType = Some expected; actualType = None; expectedError = false; actualError = Some error}
(*
   Run all tests from a test file
*)
let runTypeCheckingTestFile path = Result.map (List.map runTypeCheckingTest) (parseTypeCheckingTestFile path)
