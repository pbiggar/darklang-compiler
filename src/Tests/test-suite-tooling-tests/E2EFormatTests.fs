// E2EFormatTests.fs - Unit tests for E2E DSL parsing
//
// Verifies parser behavior for multi-line E2E test forms used by upstream .dark files.

module E2EFormatTests

open System
open System.IO
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner

type TestResult = Result<unit, string>

let private withTempFileNamed (fileName: string) (contents: string) (f: string -> TestResult) : TestResult =
    let tempDir = Path.Combine(Path.GetTempPath(), $"dark-e2eformat-{Guid.NewGuid():N}")
    let tempPath = Path.Combine(tempDir, fileName)

    try
        Directory.CreateDirectory(tempDir) |> ignore
        File.WriteAllText(tempPath, contents)
        f tempPath
    finally
        if Directory.Exists(tempDir) then
            Directory.Delete(tempDir, true)

let private withTempFile (contents: string) (f: string -> TestResult) : TestResult =
    withTempFileNamed "test.dark" contents f

let testParsesMultilineExpectationOnNextLine () : TestResult =
    let testSource =
        "(if true then Builtin.testRuntimeError \"a\" else 0L) =\n"
        + "  error=\"Uncaught exception: a\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline .dark-style test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.Contains("Builtin.testRuntimeError")) then
                    Error $"Expected parsed source to contain runtime error expression, got: {test.Source}"
                elif test.ExpectedValueExpr <> None then
                    Error "Expected error= expectation to be parsed as error expectation"
                elif test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1, got {test.ExpectedExitCode}"
                elif test.ExpectedErrorMessage <> Some "Uncaught exception: a" then
                    Error $"Expected error message parse, got: {test.ExpectedErrorMessage}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesSkipAttribute () : TestResult =
    let testSource = "(if true then 1L else 2L) = skip=\"temporarily unsupported\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected skip attribute test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                match test.SkipReason with
                | Some reason when reason = "temporarily unsupported" ->
                    Ok ()
                | Some reason ->
                    Error $"Expected skip reason 'temporarily unsupported', got '{reason}'"
                | None ->
                    Error "Expected skip reason to be parsed"
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}"
    )

let testParsesIndentedMultilineDarkTestsInsideModule () : TestResult =
    let testSource =
        "module Int64 =\n"
        + "  (match 6L with\n"
        + "   | 6L -> \"pass\"\n"
        + "   | _ -> \"fail\") = \"pass\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected indented .dark multiline test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.Source <> "match 6L with\n | 6L -> \"pass\"\n | _ -> \"fail\"" then
                    Error $"Unexpected parsed source: {test.Source}"
                elif test.ExpectedValueExpr <> Some "\"pass\"" then
                    Error $"Expected value-expression RHS '\"pass\"', got: {test.ExpectedValueExpr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected .dark module declarations to be excluded from preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineWithInnerDoubleEquals () : TestResult =
    let testSource =
        "(match 5L with\n"
        + " | x when (x + 1L) == 6L -> true\n"
        + " | 5L -> false) =\n"
        + "  true\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline test with inner == to parse, but got error: {msg}"
        | Ok tests ->
            if tests.Length <> 1 then
                Error $"Expected exactly 1 parsed test, got {tests.Length}"
            elif tests.[0].ExpectedValueExpr <> Some "true" then
                Error $"Expected value-expression RHS 'true', got: {tests.[0].ExpectedValueExpr}"
            else
                Ok ())

let testParsesMultilineWithInnerLetBinding () : TestResult =
    let testSource =
        "(let x = 4L\n"
        + " match x with\n"
        + " | 1L | 2L | 3L | 4L -> \"pass\"\n"
        + " | _ -> \"fail\") = \"pass\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline let-binding test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.StartsWith("let x = 4L")) then
                    Error $"Unexpected parsed source: {test.Source}"
                elif test.ExpectedValueExpr <> Some "\"pass\"" then
                    Error $"Expected value-expression RHS '\"pass\"', got: {test.ExpectedValueExpr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesIndentedMultilineLetWithoutModuleIndentLeak () : TestResult =
    let testSource =
        "module GenericTypeArgsAreOK =\n"
        + "  (let segments =\n"
        + "    [ \"a\"; \"b\" ]\n"
        + "  String.join \"\" segments) = \"ab\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected indented multiline let test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                let expectedSource =
                    "let segments =\n"
                    + "  [ \"a\"; \"b\" ]\n"
                    + "String.join \"\" segments"

                if test.Source <> expectedSource then
                    Error $"Unexpected parsed source: {test.Source}"
                elif test.ExpectedValueExpr <> Some "\"ab\"" then
                    Error $"Expected value-expression RHS '\"ab\"', got: {test.ExpectedValueExpr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesExpectationWithKeywordInsideQuotedMessage () : TestResult =
    let testSource =
        "module Errors =\n"
        + "  (match \"nothing matches\" with\n"
        + "   | \"not this\" -> \"fail\") = error=\"No matching case found for value \\\"nothing matches\\\" in match expression\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected quoted-message expectation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1, got {test.ExpectedExitCode}"
                elif test.ExpectedErrorMessage <> Some "No matching case found for value \"nothing matches\" in match expression" then
                    Error $"Unexpected error message parse: {test.ExpectedErrorMessage}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineExpectationWithFunctionHeadAndNextLineArg () : TestResult =
    let testSource =
        "(match 1L with\n"
        + " | 1L -> \"wrong number\") =\n"
        + "  error=\"No matching case found\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline function-head expectation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1, got {test.ExpectedExitCode}"
                elif test.ExpectedErrorMessage <> Some "No matching case found" then
                    Error $"Unexpected error message parse: {test.ExpectedErrorMessage}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesBareE2ERightHandSideAsValueExpression () : TestResult =
    let testSource = "2 + 3 = 5\n"

    withTempFileNamed "test.e2e" testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected bare .e2e RHS to parse as value expression, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedValueExpr <> Some "5" then
                    Error $"Expected value-expression RHS '5', got: {test.ExpectedValueExpr}"
                elif test.ExpectedStdout <> None then
                    Error $"Expected no stdout expectation for bare RHS, got: {test.ExpectedStdout}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble () : TestResult =
    let testSource =
        "(Stdlib.Option.Option.None |> Stdlib.Option.Option.Some) = Stdlib.Option.Option.Some\n"
        + "  Stdlib.Option.Option.None\n"
        + "(1L) = 1L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected RHS continuation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ first; second ] ->
                if first.ExpectedValueExpr <> Some "Stdlib.Option.Option.Some\n  Stdlib.Option.Option.None" then
                    Error $"Unexpected first RHS parse: {first.ExpectedValueExpr}"
                elif first.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for first test, got: {first.Preamble}"
                elif second.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for second test, got: {second.Preamble}"
                elif second.ExpectedValueExpr <> Some "1L" then
                    Error $"Unexpected second RHS parse: {second.ExpectedValueExpr}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 2 parsed tests, got {tests.Length}")

let testParsesDottedRhsContinuationAfterBareIdentifierHead () : TestResult =
    let testSource =
        "(let _ = 1L in 2L) = Stdlib\n"
        + "  .Option\n"
        + "  .Option\n"
        + "  .Some(2L)\n"
        + "(3L) = 3L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected dotted RHS continuation after bare identifier head, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ first; second ] ->
                if first.ExpectedValueExpr <> Some "Stdlib\n  .Option\n  .Option\n  .Some(2L)" then
                    Error $"Unexpected first RHS parse: {first.ExpectedValueExpr}"
                elif first.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for first test, got: {first.Preamble}"
                elif second.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for second test, got: {second.Preamble}"
                elif second.ExpectedValueExpr <> Some "3L" then
                    Error $"Unexpected second RHS parse: {second.ExpectedValueExpr}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 2 parsed tests, got {tests.Length}")

let testParsesMultilinePipeBeforeSeparator () : TestResult =
    let testSource =
        "(Stdlib.List.map_v0 [ 1L; 2L ] (fun x -> x + 1L))\n"
        + "|> Stdlib.List.length_v0 = 2L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline pipe test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.Contains("|> Stdlib.List.length_v0")) then
                    Error $"Expected source to preserve pipe continuation, got: {test.Source}"
                elif test.ExpectedValueExpr <> Some "2L" then
                    Error $"Unexpected RHS parse: {test.ExpectedValueExpr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineWithoutLeadingParen () : TestResult =
    let testSource =
        "Ctor\n"
        + "  { a = 1L\n"
        + "    b = 2L } = Ctor\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline non-paren test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.StartsWith("Ctor\n")) then
                    Error $"Unexpected source parse: {test.Source}"
                elif not (test.Source.Contains("b = 2L }")) then
                    Error $"Expected multiline record body in source, got: {test.Source}"
                elif test.ExpectedValueExpr <> Some "Ctor" then
                    Error $"Unexpected RHS parse: {test.ExpectedValueExpr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineListRhsContinuation () : TestResult =
    let testSource =
        "module AccessDataInGenericField =\n"
        + "  (Stdlib.DB.query TestRecordWithGenericThing (fun p -> p.name == \"joe\")) = [ RecordWithGenericThing\n"
        + "                                                                                { name =\n"
        + "                                                                                    \"joe\" } ]\n"
        + "  (1L) = 1L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline list RHS continuation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ first; second ] ->
                if first.ExpectedValueExpr <> Some "[ RecordWithGenericThing\n                                                                                { name =\n                                                                                    \"joe\" } ]" then
                    Error $"Unexpected first RHS parse: {first.ExpectedValueExpr}"
                elif first.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for first test, got: {first.Preamble}"
                elif second.ExpectedValueExpr <> Some "1L" then
                    Error $"Unexpected second RHS parse: {second.ExpectedValueExpr}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 2 parsed tests, got {tests.Length}")

let testParsesSingleLineDottedRhsWithoutConsumingNextTest () : TestResult =
    let testSource =
        "module GetMany =\n"
        + "  (1L) = Stdlib.Option.Option.None\n"
        + "\n"
        + "  (2L) = 2L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected single-line dotted RHS to parse without continuation, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ first; second ] ->
                if first.ExpectedValueExpr <> Some "Stdlib.Option.Option.None" then
                    Error $"Unexpected first RHS parse: {first.ExpectedValueExpr}"
                elif second.ExpectedValueExpr <> Some "2L" then
                    Error $"Unexpected second RHS parse: {second.ExpectedValueExpr}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 2 parsed tests, got {tests.Length}")

let testParsesMultilineDictExpectationWithoutPreambleLeakage () : TestResult =
    let testSource =
        "module GetManyWithKeys =\n"
        + "  (let one = 1L\n"
        + "   let two = 2L\n"
        + "   one) =\n"
        + "    Dict\n"
        + "      { one = 1L\n"
        + "        two = 2L }\n"
        + "\n"
        + "  (3L) = 3L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline Dict RHS to parse without preamble leakage, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ first; second ] ->
                if first.ExpectedValueExpr <> Some "Dict\n    { one = 1L\n      two = 2L }" then
                    Error $"Unexpected first RHS parse: {first.ExpectedValueExpr}"
                elif first.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for first test, got: {first.Preamble}"
                elif second.ExpectedValueExpr <> Some "3L" then
                    Error $"Unexpected second RHS parse: {second.ExpectedValueExpr}"
                elif second.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for second test, got: {second.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 2 parsed tests, got {tests.Length}")

let testParsesSqlErrorExpectationShorthand () : TestResult =
    let testSource =
        "friendsError (fun p -> \"x\") = sqlerror=\"Incorrect type in String \\\"x\\\"\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected sqlerror shorthand expectation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedValueExpr <> None then
                    Error $"Expected no value-expression RHS for sqlerror shorthand, got: {test.ExpectedValueExpr}"
                elif test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1 for sqlerror shorthand, got: {test.ExpectedExitCode}"
                elif test.ExpectedStdout <> Some "" then
                    Error $"Expected stdout empty-string expectation for sqlerror shorthand, got: {test.ExpectedStdout}"
                elif test.ExpectedStderr <> Some "Incorrect type in String \"x\"" then
                    Error $"Expected stderr parse for sqlerror shorthand, got: {test.ExpectedStderr}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesEscapedBackslashBeforeNAsLiteralText () : TestResult =
    let testSource = "print \"ignored\" = stdout=\"\\\\n\"\n"

    withTempFileNamed "test.e2e" testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected escaped backslash-n stdout to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedStdout <> Some "\\n" then
                    Error $"Expected stdout to contain literal backslash-n, got: {test.ExpectedStdout}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesRepeatedProcessArgumentsInOrder () : TestResult =
    let testSource = "Stdlib.Cli.Args.int64(0I) = Ok(100L) arg=\"100\" arg=\"two words\"\n"

    withTempFileNamed "test.e2e" testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected process arguments to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] when test.Arguments = ["100"; "two words"] -> Ok ()
            | [ test ] -> Error $"Unexpected process arguments: {test.Arguments}"
            | _ -> Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testBuildsParseableUniversalBatchSource () : TestResult =
    let testSource = "1L + 1L = 2L\n2L + 2L = 4L\n"

    withTempFileNamed "ordinary.e2e" testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg -> Error $"Expected batch fixture to parse, but got error: {msg}"
        | Ok tests ->
            let prepared = tests |> List.choose tryPrepareBatchTest
            if prepared.Length <> 2 then
                Error $"Expected 2 universally batchable tests, got {prepared.Length}"
            elif not (canBatchTogether prepared.[0] prepared.[1]) then
                Error "Expected adjacent tests with the same context and options to batch together"
            else
                let source = buildBatchSource prepared
                match CompilerLibrary.parseProgram false source with
                | Error msg -> Error $"Generated batch source did not parse: {msg}\n{source}"
                | Ok _ when not (source.Contains("let e2eBatchCaseRun(check: (Unit) -> Bool): Bool =")) ->
                    Error $"Generated batch did not include its non-inlineable closure runner:\n{source}"
                | Ok _ when not (source.Contains("let e2eBatchCaseResult0 =")) ->
                    Error $"Generated batch did not eagerly bind the first result:\n{source}"
                | Ok _ when not (source.EndsWith("(if e2eBatchCaseResult1 then 2L else 0L)")) ->
                    Error $"Generated batch did not encode the final result bit:\n{source}"
                | Ok _ -> Ok ())

let testParsesBatchBitmaskResults () : TestResult =
    match tryParseBatchBoolResults 3 "ignored output\n5\n" with
    | Some [true; false; true] -> Ok ()
    | other -> Error $"Expected bitmask 5 to decode as [true; false; true], got {other}"

let testRejectsInvalidBatchBitmaskResults () : TestResult =
    match tryParseBatchBoolResults 3 "8\n" with
    | None -> Ok ()
    | Some values -> Error $"Expected out-of-range result bits to be rejected, got {values}"

let testDoesNotBatchTestsWithProcessInputs () : TestResult =
    let testSource = "Stdlib.Cli.Args.int64(0I) = Ok(100L) arg=\"100\"\n"
    withTempFileNamed "ordinary.e2e" testSource (fun path ->
        match parseE2ETestFile path with
        | Ok [test] when Option.isNone (tryPrepareBatchTest test) -> Ok ()
        | Ok [test] -> Error $"Expected argument-bearing test to remain isolated: {test}"
        | Ok tests -> Error $"Expected exactly 1 parsed test, got {tests.Length}"
        | Error msg -> Error $"Expected process-input fixture to parse, but got error: {msg}")

let tests = [
    ("parses multiline expectation on next line", testParsesMultilineExpectationOnNextLine)
    ("parses skip attribute", testParsesSkipAttribute)
    ("parses indented multiline .dark tests inside module", testParsesIndentedMultilineDarkTestsInsideModule)
    ("parses multiline with inner double equals", testParsesMultilineWithInnerDoubleEquals)
    ("parses multiline with inner let binding", testParsesMultilineWithInnerLetBinding)
    ("parses indented multiline let without module indent leak", testParsesIndentedMultilineLetWithoutModuleIndentLeak)
    ("parses expectation with keyword inside quoted message", testParsesExpectationWithKeywordInsideQuotedMessage)
    ("parses multiline function-head expectation with next-line arg", testParsesMultilineExpectationWithFunctionHeadAndNextLineArg)
    ("parses bare .e2e rhs as value expression", testParsesBareE2ERightHandSideAsValueExpression)
    ("parses indented rhs continuation without preamble leakage", testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble)
    ("parses dotted rhs continuation after bare identifier head", testParsesDottedRhsContinuationAfterBareIdentifierHead)
    ("parses multiline pipe before separator", testParsesMultilinePipeBeforeSeparator)
    ("parses multiline without leading paren", testParsesMultilineWithoutLeadingParen)
    ("parses multiline list rhs continuation", testParsesMultilineListRhsContinuation)
    ("parses single-line dotted rhs without consuming next test", testParsesSingleLineDottedRhsWithoutConsumingNextTest)
    ("parses multiline Dict rhs without preamble leakage", testParsesMultilineDictExpectationWithoutPreambleLeakage)
    ("parses sqlerror shorthand expectation", testParsesSqlErrorExpectationShorthand)
    ("parses escaped backslash before n as literal text", testParsesEscapedBackslashBeforeNAsLiteralText)
    ("parses repeated process arguments in order", testParsesRepeatedProcessArgumentsInOrder)
    ("builds parseable universal batch source", testBuildsParseableUniversalBatchSource)
    ("parses batch bitmask results", testParsesBatchBitmaskResults)
    ("rejects invalid batch bitmask results", testRejectsInvalidBatchBitmaskResults)
    ("does not batch tests with process inputs", testDoesNotBatchTestsWithProcessInputs)
]
