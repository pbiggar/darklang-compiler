(*
   E2EFormatTests.ml - Unit tests for E2E DSL parsing

   Verifies parser behavior for multi-line E2E test forms used by upstream .dark files.
*)
[@@@warning "-4-42"]

open Dark_compiler
open E2EFormat
open E2ETestRunner

type testResult = (unit, string) result

let ( let* ) = Result.bind

let withTempFileNamed fileName contents operation =
  let directory = Filename.temp_file "dark-e2eformat-" "" in
  Unix.unlink directory;
  Unix.mkdir directory 0o700;
  let path = Filename.concat directory fileName in
  Fun.protect
    ~finally:(fun () ->
      if FileIO.exists path then Unix.unlink path;
      Unix.rmdir directory)
    (fun () ->
      let oc = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc contents);
      operation path)

let withTempFile contents operation =
  withTempFileNamed "test.dark" contents operation

let require condition message = if condition then Ok () else Error message

let oneNamed filename source expectation check =
  withTempFileNamed filename source (fun path ->
      match parseE2ETestFile path with
      | Error msg -> Error (expectation ^ ", but got error: " ^ msg)
      | Ok [ test ] -> check test
      | Ok tests ->
          Error
            ("Expected exactly 1 parsed test, got "
            ^ string_of_int (List.length tests)))

let one source expectation check = oneNamed "test.dark" source expectation check

let two source expectation check =
  withTempFile source (fun path ->
      match parseE2ETestFile path with
      | Error msg -> Error (expectation ^ ", but got error: " ^ msg)
      | Ok [ first; second ] -> check first second
      | Ok tests ->
          Error
            ("Expected exactly 2 parsed tests, got "
            ^ string_of_int (List.length tests)))

let value expected test =
  require
    (test.expectedValueExpr = Some expected)
    ("Unexpected RHS parse: "
    ^ Option.value ~default:"None" test.expectedValueExpr)

let emptyPreamble test =
  require
    (Text.trim test.preamble = "")
    ("Expected empty preamble, got: " ^ test.preamble)

let errorMessage expected test =
  require
    (test.expectedErrorMessage = Some expected)
    ("Unexpected error message parse: "
    ^ Option.value ~default:"None" test.expectedErrorMessage)

let exitOne test =
  require
    (test.expectedExitCode = 1)
    ("Expected exit code 1, got " ^ string_of_int test.expectedExitCode)

let testParsesMultilineExpectationOnNextLine () =
  one
    "(if true then Builtin.testRuntimeError \"a\" else 0L) =\n\
    \  error=\"Uncaught exception: a\"\n"
    "Expected multiline .dark-style test to parse" (fun test ->
      let* () =
        require
          (Text.contains test.source "Builtin.testRuntimeError")
          ("Expected parsed source to contain runtime error expression, got: "
         ^ test.source)
      in
      let* () =
        require
          (test.expectedValueExpr = None)
          "Expected error= expectation to be parsed as error expectation"
      in
      let* () = exitOne test in
      errorMessage "Uncaught exception: a" test)

let testParsesSkipAttribute () =
  one "(if true then 1L else 2L) = skip=\"temporarily unsupported\"\n"
    "Expected skip attribute test to parse" (fun test ->
      match test.skipReason with
      | Some "temporarily unsupported" -> Ok ()
      | Some reason ->
          Error
            ("Expected skip reason 'temporarily unsupported', got '" ^ reason
           ^ "'")
      | None -> Error "Expected skip reason to be parsed")

let testParsesIndentedMultilineDarkTestsInsideModule () =
  one
    "module Int64 =\n\
    \  (match 6L with\n\
    \   | 6L -> \"pass\"\n\
    \   | _ -> \"fail\") = \"pass\"\n"
    "Expected indented .dark multiline test to parse" (fun test ->
      let* () =
        require
          (test.source = " match 6L with\n | 6L -> \"pass\"\n | _ -> \"fail\"")
          ("Unexpected parsed source: " ^ test.source)
      in
      let* () = value "\"pass\"" test in
      emptyPreamble test)

let testParsesMultilineWithInnerDoubleEquals () =
  withTempFile
    "(match 5L with\n\
    \ | x when (x + 1L) == 6L -> true\n\
    \ | 5L -> false) =\n\
    \  true\n" (fun path ->
      match parseE2ETestFile path with
      | Error msg ->
          Error
            ("Expected multiline test with inner == to parse, but got error: "
           ^ msg)
      | Ok [ test ] -> value "true" test
      | Ok tests ->
          Error
            ("Expected exactly 1 parsed test, got "
            ^ string_of_int (List.length tests)))

let testParsesMultilineWithInnerLetBinding () =
  one
    "(let x = 4L\n\
    \ match x with\n\
    \ | 1L | 2L | 3L | 4L -> \"pass\"\n\
    \ | _ -> \"fail\") = \"pass\"\n"
    "Expected multiline let-binding test to parse" (fun test ->
      let* () =
        require
          (Text.startsWith test.source " let x = 4L")
          ("Unexpected parsed source: " ^ test.source)
      in
      let* () = value "\"pass\"" test in
      emptyPreamble test)

let testParsesIndentedMultilineLetWithoutModuleIndentLeak () =
  one
    "module GenericTypeArgsAreOK =\n\
    \  (let segments =\n\
    \    [ \"a\"; \"b\" ]\n\
    \  String.join \"\" segments) = \"ab\"\n"
    "Expected indented multiline let test to parse" (fun test ->
      let* () =
        require
          (test.source
         = " let segments =\n  [ \"a\"; \"b\" ]\nString.join \"\" segments")
          ("Unexpected parsed source: " ^ test.source)
      in
      let* () = value "\"ab\"" test in
      emptyPreamble test)

let testParsesExpectationWithKeywordInsideQuotedMessage () =
  one
    "module Errors =\n\
    \  (match \"nothing matches\" with\n\
    \   | \"not this\" -> \"fail\") = error=\"No matching case found for value \
     \\\"nothing matches\\\" in match expression\"\n"
    "Expected quoted-message expectation to parse" (fun test ->
      let* () = exitOne test in
      errorMessage
        "No matching case found for value \"nothing matches\" in match \
         expression"
        test)

let testParsesMultilineExpectationWithFunctionHeadAndNextLineArg () =
  one
    "(match 1L with\n\
    \ | 1L -> \"wrong number\") =\n\
    \  error=\"No matching case found\"\n"
    "Expected multiline function-head expectation to parse" (fun test ->
      let* () = exitOne test in
      let* () = errorMessage "No matching case found" test in
      emptyPreamble test)

let testParsesBareE2ERightHandSideAsValueExpression () =
  oneNamed "test.e2e" "2 + 3 = 5\n"
    "Expected bare .e2e RHS to parse as value expression" (fun test ->
      let* () = value "5" test in
      require
        (test.expectedStdout = None)
        "Expected no stdout expectation for bare RHS")

let testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble () =
  two
    "(Stdlib.Option.Option.None |> Stdlib.Option.Option.Some) = \
     Stdlib.Option.Option.Some\n\
    \  Stdlib.Option.Option.None\n\
     (1L) = 1L\n"
    "Expected RHS continuation to parse" (fun first second ->
      let* () =
        value "Stdlib.Option.Option.Some\n  Stdlib.Option.Option.None" first
      in
      let* () = emptyPreamble first in
      let* () = emptyPreamble second in
      value "1L" second)

let testParsesDottedRhsContinuationAfterBareIdentifierHead () =
  two
    "(let _ = 1L in 2L) = Stdlib\n\
    \  .Option\n\
    \  .Option\n\
    \  .Some(2L)\n\
     (3L) = 3L\n"
    "Expected dotted RHS continuation after bare identifier head"
    (fun first second ->
      let* () = value "Stdlib\n  .Option\n  .Option\n  .Some(2L)" first in
      let* () = emptyPreamble first in
      let* () = emptyPreamble second in
      value "3L" second)

let testParsesMultilinePipeBeforeSeparator () =
  one
    "(Stdlib.List.map_v0 [ 1L; 2L ] (fun x -> x + 1L))\n\
     |> Stdlib.List.length_v0 = 2L\n"
    "Expected multiline pipe test to parse" (fun test ->
      let* () =
        require
          (Text.contains test.source "|> Stdlib.List.length_v0")
          ("Expected source to preserve pipe continuation, got: " ^ test.source)
      in
      let* () = value "2L" test in
      emptyPreamble test)

let testParsesMultilineWithoutLeadingParen () =
  one "Ctor\n  { a = 1L\n    b = 2L } = Ctor\n"
    "Expected multiline non-paren test to parse" (fun test ->
      let* () =
        require
          (Text.startsWith test.source "Ctor\n")
          ("Unexpected source parse: " ^ test.source)
      in
      let* () =
        require
          (Text.contains test.source "b = 2L }")
          ("Expected multiline record body in source, got: " ^ test.source)
      in
      let* () = value "Ctor" test in
      emptyPreamble test)

let testParsesMultilineListRhsContinuation () =
  two
    "module AccessDataInGenericField =\n\
    \  (Stdlib.DB.query TestRecordWithGenericThing (fun p -> p.name == \
     \"joe\")) = [ RecordWithGenericThing\n\
    \                                                                                \
     { name =\n\
    \                                                                                    \
     \"joe\" } ]\n\
    \  (1L) = 1L\n"
    "Expected multiline list RHS continuation to parse" (fun first second ->
      let* () =
        value
          "[ RecordWithGenericThing\n\
          \                                                                                \
           { name =\n\
          \                                                                                    \
           \"joe\" } ]"
          first
      in
      let* () = emptyPreamble first in
      value "1L" second)

let testParsesSingleLineDottedRhsWithoutConsumingNextTest () =
  two "module GetMany =\n  (1L) = Stdlib.Option.Option.None\n\n  (2L) = 2L\n"
    "Expected single-line dotted RHS to parse without continuation"
    (fun first second ->
      let* () = value "Stdlib.Option.Option.None" first in
      value "2L" second)

let testParsesMultilineDictExpectationWithoutPreambleLeakage () =
  two
    "module GetManyWithKeys =\n\
    \  (let one = 1L\n\
    \   let two = 2L\n\
    \   one) =\n\
    \    Dict\n\
    \      { one = 1L\n\
    \        two = 2L }\n\n\
    \  (3L) = 3L\n"
    "Expected multiline Dict RHS to parse without preamble leakage"
    (fun first second ->
      let* () = value "Dict\n    { one = 1L\n      two = 2L }" first in
      let* () = emptyPreamble first in
      let* () = value "3L" second in
      emptyPreamble second)

let testParsesSqlErrorExpectationShorthand () =
  one
    "friendsError (fun p -> \"x\") = sqlerror=\"Incorrect type in String \
     \\\"x\\\"\"\n"
    "Expected sqlerror shorthand expectation to parse" (fun test ->
      let* () =
        require
          (test.expectedValueExpr = None)
          "Expected no value-expression RHS for sqlerror shorthand"
      in
      let* () = exitOne test in
      let* () =
        require
          (test.expectedStdout = Some "")
          "Expected stdout empty-string expectation for sqlerror shorthand"
      in
      require
        (test.expectedStderr = Some "Incorrect type in String \"x\"")
        "Expected stderr parse for sqlerror shorthand")

let testParsesEscapedBackslashBeforeNAsLiteralText () =
  oneNamed "test.e2e" "print \"ignored\" = stdout=\"\\\\n\"\n"
    "Expected escaped backslash-n stdout to parse" (fun test ->
      require
        (test.expectedStdout = Some "\\n")
        "Expected stdout to contain literal backslash-n")

let testParsesRepeatedProcessArgumentsInOrder () =
  oneNamed "test.e2e" "1 = 1 arg=\"100\" arg=\"two words\"\n"
    "Expected process arguments to parse" (fun test ->
      require
        (test.arguments = [ "100"; "two words" ])
        "Unexpected process arguments")

let preparedNamed source operation =
  withTempFileNamed "ordinary.e2e" source (fun path ->
      match parseE2ETestFile path with
      | Error msg ->
          Error ("Expected batch fixture to parse, but got error: " ^ msg)
      | Ok tests -> operation (List.filter_map tryPrepareBatchTest tests))

let parsesBatch source label =
  match WrittenParsing.parse Validation.Script source with
  | Error msg ->
      Error
        ("Generated " ^ label ^ " source did not parse: " ^ msg ^ "\n" ^ source)
  | Ok _ -> Ok ()

let testBuildsParseableUniversalBatchSource () =
  preparedNamed "1L + 1L = 2L\n2L + 2L = 4L\n" (fun prepared ->
      match prepared with
      | [ first; second ] ->
          let* () =
            require
              (canBatchTogether first second)
              "Expected adjacent tests with the same context and options to \
               batch together"
          in
          let source = buildBatchSource prepared in
          let* () = parsesBatch source "batch" in
          let* () =
            require
              (Text.contains source "_Check0 (seed: Int64) : Bool =")
              ("Generated batch did not isolate each check in a function:\n"
             ^ source)
          in
          let* () =
            require
              (Text.contains source
                 "fun value -> if seed == 0L then value else false")
              ("Generated batch check was not protected from inlining:\n"
             ^ source)
          in
          let* () =
            require
              (Text.contains source "_Result0 = e2eBatch")
              ("Generated batch did not eagerly call the first check:\n"
             ^ source)
          in
          require
            (Text.endsWith source "then 2L else 0L)")
            ("Generated batch did not encode the final result bit:\n" ^ source)
      | _ ->
          Error
            ("Expected 2 universally batchable tests, got "
            ^ string_of_int (List.length prepared)))

let testBuildsParseableMultiChunkBatchSource () =
  preparedNamed "1L + 1L = 2L\n" (function
    | [ prepared ] ->
        let source = buildBatchSource (List.init 33 (fun _ -> prepared)) in
        let* () = parsesBatch source "multi-chunk batch" in
        require
          (Text.endsWith source "then 1L else 0L)))")
          ("Generated batch did not encode its second result chunk:\n" ^ source)
    | prepared ->
        Error
          ("Expected 1 universally batchable test, got "
          ^ string_of_int (List.length prepared)))

let testBuildsParseableLargeBatchSource () =
  preparedNamed "1L + 1L = 2L\n" (function
    | [ prepared ] ->
        parsesBatch
          (buildBatchSource (List.init 220 (fun _ -> prepared)))
          "large batch"
    | prepared ->
        Error
          ("Expected 1 universally batchable test, got "
          ^ string_of_int (List.length prepared)))

let testParsesBatchBitmaskResults () =
  require
    (tryParseBatchBoolResults 3 "ignored output\n5\n"
    = Some [ true; false; true ])
    "Expected bitmask 5 to decode as [true; false; true]"

let testParsesMultiChunkBatchBitmaskResults () =
  match tryParseBatchBoolResults 65 "ignored output\n(2147483649, 2, 1)\n" with
  | Some values ->
      let indexes =
        List.mapi (fun index value -> (index, value)) values
        |> List.filter_map (fun (index, value) ->
            if value then Some index else None)
      in
      require
        (List.length values = 65 && indexes = [ 0; 31; 33; 64 ])
        "Expected selected result bits at [0; 31; 33; 64]"
  | None -> Error "Expected three result chunks to decode selected bits"

let testRejectsInvalidBatchBitmaskResults () =
  require
    (tryParseBatchBoolResults 3 "8\n" = None)
    "Expected out-of-range result bits to be rejected"

let testRejectsInvalidMultiChunkBatchBitmaskResults () =
  require
    (tryParseBatchBoolResults 65 "(0, 0)\n" = None
    && tryParseBatchBoolResults 65 "(0, 0, 2)\n" = None)
    "Expected invalid multi-chunk result vectors to be rejected"

let testDoesNotBatchTestsWithProcessInputs () =
  oneNamed "ordinary.e2e" "1 = 1 arg=\"100\"\n"
    "Expected process-input fixture to parse" (fun test ->
      require
        (tryPrepareBatchTest test = None)
        "Expected argument-bearing test to remain isolated")

let testDoesNotBatchIsolatedTests () =
  oneNamed "ordinary.e2e" "1 = 1 isolated=true\n"
    "Expected isolated fixture to parse" (fun test ->
      require
        (test.isolated && tryPrepareBatchTest test = None)
        "Expected explicitly isolated test to remain unbatched")

let testBatchesEqualityInsideExplicitResultBinding () =
  oneNamed "ordinary.e2e"
    "let identityInt (value: Int) : Int = value\n\
     identityInt 9223372036854775808 = 9223372036854775808\n"
    "Expected local-declaration fixture to parse" (fun test ->
      require
        (Option.is_some (tryPrepareBatchTest test))
        "Expected locally-declared equality to embed in an explicit result \
         binding")

let testCompileErrorDirectiveOverridesAnyUpstreamExpectation () =
  withTempFile
    "#compileerror=\"first compile failure\"\n\
     1L = 1L\n\
     #compileerror=\"second compile failure\"\n\
     Builtin.testRuntimeError \"runtime\" = error=\"runtime\"\n" (fun path ->
      match parseE2ETestFile path with
      | Ok [ first; second ]
        when first.errorExpectation = Some CompileError
             && first.expectedErrorMessage = Some "first compile failure"
             && first.expectedValueExpr = None
             && second.errorExpectation = Some CompileError
             && second.expectedErrorMessage = Some "second compile failure" ->
          Ok ()
      | Ok _ -> Error "Expected two compile-error overrides"
      | Error msg ->
          Error
            ("Expected #compileerror overrides to parse, but got error: " ^ msg))

let testCompileErrorExpectationRequiresCompileFailure () =
  one "1L = compileerror=\"compile failure\"\n"
    "Expected compileerror= to parse" (fun test ->
      let runtimeFailure = Ran (1, "", "compile failure", 0L, 0L)
      and compileFailure = CompileFailed (1, "compile failure", 0L) in
      match
        ( evaluateExpectations test runtimeFailure,
          evaluateExpectations test compileFailure )
      with
      | Error runtimeResult, Ok _
        when Text.contains runtimeResult.message
               "Expected compilation error but compilation succeeded" ->
          Ok ()
      | _ -> Error "Expected only CompileFailed to satisfy compileerror=")

let testRejectsDanglingCompileErrorDirective () =
  withTempFile "#compileerror=\"missing test\"\n" (fun path ->
      match parseE2ETestFile path with
      | Error msg
        when Text.contains msg "#compileerror must immediately precede a test"
        ->
          Ok ()
      | Error msg -> Error ("Unexpected dangling-directive error: " ^ msg)
      | Ok _ -> Error "Expected dangling #compileerror rejection")

let tests =
  [
    ( "parses multiline expectation on next line",
      testParsesMultilineExpectationOnNextLine );
    ("parses skip attribute", testParsesSkipAttribute);
    ( "parses indented multiline .dark tests inside module",
      testParsesIndentedMultilineDarkTestsInsideModule );
    ( "parses multiline with inner double equals",
      testParsesMultilineWithInnerDoubleEquals );
    ( "parses multiline with inner let binding",
      testParsesMultilineWithInnerLetBinding );
    ( "parses indented multiline let without module indent leak",
      testParsesIndentedMultilineLetWithoutModuleIndentLeak );
    ( "parses expectation with keyword inside quoted message",
      testParsesExpectationWithKeywordInsideQuotedMessage );
    ( "parses multiline function-head expectation with next-line arg",
      testParsesMultilineExpectationWithFunctionHeadAndNextLineArg );
    ( "parses bare .e2e rhs as value expression",
      testParsesBareE2ERightHandSideAsValueExpression );
    ( "parses indented rhs continuation without preamble leakage",
      testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble );
    ( "parses dotted rhs continuation after bare identifier head",
      testParsesDottedRhsContinuationAfterBareIdentifierHead );
    ( "parses multiline pipe before separator",
      testParsesMultilinePipeBeforeSeparator );
    ( "parses multiline without leading paren",
      testParsesMultilineWithoutLeadingParen );
    ( "parses multiline list rhs continuation",
      testParsesMultilineListRhsContinuation );
    ( "parses single-line dotted rhs without consuming next test",
      testParsesSingleLineDottedRhsWithoutConsumingNextTest );
    ( "parses multiline Dict rhs without preamble leakage",
      testParsesMultilineDictExpectationWithoutPreambleLeakage );
    ( "parses sqlerror shorthand expectation",
      testParsesSqlErrorExpectationShorthand );
    ( "parses escaped backslash before n as literal text",
      testParsesEscapedBackslashBeforeNAsLiteralText );
    ( "parses repeated process arguments in order",
      testParsesRepeatedProcessArgumentsInOrder );
    ( "builds parseable universal batch source",
      testBuildsParseableUniversalBatchSource );
    ( "builds parseable multi-chunk batch source",
      testBuildsParseableMultiChunkBatchSource );
    ("builds parseable large batch source", testBuildsParseableLargeBatchSource);
    ("parses batch bitmask results", testParsesBatchBitmaskResults);
    ( "parses multi-chunk batch bitmask results",
      testParsesMultiChunkBatchBitmaskResults );
    ( "rejects invalid batch bitmask results",
      testRejectsInvalidBatchBitmaskResults );
    ( "rejects invalid multi-chunk batch bitmask results",
      testRejectsInvalidMultiChunkBatchBitmaskResults );
    ( "does not batch tests with process inputs",
      testDoesNotBatchTestsWithProcessInputs );
    ("does not batch explicitly isolated tests", testDoesNotBatchIsolatedTests);
    ( "batches equality inside explicit result binding",
      testBatchesEqualityInsideExplicitResultBinding );
    ( "compile-error directive overrides any upstream expectation",
      testCompileErrorDirectiveOverridesAnyUpstreamExpectation );
    ( "compile-error expectation requires compile failure",
      testCompileErrorExpectationRequiresCompileFailure );
    ( "rejects dangling compile-error directive",
      testRejectsDanglingCompileErrorDirective );
  ]
