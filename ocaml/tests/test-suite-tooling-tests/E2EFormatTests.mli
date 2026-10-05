(*
   E2EFormatTests.fs - Unit tests for E2E DSL parsing

   Verifies parser behavior for multi-line E2E test forms used by upstream .dark files.
*)
type testResult=(unit,string) result
val testParsesMultilineExpectationOnNextLine : unit -> testResult
val testParsesSkipAttribute : unit -> testResult
val testParsesIndentedMultilineDarkTestsInsideModule : unit -> testResult
val testParsesMultilineWithInnerDoubleEquals : unit -> testResult
val testParsesMultilineWithInnerLetBinding : unit -> testResult
val testParsesIndentedMultilineLetWithoutModuleIndentLeak : unit -> testResult
val testParsesExpectationWithKeywordInsideQuotedMessage : unit -> testResult
val testParsesMultilineExpectationWithFunctionHeadAndNextLineArg : unit -> testResult
val testParsesBareE2ERightHandSideAsValueExpression : unit -> testResult
val testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble : unit -> testResult
val testParsesDottedRhsContinuationAfterBareIdentifierHead : unit -> testResult
val testParsesMultilinePipeBeforeSeparator : unit -> testResult
val testParsesMultilineWithoutLeadingParen : unit -> testResult
val testParsesMultilineListRhsContinuation : unit -> testResult
val testParsesSingleLineDottedRhsWithoutConsumingNextTest : unit -> testResult
val testParsesMultilineDictExpectationWithoutPreambleLeakage : unit -> testResult
val testParsesSqlErrorExpectationShorthand : unit -> testResult
val testParsesEscapedBackslashBeforeNAsLiteralText : unit -> testResult
val testParsesRepeatedProcessArgumentsInOrder : unit -> testResult
val testBuildsParseableUniversalBatchSource : unit -> testResult
val testBuildsParseableMultiChunkBatchSource : unit -> testResult
val testBuildsParseableLargeBatchSource : unit -> testResult
val testParsesBatchBitmaskResults : unit -> testResult
val testParsesMultiChunkBatchBitmaskResults : unit -> testResult
val testRejectsInvalidBatchBitmaskResults : unit -> testResult
val testRejectsInvalidMultiChunkBatchBitmaskResults : unit -> testResult
val testDoesNotBatchTestsWithProcessInputs : unit -> testResult
val testDoesNotBatchIsolatedTests : unit -> testResult
val testBatchesEqualityInsideExplicitResultBinding : unit -> testResult
val testCompileErrorDirectiveOverridesAnyUpstreamExpectation : unit -> testResult
val testCompileErrorExpectationRequiresCompileFailure : unit -> testResult
val testRejectsDanglingCompileErrorDirective : unit -> testResult
val tests : (string * (unit -> testResult)) list
