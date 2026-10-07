(* Original type-checker unit assertions and recursive traversal helper. *)
type testResult = (unit, string) result

val expectType :
  Dark_compiler.AST.expr -> Dark_compiler.AST.semanticType -> testResult

val countMatches : Dark_compiler.CheckedAST.expr -> int
val testInt64Literal : unit -> testResult
val testInt128Literal : unit -> testResult
val testUInt128Literal : unit -> testResult
val testSumEqualityUsesSinglePairMatch : unit -> testResult
val testRecordAccessRejectsInvalidRecordArity : unit -> testResult
val testAddition : unit -> testResult
val testSubtraction : unit -> testResult
val testMultiplication : unit -> testResult
val testDivision : unit -> testResult
val testNegation : unit -> testResult
val testNestedOperations : unit -> testResult
val testComplexExpression : unit -> testResult
val testDuplicateNominalTypeDeclarationUsesLastOverlay : unit -> testResult
val testDuplicateConstructorDeclarationRejected : unit -> testResult
val testDuplicateAndUndeclaredTypeParametersRejected : unit -> testResult
val testEmptyNominalDeclarationsRejected : unit -> testResult
val testInvalidDeclarationTypeReferencesRejected : unit -> testResult
val testConstructorIdentityCollisionRejected : unit -> testResult
val testRecursiveGroupsReceiveStableTypedIdentities : unit -> testResult
val testWrittenRecordRejectsRepeatedField : unit -> testResult
val testManyTopLevelFunctionsAndLetsAreStackSafe : unit -> testResult
val testFilteredResolutionCandidates : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
