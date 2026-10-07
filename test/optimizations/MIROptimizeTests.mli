type testResult = (unit, string) result

val testCseReusesEffectFreeDirectScalarCalls : unit -> testResult
val testCseReusesDominatingEffectFreeDirectScalarCalls : unit -> testResult
val testCseDirectCallsRespectBarriersAndScalarTypes : unit -> testResult
val testCseDoesNotReuseThrowingDirectCalls : unit -> testResult
val testCseAfterCopyPropFixpoint : unit -> testResult
val testCseReusesDominatingExpressions : unit -> testResult

val testUnaryPartialRedundancyEliminationCompletesMissingPath :
  unit -> testResult

val testPartialRedundancyEliminationCompletesMissingPath : unit -> testResult
val testCseReusesDominatingScalarHeapLoad : unit -> testResult

val testCseDoesNotReuseDominatingScalarHeapLoadAcrossBarriers :
  unit -> testResult

val testCsePreservesExpressionsAcrossSiblingBlocks : unit -> testResult
val testCseDoesNotReuseExpressionsAcrossRefCountDecrement : unit -> testResult
val testCseDoesNotExtendExpressionsAcrossCalls : unit -> testResult
val testCseDoesNotExportNonScalarBinaryTypes : unit -> testResult

val testCseKeepsScalarHeapLoadsAvailableAcrossPureScalarInstructions :
  unit -> testResult

val testCseDoesNotExportScalarHeapLoadsAcrossPureScalarInstructions :
  unit -> testResult

val testCseDoesNotKeepDirectCallsAvailableAcrossPureScalarInstructions :
  unit -> testResult

val testDceRemovesSelfReferentialDeadPhi : unit -> testResult
val testCfgSimplifyRemovesRetPhiJoin : unit -> testResult
val testCfgSimplifyCollapsesCopyWrappedRetPhiChain : unit -> testResult
val testEmptyBlockRemovalRewritesPhiSourceToPredecessor : unit -> testResult
val testLinearBlockMergePreservesPhiSources : unit -> testResult
val testLinearBlockMergeExposesLocalCSE : unit -> testResult
val testSameTargetBranchBecomesJumpAndDropsCondition : unit -> testResult
val testSccpPropagatesPhiConstantAndRemovesUnreachableEdge : unit -> testResult
val testSccpPhiIgnoresNonExecutableIncomingEdge : unit -> testResult
val testSccpCombinesCopyFoldingAndDeadEdgePruning : unit -> testResult
val testSccpPropagatesNegatedBooleanThroughCopy : unit -> testResult
val testSccpLoopBackedgeWidensInductionValue : unit -> testResult
val testSccpTracksFloatAndStringConstantsWithoutBypass : unit -> testResult
val testSccpStabilizesNanConstants : unit -> testResult
val testSccpTracksAggregateConstructorFields : unit -> testResult
val testSccpUsesCallResultRange : unit -> testResult
val testSccpPropagatesConstantCallResult : unit -> testResult
val testSccpDoesNotApplyIntegerFoldsToFloatOperations : unit -> testResult

val testPartialRedundancyEliminationSupportsFloatAndNarrowValues :
  unit -> testResult

val testSelfComparisonFoldingRequiresConcreteSafeType : unit -> testResult
val testSelfComparisonFoldingRequiresSameRegister : unit -> testResult
val testLicmCanonicalizesMultipleLoopEntries : unit -> testResult
val testLicmRetainsExistingLoopPreheader : unit -> testResult
val testLicmCanonicalizesNestedLoopEntry : unit -> testResult
val testLicmHoistsFloatUnaryAndConversionFamilies : unit -> testResult

val testCountedLoopUnrollingSupportsNarrowSignedAndUnsignedValues :
  unit -> testResult

val tests : (string * (unit -> testResult)) list
