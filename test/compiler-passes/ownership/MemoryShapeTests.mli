(* MemoryShapeTests.mli - Verify memory shapes and stable recursive release plans. *)
type testResult = (unit, string) result

val testRcShapeConstructionAndEquality : unit -> testResult
val testRcShapeClassifiesPrimitivesAsImmediate : unit -> testResult
val testRcShapeClassifiesManagedIntegerBuffers : unit -> testResult
val testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks : unit -> testResult
val testRcShapeClassifiesRemainingRuntimeShapes : unit -> testResult
val testRcShapeClassifiesSumsWithVariantMetadata : unit -> testResult
val testRcShapeOwnershipHelpersClassifyManagedRoots : unit -> testResult
val testRcShapeOwnershipHelpersClassifyAutomaticBindingDecs : unit -> testResult
val testRcShapeOwnershipHelpersClassifyBorrowedRetains : unit -> testResult
val testRcShapeOwnershipHelpersSelectRootDispatch : unit -> testResult

val testRcShapeOwnershipHelpersSelectRetainReleaseOperations :
  unit -> testResult

val testRcShapeOwnershipHelpersClassifyStorage : unit -> testResult
val testRcShapeOwnershipHelpersClassifyRootManagement : unit -> testResult

val testRcShapeOwnershipHelpersClassifyOwnershipTransferRoots :
  unit -> testResult

val testRcShapeOwnershipHelpersClassifyRecursiveRelease : unit -> testResult
val testRcShapeReleasePlanClassifiesFieldCleanup : unit -> testResult
val testRcSourceTypeFingerprintIsStructuralAndStable : unit -> testResult
val testRcReleasePlanFingerprintIsCompositionalAndStable : unit -> testResult
val testRcReleasePlanCacheKeyOnlyFingerprintsLargePlans : unit -> testResult
val testRcReleasePlanOfTypeUsesRecordMetadata : unit -> testResult
val testRcReleasePlanOfTypeUsesSumPayloadMetadata : unit -> testResult
val testRcReleasePlanOfTypeWithSumsUsesVariantMetadata : unit -> testResult
val testRecursiveSumReleasePlanUsesTypedBackEdge : unit -> testResult
val testRecursiveRecordReleasePlanUsesTypedBackEdge : unit -> testResult
val testRcReleasePlanOfTypeClassifiesRemainingRootKinds : unit -> testResult
val testRcShapeRequiresRecordMetadata : unit -> testResult
val testRcShapeWithSumsRequiresSumMetadata : unit -> testResult
val tests : (string * (unit -> testResult)) list
