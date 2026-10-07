(* Complete x64 code generation contracts and executable fixtures. *)
val testBranchFalseEdgeFallsThrough : unit -> (unit, string) result
val testStringLiteralUsesStaticStorage : unit -> (unit, string) result
val testStringLiteralHeapStorePreservesX3 : unit -> (unit, string) result
val testRawSlotInitRetainsX12Value : unit -> (unit, string) result
val testStringConcatLoadsStackSlotOperand : unit -> (unit, string) result
val testCliArgvHelperResolvesAsCodeLabel : unit -> (unit, string) result
val testCliArgvReturnsNullableString : unit -> (unit, string) result
val testCliHostOperationsExecute : unit -> (unit, string) result
val testCliNativePreservesLiveCallerRegister : unit -> (unit, string) result
val testDateTimeNowLowersTo100nsUnixTicks : unit -> (unit, string) result

val testSleepLowersToNormalizedInterruptSafeNanosleep :
  unit -> (unit, string) result

val testFloatArgumentMovesResolveCycles : unit -> (unit, string) result
val testHighFloatRegistersExecute : unit -> (unit, string) result

val testNonCommutativeFloatAliasesPreserveScratch :
  unit -> (unit, string) result

val testReportsMissingEntryBlock : unit -> (unit, string) result
val testRejectsConditionsWithoutBlockComparison : unit -> (unit, string) result
val testBranch : unit -> (unit, string) result

val testGenericRefCountDecMixedSumPayloadUsesVariantDispatch :
  unit -> (unit, string) result

val testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch :
  unit -> (unit, string) result

val testDictRefCountDecDictListValueUsesPlannedHelper :
  unit -> (unit, string) result

val testTaggedListTuplePayloadUsesPlannedHelper : unit -> (unit, string) result
val testTaggedListRecordPayloadUsesPlannedHelper : unit -> (unit, string) result

val testDictRefCountDecStringCollisionKeysAndValues :
  unit -> (unit, string) result

val testDictRefCountDecStringCollisionKeysAndTupleListValues :
  unit -> (unit, string) result

val testDictRefCountDecStringKeyTupleValueUsesPlannedHelper :
  unit -> (unit, string) result

val testTaggedListTuple5PayloadUsesPlannedHelper : unit -> (unit, string) result

val testTaggedListRecord5PayloadUsesPlannedHelper :
  unit -> (unit, string) result

val testClosureRefCountDecPreservesLiveArgumentClosures :
  unit -> (unit, string) result

val testClosureRefCountDecMixedSumCaptureUsesVariantDispatch :
  unit -> (unit, string) result

val testTaggedListRefCountDecClosurePayloadInStdlibFunction :
  unit -> (unit, string) result

val testTaggedListRefCountDecTuple3DynamicPayloadCombinations :
  unit -> (unit, string) result

val testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations :
  unit -> (unit, string) result

val testTaggedListRefCountDecRecord3DynamicPayloadCombinations :
  unit -> (unit, string) result

val testTaggedListRefCountDecMixedSumDynamicPayloadUsesVariantDispatch :
  unit -> (unit, string) result

val testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations :
  unit -> (unit, string) result

val testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations :
  unit -> (unit, string) result

val testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations :
  unit -> (unit, string) result

val tests : (string * (unit -> (unit, string) result)) list
