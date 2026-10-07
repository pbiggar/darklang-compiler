(* Alias transfer checks and shared ownership-path scanners. *)
open Dark_compiler

val hasRefCountIncForTemp : ANF.tempId -> ANF.aExpr -> bool
val hasRefCountDecForTemp : ANF.tempId -> ANF.aExpr -> bool
val pathHasRetainsBeforeDec : ANF.tempId list -> ANF.tempId -> ANF.aExpr -> bool

val tryRefCountDecSourceTypeForTemp :
  ANF.tempId -> ANF.aExpr -> AST.semanticType option

val testFreshOwnedValueTransfersIntoRawSlot : unit -> (unit, string) result

val testRawSlotRetainsValueUsedAfterInitialization :
  unit -> (unit, string) result

val testRawSlotRetainsFreshStreamValue : unit -> (unit, string) result

val testBranchLocalTempReuseUsesCurrentTypeContext :
  unit -> (unit, string) result

val testReturnedAggregateTransfersOwnedValueThroughAlias :
  unit -> (unit, string) result

val testReturnedAggregateTransfersOwnedValueThroughTypedAlias :
  unit -> (unit, string) result

val testReturnedAggregateRetainsOwnershipProducingStreamAlias :
  unit -> (unit, string) result

val testReturnedAggregateTransfersOwnedValueAfterBorrowedUse :
  unit -> (unit, string) result

val testExplicitReleaseBlocksLaterAggregateTransfer :
  unit -> (unit, string) result

val testReturnedAggregateTransfersOwnedValueAcrossBranches :
  unit -> (unit, string) result

val testReturnedAggregateRequiresEveryBranchToTransferOwnedValue :
  unit -> (unit, string) result

val testReturnedAggregateTransfersNestedOwnedAliases :
  unit -> (unit, string) result

val testReturnedAggregateDoesNotTransferDuplicatedAliases :
  unit -> (unit, string) result

val testStaticStringBindingSkipsNoOpRcTraffic : unit -> (unit, string) result
val testKnownEmptyListBindingSkipsNoOpRcTraffic : unit -> (unit, string) result

val testAggregateSkipsRetainsForKnownNonRcSentinels :
  unit -> (unit, string) result

val testAggregateSkipsRetainForConditionalStaticString :
  unit -> (unit, string) result

val testNonSelfTailCallDoesNotLeaveDecAfterTailCall :
  unit -> (unit, string) result

val testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed :
  unit -> (unit, string) result

val testRecordReuseRetainsReplacementBeforeReleasingOldChild :
  unit -> (unit, string) result

val testCompositeRecordReuseCarriesRecursiveReleasePlan :
  unit -> (unit, string) result

val testNestedRecordReuseCarriesRecursiveReleasePlan :
  unit -> (unit, string) result

val testBoxedSumReuseReleasesSourceVariantBeforeOverwrite :
  unit -> (unit, string) result

val testRecursiveRecordReuseCarriesTypedBackEdgeReleasePlan :
  unit -> (unit, string) result

val testRecursiveBoxedSumReuseCarriesTypedBackEdgeReleasePlan :
  unit -> (unit, string) result
