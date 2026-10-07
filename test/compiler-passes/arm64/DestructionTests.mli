(*
   DestructionTests.mli - Verify recursive payload destruction and helper register preservation.
   Recursive-nominal release dispatch shape is not observable in an executable
   E2E test. A variant without managed fields must not consume a tag case in
   the generated helper, while the recursive variant must remain dispatched.
*)
val testDictListValuePlannedHelperReleasesCollisionPayloads : unit -> (unit,string) result
val testDictTupleValuePlannedHelperReleasesCollisionPayloads : unit -> (unit,string) result
val testDictStringKeyTupleValuePlannedHelperReleasesCollisionPayloads : unit -> (unit,string) result
val testGenericFixedBlockNestedBytesFieldUsesReleasePlan : unit -> (unit,string) result
val testPlannedListGenericLeafReleaseReloadsBlockPointer : unit -> (unit,string) result
val testPlannedListNestedGenericReleasePreservesBlockPointer : unit -> (unit,string) result
val testPlannedListTuplePayloadUsesPlannedHelper : unit -> (unit,string) result
val testPlannedListRecordPayloadUsesPlannedHelper : unit -> (unit,string) result
val testPlannedListRecordNestedStringDictUsesPlannedListHelper : unit -> (unit,string) result
val testPlannedListTuple5PayloadUsesPlannedHelper : unit -> (unit,string) result
val testPlannedListRecord5PayloadUsesPlannedHelper : unit -> (unit,string) result
val testGenericFixedBlockNestedImmediateFieldReleasesChildRoot : unit -> (unit,string) result
val testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch : unit -> (unit,string) result
val testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases : unit -> (unit,string) result
val testRecursiveSumReleaseSkipsVariantWithoutManagedFields : unit -> (unit,string) result
val testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan : unit -> (unit,string) result
val testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan : unit -> (unit,string) result
