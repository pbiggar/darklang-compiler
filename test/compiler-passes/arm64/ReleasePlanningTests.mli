(*
   ReleasePlanningTests.mli - Verify planned release outlining and cache behavior.
   Native record descriptors are compile-time metadata. This fixture locks the
   compact payload layout at ARM64 codegen: fields begin at byte zero and no
   descriptor immediate is materialized in the heap object.
*)
open Dark_compiler

val testCompactRecordFieldsStartAtOffsetZero : unit -> (unit, string) result
val testSmallGenericReleasePlanRemainsInline : unit -> (unit, string) result
val testExpensiveGenericReleaseIsPreparedAsCall : unit -> (unit, string) result

val testGenericReleaseHelperPreservesCachedInstructions :
  unit -> (unit, string) result

val testOutlinedGenericReleaseUsesAllocatorLiveness :
  unit -> (unit, string) result

val testGenericReleaseHelpersPreserveOwnershipPolicy :
  unit -> (unit, string) result

val emitsPlannedListHelperLabel : Symbolic.instr list -> bool
