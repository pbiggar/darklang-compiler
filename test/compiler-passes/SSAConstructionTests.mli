(* SSAConstructionTests.mli - Unit tests for MIR SSA construction invariants. *)
type testResult = (unit, string) result
val testGetBlockUsesCoversEveryOperandPosition : unit -> testResult
val testComputeLivenessReportsMissingSuccessorBlock : unit -> testResult
val testComputeDominatorsHandlesJoinLoopAndUnreachableBlock : unit -> testResult
val testSSAVersionsStartAboveParameterRegisters : unit -> testResult
val testDeferredPhiUpdatesPreserveInstructionAndSourceOrder : unit -> testResult
val tests : (string * (unit -> testResult)) list
