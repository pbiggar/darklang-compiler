(* Original LIR cleanup unit tests with unchanged expectations. *)
type testResult=(unit,string) result
val testRemoveSelfMovesFromAllocatedFunction : unit -> testResult
val testRemoveFloatingCopyBackMovesFromAllocatedFunction : unit -> testResult
val testFloatingCopyBackKeepsMoveAfterFPhiWritesSource : unit -> testResult
val testFNegMoveChainFusesWhenTempDies : unit -> testResult
val testFloatingArithmeticMoveChainsFuseWhenTempsDie : unit -> testResult
val testFloatingArithmeticMoveChainKeepsLiveTemp : unit -> testResult
val testSeparatedFloatAddKeepsLiveTemporary : unit -> testResult
val testSinkSeparatedAllocatedFloatAdd : unit -> testResult
val testSinkImmediateCounterUpdatePastAccumulator : unit -> testResult
val testSinkImmediateCounterUpdatePastSubtraction : unit -> testResult
val testSinkImmediateCounterUpdatePastDivision : unit -> testResult
val testSinkImmediateCounterUpdatePastProduct : unit -> testResult
val testSinkImmediateCounterUpdatePastMultiplyAdd : unit -> testResult
val testSinkImmediateCounterUpdatePastMultiplySubtract : unit -> testResult
val testSinkImmediateCounterUpdatePastXor : unit -> testResult
val testSinkImmediateCounterUpdatePastAnd : unit -> testResult
val testSinkImmediateCounterUpdatePastOr : unit -> testResult
val testSinkImmediateCounterUpdatePastLeftShift : unit -> testResult
val testSinkImmediateCounterUpdatePastRightShift : unit -> testResult
val testMulAddFusionKeepsLiveTempForPrint : unit -> testResult
val testMulSubFusionReplacesDeadTemp : unit -> testResult
val testMulSubFusionKeepsLiveTempForPrint : unit -> testResult
val testFloatMultiplyAddCombineAndTargetDecision : unit -> testResult
val testScalarDiamondFormsSelect : unit -> testResult
val testBranchZeroDiamondFormsMultipleNarrowSelects : unit -> testResult
val testMaterializedBooleanDiamondFormsSelect : unit -> testResult
val testMulConstantKeepsLiveConstRegister : unit -> testResult
val testBooleanNotBranchSwapsSuccessors : unit -> testResult
val testConditionalBranchKeepsBooleanUsedInSuccessor : unit -> testResult
val testOptimizeCFGRejectsMissingSuccessorLabel : unit -> testResult
val tests : (string * (unit -> testResult)) list
(* Additional migration checks derived from the unchanged optimization DSL. *)
val dslTests : (string * (unit -> testResult)) list
