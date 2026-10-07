(* ApplyBlockAllocation.mli - Apply allocation and caller-save plans across CFG blocks. *)
val applyToTerminator : AllocationModel.allocationResult -> LIR.terminator -> LIR.instr list * LIR.terminator
type blockAllocationPreparation = {saveRegsLiveness : (AllocationModel.bitSet * AllocationModel.bitSet) list}
val applyToBlockWithLiveness : Platform.arch -> AllocationModel.allocationResult -> FloatAllocation.fAllocationResult -> AllocationModel.bitSet -> AllocationModel.bitSet -> LIR.basicBlock -> LIR.basicBlock
val prepareCFGAllocation : LIR.basicBlock array -> AllocationModel.allocationResult -> FloatAllocation.fAllocationResult -> AllocationModel.blockLiveness array -> AllocationModel.blockLiveness array -> AllocationModel.classifiedBlock array -> blockAllocationPreparation array
val applyPreparedCFGAllocation : Platform.arch -> LIR.basicBlock array -> AllocationModel.allocationResult -> FloatAllocation.fAllocationResult -> blockAllocationPreparation array -> LIR.basicBlock array
val applyToCFGWithLiveness : Platform.arch -> LIR.basicBlock array -> AllocationModel.allocationResult -> FloatAllocation.fAllocationResult -> AllocationModel.blockLiveness array -> AllocationModel.blockLiveness array -> LIR.basicBlock array
