(* SpillOperands.fs - Materialize allocated operands and spilled register values. *)
val tryAllocation : AllocationModel.allocationResult -> int -> AllocationModel.allocation option
val getLiveCallerSavedRegs : AllocationModel.allocationResult -> AllocationModel.bitSet -> LIR.physReg list
val getLiveCallerSavedFloatRegs : Platform.arch -> AllocationModel.bitSet -> FloatAllocation.fAllocationResult -> LIR.physFPReg list
val applyToReg : AllocationModel.allocationResult -> LIR.reg -> LIR.reg * AllocationModel.allocation option
val applyToOperand : AllocationModel.allocationResult -> LIR.operand -> LIR.physReg -> LIR.operand * LIR.instr list
val applyToOperandNoLoad : AllocationModel.allocationResult -> LIR.operand -> LIR.operand
val loadSpilled : AllocationModel.allocationResult -> LIR.reg -> LIR.physReg -> LIR.reg * LIR.instr list
val isX86_64 : Platform.arch -> bool
val aliasesX86ScratchReg : LIR.physReg -> bool
val x86SpillTempExcluding : LIR.reg list -> LIR.physReg
val loadSpilledPair : Platform.arch -> AllocationModel.allocationResult -> LIR.reg -> LIR.reg -> LIR.reg -> (LIR.reg * LIR.instr list) * (LIR.reg * LIR.instr list)
