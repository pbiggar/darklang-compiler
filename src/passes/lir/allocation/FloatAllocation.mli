(* FloatAllocation.mli - Schedule, allocate, spill, and materialize floating-point values. *)
val floatCallerSavedRegs : LIR.physFPReg list
val floatCalleeSavedRegs : LIR.physFPReg list
val allocatableFloatRegs : LIR.physFPReg list
val allocatableFloatRegsFor : Platform.arch -> LIR.physFPReg list
val floatCallerSavedRegsFor : Platform.arch -> LIR.physFPReg list
type fAllocation = FPhysReg of LIR.physFPReg | FStackSlot of int | FRematerialized of float
type fAllocationResult = {domain : AllocationModel.vRegDomain; allocations : fAllocation option array; stackSize : int; usedCalleeSavedF : LIR.physFPReg list; spillScratchLeft : LIR.fReg; spillScratchRight : LIR.fReg; spillScratchThird : LIR.fReg}
val physFPRegToInt : LIR.physFPReg -> int
val tryFloatAllocation : fAllocationResult -> int -> fAllocation option
val scheduleFloatLoadsInBlock : LIR.basicBlock -> LIR.basicBlock
val scheduleFloatLoadsInCFG : LIR.cfg -> LIR.cfg
val chordalFloatAllocationWithLiveness : LIR.physFPReg list -> int -> AllocationModel.blockIndex -> LIR.basicBlock array -> AllocationModel.classifiedBlock array -> AllocationModel.bitSet -> (int * int) list -> AllocationModel.vRegDomain -> AllocationModel.blockLiveness array -> fAllocationResult
val chordalFloatAllocation : LIR.cfg -> int list -> fAllocationResult
val applyFloatAllocationToFReg : fAllocationResult -> LIR.fReg -> LIR.fReg
val applyFloatAllocationToInstrs : fAllocationResult -> LIR.instr -> LIR.instr list
val applyFloatAllocationToBlock : fAllocationResult -> LIR.basicBlock -> LIR.basicBlock
val applyFloatAllocationToBlocks : fAllocationResult -> LIR.basicBlock array -> LIR.basicBlock array
val applyFloatAllocationToCFG : fAllocationResult -> LIR.cfg -> LIR.cfg
