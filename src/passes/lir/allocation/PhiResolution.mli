(* PhiResolution.mli - Lower phi edges to allocation-aware parallel moves. *)
val generateFloatMoveInstrsWithAllocation : (LIR.fReg * LIR.fReg) list -> FloatAllocation.fAllocationResult -> LIR.instr list
val resolvePhiNodes : AllocationModel.blockIndex -> LIR.basicBlock array -> AllocationModel.allocationResult -> FloatAllocation.fAllocationResult -> LIR.basicBlock array
