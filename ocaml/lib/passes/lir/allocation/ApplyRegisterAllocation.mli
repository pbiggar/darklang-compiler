(* ApplyInstructions.fs - Rewrite instruction operands through the allocation and spill plan. *)
val applyToInstr : Platform.arch -> AllocationModel.allocationResult -> LIR.instr -> LIR.instr list
