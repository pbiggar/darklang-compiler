(* Orchestrate integer and floating allocation and phi elimination. *)
val parameterRegs : LIR.physReg list
val floatParamRegs : LIR.physFPReg list
val allocateRegisters : Platform.arch -> LIR.functionDef -> LIR.functionDef
val allocateRegistersWithCallSummaries : Platform.arch -> ARM64CalleeClobbers.writes FunctionIdMap.t -> LIR.functionDef -> LIR.functionDef
val allocateRegistersWithTiming : Platform.arch -> LIR.functionDef -> LIR.functionDef * AllocationModel.registerAllocationTiming list
