val getUsedVRegs : LIR.instr -> int list
val getDefinedVReg : LIR.instr -> int option
val getUsedFVRegs : LIR.instr -> int list
val getDefinedFVReg : LIR.instr -> int option
val getTerminatorUsedVRegs : LIR.terminator -> int list
val classifyBlocks : LIR.basicBlock array -> AllocationModel.classifiedBlock array
