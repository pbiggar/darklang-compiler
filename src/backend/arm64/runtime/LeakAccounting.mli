val generateLeakCounterInc :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list

val generateLeakCounterDec :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list

val generateLeakCounterIncIfResultError :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.reg -> Symbolic.instr list

val generateLeakCheckReport :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list
