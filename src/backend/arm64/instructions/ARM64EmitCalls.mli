val emitCall :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  AST.functionId ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitTailCall :
  ARM64CodeGenTypes.codeGenContext ->
  AST.functionId ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitIndirectCall :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitIndirectTailCall :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitClosureCall :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitClosureTailCall :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitSaveRegs :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.physReg list ->
  LIR.physFPReg list ->
  (Symbolic.instr list, string) result

val emitRestoreRegs :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.physReg list ->
  LIR.physFPReg list ->
  (Symbolic.instr list, string) result

val emitLoadFuncAddr :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  AST.functionId ->
  (Symbolic.instr list, string) result
