val emitFPhi :
  ARM64CodeGenTypes.codeGenContext -> (Symbolic.instr list, string) result

val emitFArgMoves :
  ARM64CodeGenTypes.codeGenContext ->
  (LIR.physFPReg * LIR.fReg) list ->
  (Symbolic.instr list, string) result

val emitFMov :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFLoad :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  float ->
  (Symbolic.instr list, string) result

val emitFSpillLoad :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  int ->
  (Symbolic.instr list, string) result

val emitFSpillStore :
  ARM64CodeGenTypes.codeGenContext ->
  int ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFAdd :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFSub :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFMul :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFMadd :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFDiv :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFNeg :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFAbs :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFSqrt :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFCmp :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.fReg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFloatToInt64 :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFpToGp :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFloatToBits :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitFloatToString :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.fReg ->
  (Symbolic.instr list, string) result
