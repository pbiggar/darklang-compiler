val emitMov :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitStore :
  X64CodeGenTypes.funcCtx ->
  int ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitAdd :
  X64CodeGenTypes.funcCtx ->
  X64InstructionContext.comparisonContext option ->
  LIR.reg ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitSub :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitMul :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitSdiv :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitUdiv :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitMsub :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitCmp :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitCset :
  X64CodeGenTypes.funcCtx ->
  X64InstructionContext.comparisonContext option ->
  LIR.reg ->
  LIR.condition ->
  (X86_64.instr list, string) result

val emitSelect :
  X64CodeGenTypes.funcCtx ->
  X64InstructionContext.comparisonContext option ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  LIR.condition ->
  (X86_64.instr list, string) result

val emitAnd :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitAnd_imm :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  int64 ->
  (X86_64.instr list, string) result

val emitOrr :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitEor :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitLsl_imm :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  int ->
  (X86_64.instr list, string) result

val emitLsr_imm :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  int ->
  (X86_64.instr list, string) result

val emitAsr_imm :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  int ->
  (X86_64.instr list, string) result

val emitNeg :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitMvn :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitSxtb :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitSxth :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitSxtw :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitUxtb :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitExit : X64CodeGenTypes.funcCtx -> (X86_64.instr list, string) result

val emitStdoutWrite :
  X64CodeGenTypes.funcCtx ->
  LIR.operand ->
  bool ->
  (X86_64.instr list, string) result

val emitStdinReadLine :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result

val emitRuntimeError :
  X64CodeGenTypes.funcCtx -> string -> (X86_64.instr list, string) result

val emitRuntimeErrorString :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result

val emitArgMoves :
  X64CodeGenTypes.funcCtx ->
  (LIR.physReg * LIR.operand) list ->
  (X86_64.instr list, string) result

val emitTailArgMoves :
  X64CodeGenTypes.funcCtx ->
  (LIR.physReg * LIR.operand) list ->
  (X86_64.instr list, string) result

val emitPhi :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result

val emitInt64ToFloat :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitGpToFp :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitLsl :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitLsr :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitAsr :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitUxth :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitUxtw :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitClosureAlloc :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  AST.functionId ->
  LIR.operand list ->
  (X86_64.instr list, string) result

val emitMadd :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result
