(* X64EmitFloatingPoint.mli - Emit x64 instructions for floatingpoint operations. *)
val emitFArgMoves :
  X64CodeGenTypes.funcCtx ->
  (LIR.physFPReg * LIR.fReg) list ->
  (X86_64.instr list, string) result

val emitFPhi : X64CodeGenTypes.funcCtx -> (X86_64.instr list, string) result

val emitFMov :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFLoad :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  float ->
  (X86_64.instr list, string) result

val emitFSpillLoad :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  int ->
  (X86_64.instr list, string) result

val emitFSpillStore :
  X64CodeGenTypes.funcCtx ->
  int ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFAdd :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFSub :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFMul :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFDiv :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFNeg :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFAbs :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFSqrt :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFCmp :
  X64CodeGenTypes.funcCtx ->
  LIR.fReg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFloatToInt64 :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFpToGp :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFloatToBits :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.fReg ->
  (X86_64.instr list, string) result

val emitFloatToString :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.fReg ->
  (X86_64.instr list, string) result
