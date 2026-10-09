val emitHeapAlloc :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  int ->
  (X86_64.instr list, string) result

val emitHeapStore :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  int ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitHeapLoad :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  int ->
  (X86_64.instr list, string) result

val emitMappedAlloc :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitMappedFree :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result

val emitRawAlloc :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitRawFree :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result

val emitRawGet :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitRawGetByte :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitRawWriteWord :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result

val emitRawSlotInit :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  AST.semanticType ->
  (X86_64.instr list, string) result

val emitRawWriteByte :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (X86_64.instr list, string) result
