val emitFileReadBlob :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitFileWriteBlob :
  X64CodeGenTypes.funcCtx ->
  LIR.instr ->
  LIR.reg ->
  LIR.operand ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitFileExists :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitFileDelete :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitFileCreateDirectory :
  X64CodeGenTypes.funcCtx ->
  LIR.reg ->
  LIR.operand ->
  (X86_64.instr list, string) result

val emitFileSetExecutable :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result

val emitFileWriteFromPtr :
  X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list, string) result
