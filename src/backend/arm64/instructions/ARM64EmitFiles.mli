val emitFileReadBlob :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileExists :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileWriteBlob :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileAppendText :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileDelete :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileCreateDirectory :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileSetExecutable :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitFileWriteFromPtr :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.operand ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result
