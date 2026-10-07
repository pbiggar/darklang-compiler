val emitRefCountInc :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  int ->
  LIR.rcKind ->
  (Symbolic.instr list, string) result

val emitRefCountDec :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  int ->
  LIR.rcKind ->
  MemoryModel.rcMetadata option ->
  (Symbolic.instr list, string) result

val emitRefCountIncString :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitRefCountDecString :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitRefCountIncInt :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.operand ->
  (Symbolic.instr list, string) result

val emitRefCountDecInt :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.operand ->
  (Symbolic.instr list, string) result
