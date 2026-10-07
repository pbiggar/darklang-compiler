val emitHeapAlloc :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  int ->
  (Symbolic.instr list, string) result

val emitHeapStore :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  int ->
  LIR.operand ->
  AST.semanticType option ->
  (Symbolic.instr list, string) result

val emitHeapLoad :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  int ->
  (Symbolic.instr list, string) result

val emitMappedAlloc :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitMappedFree :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitRawAlloc :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitRawFree :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitRawGet :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitRawGetByte :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitRawWriteWord :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitRawSlotInit :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  AST.semanticType ->
  (Symbolic.instr list, string) result

val emitRawWriteByte :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.reg ->
  LIR.reg ->
  (Symbolic.instr list, string) result
