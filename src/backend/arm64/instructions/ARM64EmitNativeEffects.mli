val emitRandomInt64 :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitDateTimeNow :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  (Symbolic.instr list, string) result

val emitSleep :
  ARM64CodeGenTypes.codeGenContext ->
  int ->
  LIR.fReg ->
  (Symbolic.instr list, string) result

val emitCliNative :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.reg ->
  LIR.cliOperation ->
  LIR.operand list ->
  (Symbolic.instr list, string) result

val emitCoverageHit :
  ARM64CodeGenTypes.codeGenContext ->
  int ->
  (Symbolic.instr list, string) result
