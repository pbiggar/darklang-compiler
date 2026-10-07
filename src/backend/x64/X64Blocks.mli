val translateTerminator :
  X64InstructionContext.comparisonContext option ->
  string ->
  string option ->
  LIR.terminator ->
  (X86_64.instr list, string) result

val translateBlock :
  X64CodeGenTypes.funcCtx ->
  string ->
  LIR.basicBlock option ->
  LIR.basicBlock ->
  (X86_64.instr list, string) result
