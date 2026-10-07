val translateInstr :
  X64InstructionContext.comparisonContext option ->
  X64CodeGenTypes.funcCtx ->
  LIR.instr ->
  (X86_64.instr list, string) result
