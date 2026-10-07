val convertInstr :
  ARM64CodeGenTypes.codeGenContext ->
  LIR.instr ->
  (Symbolic.instr list, string) result
