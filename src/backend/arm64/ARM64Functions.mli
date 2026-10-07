val convertFunction :
  Symbolic.instr list ->
  ARM64CodeGenTypes.codeGenContext ->
  LIR.functionDef ->
  (Symbolic.instr list, string) result
