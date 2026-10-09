val translateFunction :
  bool ->
  LIR.recordRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  string FunctionIdMap.t ->
  LIR.functionDef ->
  (X86_64.instr list, string) result
