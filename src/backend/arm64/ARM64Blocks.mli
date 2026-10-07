val convertTerminator :
  string ->
  string option ->
  LIR.terminator ->
  (Symbolic.instr list, string) result

val convertBlock :
  ARM64CodeGenTypes.codeGenContext ->
  string ->
  LIR.basicBlock option ->
  LIR.basicBlock ->
  (Symbolic.instr list, string) result

val convertCFG :
  ARM64CodeGenTypes.codeGenContext ->
  string ->
  LIR.cfg ->
  (Symbolic.instr list, string) result
