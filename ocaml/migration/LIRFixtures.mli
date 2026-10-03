val observe : string -> Yojson.Basic.t
val instructions : string -> Dark_compiler.LIR.instr list
val instructionsWithOperand : string -> Dark_compiler.LIR.operand -> Dark_compiler.LIR.instr list
