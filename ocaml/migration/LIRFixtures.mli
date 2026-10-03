val observe : string -> Yojson.Basic.t
val instructions : string -> Dark_compiler.LIR.instr list
val instructionsWithOperand : string -> Dark_compiler.LIR.operand -> Dark_compiler.LIR.instr list
val instructionsWithRegisters : string -> Dark_compiler.LIR.reg -> Dark_compiler.LIR.fReg -> Dark_compiler.LIR.operand -> Dark_compiler.AST.semanticType -> Dark_compiler.LIR.instr list
