val emitPhi : ARM64CodeGenTypes.codeGenContext -> (Symbolic.instr list,string) result
val emitMov : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.operand -> (Symbolic.instr list,string) result
val emitStore : ARM64CodeGenTypes.codeGenContext -> int -> LIR.reg -> (Symbolic.instr list,string) result
val emitAdd : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.operand -> (Symbolic.instr list,string) result
val emitSub : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.operand -> (Symbolic.instr list,string) result
val emitMul : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitSdiv : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitUdiv : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitMsub : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitMadd : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitCmp : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.operand -> (Symbolic.instr list,string) result
val emitCset : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.condition -> (Symbolic.instr list,string) result
val emitSelect : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> LIR.condition -> (Symbolic.instr list,string) result
val emitAnd : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitAnd_imm : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> int64 -> (Symbolic.instr list,string) result
val emitOrr : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitEor : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitLsl : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitLsr : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitAsr : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitLsl_imm : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> int -> (Symbolic.instr list,string) result
val emitLsr_imm : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> int -> (Symbolic.instr list,string) result
val emitAsr_imm : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> int -> (Symbolic.instr list,string) result
val emitNeg : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitMvn : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitSxtb : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitSxth : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitSxtw : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitUxtb : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitUxth : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitUxtw : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> LIR.reg -> (Symbolic.instr list,string) result
val emitClosureAlloc : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> AST.functionId -> LIR.operand list -> (Symbolic.instr list,string) result
val emitTailArgMoves : ARM64CodeGenTypes.codeGenContext -> (LIR.physReg * LIR.operand) list -> (Symbolic.instr list,string) result
val emitArgMoves : ARM64CodeGenTypes.codeGenContext -> (LIR.physReg * LIR.operand) list -> (Symbolic.instr list,string) result
val emitExit : ARM64CodeGenTypes.codeGenContext -> (Symbolic.instr list,string) result
val emitStdoutWrite : ARM64CodeGenTypes.codeGenContext -> int -> LIR.operand -> bool -> (Symbolic.instr list,string) result
val emitStdinReadLine : ARM64CodeGenTypes.codeGenContext -> int -> LIR.reg -> (Symbolic.instr list,string) result
val emitRuntimeError : ARM64CodeGenTypes.codeGenContext -> string -> (Symbolic.instr list,string) result
val emitRuntimeErrorString : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitInt64ToFloat : ARM64CodeGenTypes.codeGenContext -> LIR.fReg -> LIR.reg -> (Symbolic.instr list,string) result
val emitGpToFp : ARM64CodeGenTypes.codeGenContext -> LIR.fReg -> LIR.reg -> (Symbolic.instr list,string) result
