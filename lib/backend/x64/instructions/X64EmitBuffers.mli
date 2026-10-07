val emitCanonicalBufferEq : X64CodeGenTypes.funcCtx -> MemoryModel.canonicalBufferKind -> LIR.reg -> LIR.operand -> LIR.operand -> (X86_64.instr list,string) result
val emitStringConcat : X64CodeGenTypes.funcCtx -> LIR.reg -> LIR.operand -> LIR.operand -> LIR.operand list -> (X86_64.instr list,string) result
