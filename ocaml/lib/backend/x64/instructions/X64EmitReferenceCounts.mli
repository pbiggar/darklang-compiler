val emitRefCountInc : X64CodeGenTypes.funcCtx -> LIR.reg -> int -> LIR.rcKind -> (X86_64.instr list,string) result
val emitRefCountDec : X64CodeGenTypes.funcCtx -> LIR.reg -> int -> LIR.rcKind -> MemoryModel.rcMetadata option -> (X86_64.instr list,string) result
val emitRefCountIncString : X64CodeGenTypes.funcCtx -> LIR.operand -> (X86_64.instr list,string) result
val emitRefCountDecString : X64CodeGenTypes.funcCtx -> LIR.operand -> (X86_64.instr list,string) result
val emitRefCountIncInt : X64CodeGenTypes.funcCtx -> LIR.operand -> (X86_64.instr list,string) result
val emitRefCountDecInt : X64CodeGenTypes.funcCtx -> LIR.operand -> (X86_64.instr list,string) result
