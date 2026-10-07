(* X64EmitCalls.mli - Emit x64 instructions for calls operations. *)
val emitSaveRegs : X64CodeGenTypes.funcCtx -> LIR.physReg list -> LIR.physFPReg list -> (X86_64.instr list,string) result
val emitRestoreRegs : X64CodeGenTypes.funcCtx -> LIR.physReg list -> LIR.physFPReg list -> (X86_64.instr list,string) result
val emitCall : X64CodeGenTypes.funcCtx -> LIR.reg -> AST.functionId -> LIR.operand list -> (X86_64.instr list,string) result
val emitTailCall : X64CodeGenTypes.funcCtx -> AST.functionId -> LIR.operand list -> (X86_64.instr list,string) result
val emitIndirectCall : X64CodeGenTypes.funcCtx -> LIR.reg -> LIR.reg -> LIR.operand list -> (X86_64.instr list,string) result
val emitIndirectTailCall : X64CodeGenTypes.funcCtx -> LIR.reg -> LIR.operand list -> (X86_64.instr list,string) result
val emitLoadFuncAddr : X64CodeGenTypes.funcCtx -> LIR.reg -> AST.functionId -> (X86_64.instr list,string) result
val emitClosureCall : X64CodeGenTypes.funcCtx -> LIR.reg -> LIR.reg -> LIR.operand list -> (X86_64.instr list,string) result
val emitClosureTailCall : X64CodeGenTypes.funcCtx -> LIR.reg -> LIR.operand list -> (X86_64.instr list,string) result
