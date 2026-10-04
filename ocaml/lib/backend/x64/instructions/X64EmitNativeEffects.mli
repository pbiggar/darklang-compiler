val emitCoverageHit : X64CodeGenTypes.funcCtx -> (X86_64.instr list,string) result
val emitRandomInt64 : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitDateTimeNow : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitSleep : X64CodeGenTypes.funcCtx -> int -> LIR.fReg -> (X86_64.instr list,string) result
val emitCliNative : X64CodeGenTypes.funcCtx -> LIR.reg -> LIR.cliOperation -> LIR.operand list -> (X86_64.instr list,string) result
