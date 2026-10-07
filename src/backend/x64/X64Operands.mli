val syscalls : Platform.syscallNumbers
val lirRegToX86 : LIR.physReg -> X86_64.reg
val lirFRegToX86 : LIR.physFPReg -> X86_64.fReg
val resolveReg : LIR.reg -> (X86_64.reg, string) result
val loadImm64 : X86_64.reg -> int64 -> X86_64.instr list
val scratch : X86_64.reg
val arithmeticTempExcluding : X86_64.reg list -> X86_64.reg

val withPreservedFloatScratch :
  X86_64.fReg list -> (X86_64.fReg -> X86_64.instr list) -> X86_64.instr list

val heapPtr : X86_64.reg
val freeListBase : X86_64.reg
val freeListSize : int
val processTableOffset : int
val processTableSize : int
val maxFreeListPayload : int
val emitStringLiteral : X86_64.reg -> string -> X86_64.instr list
val emitStringLiteralNoRefCount : X86_64.reg -> string -> X86_64.instr list
val heapMmapSizeBytes : int64
val genWriteSyscall : X86_64.instr list
val genPrintChars : char list -> X86_64.instr list
val genExitSyscall : X86_64.instr list
val oomHandlerLabel : string
val runtimeErrorHandlerLabel : string
val genOomJump : unit -> X86_64.instr list
val genOomHandler : unit -> X86_64.instr list
val genRuntimeErrorHandler : unit -> X86_64.instr list
val freshLabel : string -> string
