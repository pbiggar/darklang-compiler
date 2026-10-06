(* Printing.fs - Emit x64 instructions for printing operations. *)
val emitPrintChars : X64CodeGenTypes.funcCtx -> char list -> (X86_64.instr list,string) result
val emitPrintInt64 : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintUInt64 : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintInt64NoNewline : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintUInt64NoNewline : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintBool : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintBoolNoNewline : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintHeapString : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintHeapStringNoNewline : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
val emitPrintString : X64CodeGenTypes.funcCtx -> string -> (X86_64.instr list,string) result
val emitPrintFloat : X64CodeGenTypes.funcCtx -> LIR.fReg -> (X86_64.instr list,string) result
val emitPrintFloatNoNewline : X64CodeGenTypes.funcCtx -> LIR.fReg -> (X86_64.instr list,string) result
val emitPrintList : X64CodeGenTypes.funcCtx -> LIR.reg -> AST.semanticType -> (X86_64.instr list,string) result
val emitPrintSum : X64CodeGenTypes.funcCtx -> LIR.reg -> (string*int*AST.semanticType option) list -> bool -> (X86_64.instr list,string) result
val emitPrintRecord : X64CodeGenTypes.funcCtx -> LIR.reg -> string -> (string*AST.semanticType) list -> (X86_64.instr list,string) result
val emitPrintBlob : X64CodeGenTypes.funcCtx -> LIR.reg -> (X86_64.instr list,string) result
