val emitPrintBool : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintChars : ARM64CodeGenTypes.codeGenContext -> int list -> (Symbolic.instr list,string) result
val emitPrintBlob : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintInt64NoNewline : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintUInt64NoNewline : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintBoolNoNewline : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintFloatNoNewline : ARM64CodeGenTypes.codeGenContext -> LIR.fReg -> (Symbolic.instr list,string) result
val emitPrintHeapStringNoNewline : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintList : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> AST.semanticType -> (Symbolic.instr list,string) result
val emitPrintSum : ARM64CodeGenTypes.codeGenContext -> (ARM64CodeGenTypes.codeGenContext -> LIR.instr -> (Symbolic.instr list,string) result) -> LIR.reg -> (string * int * AST.semanticType option) list -> bool -> (Symbolic.instr list,string) result
val emitPrintRecord : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> string -> (string * AST.semanticType) list -> (Symbolic.instr list,string) result
val emitPrintInt64 : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintUInt64 : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
val emitPrintFloat : ARM64CodeGenTypes.codeGenContext -> LIR.fReg -> (Symbolic.instr list,string) result
val emitPrintString : ARM64CodeGenTypes.codeGenContext -> string -> (Symbolic.instr list,string) result
val emitPrintHeapString : ARM64CodeGenTypes.codeGenContext -> LIR.reg -> (Symbolic.instr list,string) result
