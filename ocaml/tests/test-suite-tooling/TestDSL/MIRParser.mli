(* Parse the original flat MIR fixture syntax. *)
type instructionOrTerminator=Instruction of Dark_compiler.MIR.instr | Terminator of Dark_compiler.MIR.terminator
val parseVReg : string -> (Dark_compiler.MIR.vReg,string) result
val parseOperand : string -> (Dark_compiler.MIR.operand,string) result
val parseOp : string -> (Dark_compiler.MIR.binOp,string) result
val parseInstructionOrTerminator : int -> string -> (instructionOrTerminator,string) result
val parseMIRWithEntryLabel : string -> string -> (Dark_compiler.MIR.program,string) result
val parseMIR : string -> (Dark_compiler.MIR.program,string) result
