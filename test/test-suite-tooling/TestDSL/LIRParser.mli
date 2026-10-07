(* Parse symbolic LIR fixture instructions, operands and flat control-flow graphs. *)
type instructionOrTerminator =
  | Instruction of Dark_compiler.LIR.instr
  | Terminator of Dark_compiler.LIR.terminator

val parsePhysReg : string -> (Dark_compiler.LIR.physReg, string) result
val parseRegister : string -> (Dark_compiler.LIR.reg, string) result
val parseOperand : string -> (Dark_compiler.LIR.operand, string) result

val parseInstructionOrTerminator :
  int -> string -> (instructionOrTerminator, string) result

val parseLIR : string -> (Dark_compiler.LIR.program, string) result
