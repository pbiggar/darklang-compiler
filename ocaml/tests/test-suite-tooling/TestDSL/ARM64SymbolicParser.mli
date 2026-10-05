(* Parse the frozen symbolic ARM64 fixture syntax. *)
val parseReg : string -> (Dark_compiler.ARM64.reg,string) result
val parseCond : string -> (Dark_compiler.ARM64.condition,string) result
val parseLabelRef : string -> (Dark_compiler.Symbolic.labelRef,string) result
val parseInstruction : int -> string -> (Dark_compiler.Symbolic.instr,string) result
val parseARM64Symbolic : string -> (Dark_compiler.Symbolic.instr list,string) result
