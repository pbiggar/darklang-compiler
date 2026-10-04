val parseReg : string -> (Dark_compiler.ARM64.reg,string) result
val parseCond : string -> (Dark_compiler.ARM64.condition,string) result
val parseInstruction : int -> string -> (Dark_compiler.ARM64.instr,string) result
val parseARM64 : string -> (Dark_compiler.ARM64.instr list,string) result
val parseARM64ForEncodingError : string -> (Dark_compiler.ARM64.instr list,string) result
