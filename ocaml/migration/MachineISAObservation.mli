val observe : string -> Yojson.Basic.t
val x64Instr : Dark_compiler.X86_64.instr -> Yojson.Basic.t
val armInstr : Dark_compiler.ARM64.instr -> Yojson.Basic.t
val armReg : Dark_compiler.ARM64.reg -> Yojson.Basic.t
val armFReg : Dark_compiler.ARM64.fReg -> Yojson.Basic.t
val symInstr : Dark_compiler.Symbolic.instr -> Yojson.Basic.t
val symLabelRef : Dark_compiler.Symbolic.labelRef -> Yojson.Basic.t
val armInstructions : string -> int -> int -> Dark_compiler.ARM64.instr list
