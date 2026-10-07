(* Complete compiler-pass fixture loading, execution and typed diagnostics. *)
type passTestResult=TestOutcome.t={success:bool;message:string;expected:string option;actual:string option}
val prettyPrintMIR : Dark_compiler.MIR.program -> string
val prettyPrintLIR : Dark_compiler.LIR.program -> string
val renameLIRFunctions : string -> Dark_compiler.LIR.program -> Dark_compiler.LIR.program
val prettyPrintANF : Dark_compiler.ANF.program -> string
val loadMIR2LIRTest : string -> (Dark_compiler.MIR.program * Dark_compiler.LIR.program,string) result
val runMIR2LIRTest : Dark_compiler.MIR.program -> Dark_compiler.LIR.program -> passTestResult
val loadANF2MIRTest : string -> (Dark_compiler.ANF.program * Dark_compiler.MIR.program,string) result
val runANF2MIRTest : Dark_compiler.ANF.program -> Dark_compiler.MIR.program -> passTestResult
val prettyPrintARM64Reg : Dark_compiler.ARM64.reg -> string
val prettyPrintARM64Instr : Dark_compiler.Symbolic.instr -> string
val prettyPrintARM64 : Dark_compiler.Symbolic.instr list -> string
val loadLIR2ARM64Test : string -> (Dark_compiler.LIR.program * Dark_compiler.Symbolic.instr list,string) result
val runLIR2ARM64Test : Dark_compiler.LIR.program -> Dark_compiler.Symbolic.instr list -> passTestResult
