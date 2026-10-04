val recordPassTiming : CompilerOptions.passTimingRecorder option -> string -> float -> unit
val shouldDumpIR : int -> bool -> bool
val buildANFOptimizeOptions : CompilerOptions.compilerOptions -> ANFConstants.optimizeOptions
val shouldRunANFOptimize : ANFConstants.optimizeOptions -> bool
val buildMIROptimizeOptions : CompilerOptions.compilerOptions -> MIROptimizationFacts.optimizeOptions
val shouldRunMIROptimize : MIROptimizationFacts.optimizeOptions -> bool
val formatPassGroup : string -> (string * bool) list -> string
val printANFProgram : CompilerOptions.compilerOptions -> string -> ANF.program -> unit
val printMIRProgram : CompilerOptions.compilerOptions -> string -> MIR.program -> unit
val printLIRProgram : CompilerOptions.compilerOptions -> string -> LIR.program -> unit
