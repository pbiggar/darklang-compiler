(* MIR_Optimize.fs - Schedule MIR simplification and optimization to a fixed point. *)
val optimizeCFGOnce : MIROptimizationFacts.optimizeOptions -> MIR.cfg -> MIR.cfg * bool
val optimizeCFGWithOptions : MIROptimizationFacts.optimizeOptions -> MIR.cfg -> MIR.cfg
val optimizeCFG : MIR.cfg -> MIR.cfg
val optimizeFunctionWithOptions : MIROptimizationFacts.optimizeOptions -> MIR.functionDef -> MIR.functionDef
val optimizeFunctionWithEffectFreeCallsAndTickTrace : (string -> int64 -> unit) option -> SpecializationIdentity.FunctionSet.t -> MIROptimizationFacts.optimizeOptions -> MIR.functionDef -> MIR.functionDef
val optimizeFunction : MIR.functionDef -> MIR.functionDef
val constantReturnOperand : MIR.functionDef -> MIR.operand option
val optimizeProgramWithOptions : MIROptimizationFacts.optimizeOptions -> MIR.program -> MIR.program
val optimizeProgramWithOptionsAndTrace : (string -> float -> unit) option -> MIROptimizationFacts.optimizeOptions -> MIR.program -> MIR.program
val optimizeProgram : MIR.program -> MIR.program
