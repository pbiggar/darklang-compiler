(* MIROptimizationFacts.mli - Describe MIR effects and explicit definition/use edges. *)
type optimizeOptions = {enableSCCP : bool; enableCSE : bool; enableDCE : bool; enableLICM : bool}
val defaultOptimizeOptions : optimizeOptions
val hasSideEffects : MIR.instr -> bool
val analyzeEffectFreeFunctionsWithKnown : SpecializationIdentity.FunctionSet.t -> MIR.functionDef list -> SpecializationIdentity.FunctionSet.t
val analyzeEffectFreeFunctions : MIR.functionDef list -> SpecializationIdentity.FunctionSet.t
type puritySummary = {observableEffects : bool; readsMutableState : bool; mayTrap : bool; mayDiverge : bool}
val unknownPurity : puritySummary
val isPure : puritySummary -> bool
val analyzePurityWithKnown : puritySummary FunctionIdMap.t -> MIR.functionDef list -> puritySummary FunctionIdMap.t
val analyzePureFunctionsWithKnown : SpecializationIdentity.FunctionSet.t -> MIR.functionDef list -> SpecializationIdentity.FunctionSet.t
val effectFreeCallsForFunction : SpecializationIdentity.FunctionSet.t -> MIR.functionDef -> SpecializationIdentity.FunctionSet.t
val getInstrDest : MIR.instr -> MIR.vReg option
val foldInstrUses : ('state -> MIR.vReg -> 'state) -> 'state -> MIR.instr -> 'state
val getInstrUses : MIR.instr -> MIR.VRegSet.t
val foldTerminatorUses : ('state -> MIR.vReg -> 'state) -> 'state -> MIR.terminator -> 'state
val getTerminatorUses : MIR.terminator -> MIR.VRegSet.t
