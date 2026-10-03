val filterUserFunctionsWithCallGraph : string option -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> LIR.functionDef list
val filterUserFunctions : string option -> LIR.functionDef list -> LIR.functionDef list
val filterStdlibFunctionsWithUserCallGraph : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> LIR.functionDef list -> LIR.functionDef list
val filterStdlibFunctions : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> LIR.functionDef list -> LIR.functionDef list
val getReachableStdlibNames : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> ANF.program -> SpecializationIdentity.FunctionSet.t
