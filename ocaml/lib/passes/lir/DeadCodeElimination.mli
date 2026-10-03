val getCalledFunctions : AST.functionId StringOrder.Map.t -> LIR.functionDef -> SpecializationIdentity.FunctionSet.t
val requiresListDisplayHelpers : LIR.functionDef -> bool
val buildCallGraph : AST.functionId StringOrder.Map.t -> LIR.functionDef list -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t
val findReachable : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> SpecializationIdentity.FunctionSet.t -> SpecializationIdentity.FunctionSet.t
val directCallsFromFunctions : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> SpecializationIdentity.FunctionSet.t
val filterFunctionsWithUserCallGraph : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> LIR.functionDef list -> LIR.functionDef list
val filterFunctions : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> AST.functionId StringOrder.Map.t -> LIR.functionDef list -> LIR.functionDef list -> LIR.functionDef list
