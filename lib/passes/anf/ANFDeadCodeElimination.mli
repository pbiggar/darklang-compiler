(* ANFDeadCodeElimination.fs - ANF-level Dead Code Elimination *)
val getCalledFunctions : ANF.functionDef -> SpecializationIdentity.FunctionSet.t
val buildCallGraph : ANF.functionDef list -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t
val filterReachableFunctions : SpecializationIdentity.FunctionSet.t -> ANF.functionDef list -> ANF.functionDef list
val getReachableStdlib : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> ANF.functionDef list -> SpecializationIdentity.FunctionSet.t
