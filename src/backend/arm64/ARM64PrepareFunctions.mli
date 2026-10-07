(* PrepareFunctions.fs - Attach target planning facts and outline expensive release operations. *)
val attachARM64CodegenFactsToFunctionsWithCache : ARM64CodeGenTypes.releasePlanSummaryCache option -> LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> LIR.functionDef list -> LIR.functionDef list
val attachARM64CodegenFactsToFunctions : LIR.functionDef list -> LIR.functionDef list
val prepareARM64FunctionsForAllocationWithCache : ARM64CodeGenTypes.releasePlanSummaryCache option -> (string -> float -> unit) option -> LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> AST.functionId -> AST.functionId StringOrder.Map.t -> LIR.functionDef list -> LIR.functionDef list * AST.functionId StringOrder.Map.t
val prepareARM64FunctionsForAllocation : LIR.functionDef list -> LIR.functionDef list
val prepareARM64Program : LIR.program -> LIR.program
