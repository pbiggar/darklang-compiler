val closurePayloadSizesFromAllocs : LIR.functionDef list -> int FunctionIdMap.t
val closureCaptureTypesFromParams : LIR.functionDef list -> AST.semanticType list StringOrder.Map.t
val closurePayloadSizesFromParams : LIR.functionDef list -> int StringOrder.Map.t
val generateClosureRefCountIncHelper : int StringOrder.Map.t -> X86_64.instr list
val generateClosureRefCountDecHelper : bool -> LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> int StringOrder.Map.t -> AST.semanticType list StringOrder.Map.t -> X86_64.instr list
