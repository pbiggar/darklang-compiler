val genRefCountDecGenericWithPlan : X64CodeGenTypes.funcCtx -> X86_64.reg -> int -> MemoryModel.rcReleasePlan option -> X86_64.instr list
val genRefCountDecGeneric : X64CodeGenTypes.funcCtx -> X86_64.reg -> int -> MemoryModel.rcMetadata option -> X86_64.instr list
val generateStreamRefCountDecHelper : X64CodeGenTypes.funcCtx -> X86_64.instr list
val generateRecursiveNominalRefCountDecHelper : bool -> LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> AST.semanticType -> X86_64.instr list
val recursiveReleaseTypesInFunctions : LIR.functionDef list -> MemoryPlanning.SemanticTypeSet.t
val genRefCountIncGeneric : X86_64.reg -> int -> X86_64.instr list
