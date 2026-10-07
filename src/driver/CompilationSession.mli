(* CompilationSession.mli - Own bounded compilation caches and their explicit session lifetime. *)
class compilationSession : ?collectCodegenMetrics:bool -> unit -> object
 method jsonPlanning : JsonPlanning.planningSession
 (* Returned instruction templates must not be mutated. *)
 method encodeX64Instruction : X86_64.instr -> bytes
 method arm64GenericReleaseHelperContextIdentity : Obj.t
 method convertAnfDependencies : Obj.t -> CompilationCacheIdentity.anfDependencyKey -> (unit -> (AST_to_ANF.functionConversion,string) result) -> (AST_to_ANF.functionConversion * Obj.t,string) result
 method compileDependencies : Obj.t -> CompilationCacheIdentity.compiledDependencyConfig -> (unit -> (LIR.functionDef list * CompilationCacheIdentity.functionSummary FunctionIdMap.t,string) result) -> (LIR.functionDef list * CompilationCacheIdentity.functionSummary FunctionIdMap.t,string) result
 method arm64LirOpExpansionRecorder : ARM64CodeGenTypes.lirOpExpansionRecorder option
 method optimizeMirFunction : CompilationCacheIdentity.mirOptimizationKey -> (unit -> MIR.functionDef) -> MIR.functionDef
 method allocateLirFunction : Platform.arch -> LIR.functionDef -> (unit -> LIR.functionDef) -> LIR.functionDef
 method allocateCallAwareLirFunction : LIR.functionDef -> ARM64CalleeClobbers.writes FunctionIdMap.t -> (unit -> LIR.functionDef) -> LIR.functionDef
 method refineArm64LirFunction : LIR.functionDef -> ARM64CalleeClobbers.writes FunctionIdMap.t -> (unit -> LIR.functionDef) -> LIR.functionDef
 method reachableStdlibFunctions : Obj.t -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> LIR.functionDef list -> LIR.functionDef list
 method projectMirRegistries : Obj.t -> MIR.variantRegistry * MIR.recordRegistry -> LoweringPrimitives.variantLookup -> (string * AST.semanticType) list StringOrder.Map.t -> MIR.variantRegistry * MIR.recordRegistry
 method arm64MetadataGroup : Obj.t -> LIR.functionDef list -> (unit -> ARM64CodeGenTypes.arm64ProgramMetadata) -> ARM64CodeGenTypes.arm64ProgramMetadata
 method codegenFunctionGroup : Obj.t -> ARM64.targetConfig -> ARM64CodeGenTypes.codeGenOptions -> LIR.functionDef list -> (unit -> (Backend_Arm64_CodeGen.generatedChunk list,string) result) -> (Backend_Arm64_CodeGen.generatedChunk list,string) result
 method arm64ReleasePlanSummary : bool -> string -> MemoryModel.rcReleasePlan -> (unit -> LIR.arm64ReleasePlanSummary) -> LIR.arm64ReleasePlanSummary
 method codegenFunction : Obj.t -> ARM64.targetConfig -> ARM64CodeGenTypes.codeGenOptions -> LIR.functionDef -> (unit -> (Symbolic.instr list,string) result) -> (Symbolic.instr list,string) result
 method arm64Helpers : Obj.t -> ARM64.targetConfig -> ARM64CodeGenTypes.codeGenOptions -> Backend_Arm64_CodeGen.helperCacheKey -> (unit -> Symbolic.instr list) -> Symbolic.instr list
 method prepareArm64EmissionChunk : Symbolic.instr list -> (unit -> ARM64_Encoding.preparedChunk) -> ARM64_Encoding.preparedChunk
 method prepareArm64EmissionChunkGroup : Symbolic.instr list list -> (unit -> ARM64_Encoding.preparedChunk) -> ARM64_Encoding.preparedChunk
 method cachedArm64FunctionCount : int
 method cachedArm64HelperCount : int
 method cachedAnfDependencyCount : int
 method cachedCompiledDependencyCount : int
 method cachedMirOptimizationCount : int
 method cachedAllocatedLirFunctionCount : int
 method cachedStdlibReachabilityCount : int
 method cachedMirRegistryProjectionCount : int
 method cachedArm64MetadataGroupCount : int
 method cachedArm64FunctionGroupCount : int
 method cachedArm64EmissionChunkCount : int
 method cachedArm64ReleasePlanSummaryCount : int
 method cachedJsonPlanCount : int
 method jsonPlanHitCount : int
 method jsonPlanMissCount : int
 method anfDependencyHitCount : int
 method anfDependencyMissCount : int
 method compiledDependencyHitCount : int
 method compiledDependencyMissCount : int
 method mirOptimizationHitCount : int
 method mirOptimizationMissCount : int
 method allocatedLirFunctionHitCount : int
 method allocatedLirFunctionMissCount : int
 method stdlibReachabilityHitCount : int
 method stdlibReachabilityMissCount : int
 method mirRegistryProjectionHitCount : int
 method mirRegistryProjectionMissCount : int
 method arm64CodegenHitCount : int
 method arm64CodegenMissCount : int
 method arm64StartCodegenHitCount : int
 method arm64MetadataGroupHitCount : int
 method arm64MetadataGroupMissCount : int
 method arm64FunctionGroupHitCount : int
 method arm64FunctionGroupMissCount : int
 method arm64HelperHitCount : int
 method arm64HelperMissCount : int
 method arm64ReleasePlanSummaryHitCount : int
 method arm64ReleasePlanSummaryMissCount : int
 method arm64CodegenMetrics : CompilerOptions.codegenFunctionMetric list
 method arm64LirOpMetrics : CompilerOptions.codegenLirOpMetric list
 method dispose : unit
end
