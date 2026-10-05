open Dark_compiler
type testResult=(unit,string) result
val testMirOptimizationCacheReusesStructuralFunctions : CompilationContexts.stdlibResult -> unit -> testResult
val testAllocatedLirFunctionCacheReusesStructuralFunctions : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64HitWithNestedJson : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64CodegenCacheSegregatesTargetOptionsAndCoverage : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64CodegenMetricsAreOptIn : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64CodegenCacheSegregatesCompilationContexts : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64CodegenCacheReusesContextIndependentFunctions : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64CodegenCacheReusesPlannedSlotInitFunctions : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64EmissionChunkCacheUsesChunkIdentity : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64EmissionChunkGroupCacheUsesGroupIdentity : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64ReleasePlanSummaryCacheConfirmsPlanShape : CompilationContexts.stdlibResult -> unit -> testResult
val testSessionIsolationAndDisposal : CompilationContexts.stdlibResult -> unit -> testResult
val testJsonPlanCacheSegregatesNominalShapes : CompilationContexts.stdlibResult -> unit -> testResult
val testJsonDependenciesAreReusedBeforeLowering : CompilationContexts.stdlibResult -> unit -> testResult
val testDependencyMetadataIsReusedCompositionally : CompilationContexts.stdlibResult -> unit -> testResult
val testStdlibReachabilityIsReused : CompilationContexts.stdlibResult -> unit -> testResult
val testArm64HelpersAreReused : CompilationContexts.stdlibResult -> unit -> testResult
val tests : Platform.target -> CompilationContexts.stdlibResult -> (string * (unit -> testResult)) list
