(* AsyncRuntimeMaterialization.mli - Specialize runtime dependencies requested by async lowering. *)
val materialize :
  CompilerOptions.passTimingRecorder option ->
  CompilationContexts.pipelineContext ->
  SpecializationIdentity.SpecSet.t ->
  (CompilationContexts.pipelineContext * ANF.functionDef list, string) result

(* Load value-level definitions for existing specializations without recompiling
   their synchronous ABI. Async inference emits only the changed versions. *)
val materializeSource :
  CompilerOptions.passTimingRecorder option ->
  CompilationContexts.pipelineContext ->
  SpecializationIdentity.SpecSet.t ->
  (CompilationContexts.pipelineContext * ANF.functionDef list, string) result
