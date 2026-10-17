(* AsyncRuntimeMaterialization.mli - Specialize runtime dependencies requested by async lowering. *)
val materialize :
  CompilerOptions.passTimingRecorder option ->
  CompilationContexts.pipelineContext ->
  SpecializationIdentity.SpecSet.t ->
  (CompilationContexts.pipelineContext * ANF.functionDef list, string) result
