(* ANFPipeline.mli - Construct and optimize SSA after ANF helper lowering. *)
type asyncRuntime = {
  context : CompilationContexts.pipelineContext;
  materialize :
    CompilationContexts.pipelineContext ->
    SpecializationIdentity.SpecSet.t ->
    (CompilationContexts.pipelineContext * ANF.functionDef list, string) result;
  materializeSource :
    CompilationContexts.pipelineContext ->
    SpecializationIdentity.SpecSet.t ->
    (CompilationContexts.pipelineContext * ANF.functionDef list, string) result;
}

val buildConversionResult :
  ANF.program ->
  AST_to_ANF.registries ->
  OwnedIR.callSignature FunctionIdMap.t ->
  AST_to_ANF.conversionResult

val stdlibInliningConfig : InliningCommon.inliningConfig

(* The elapsed callback supplies the caller-owned stopwatch's milliseconds.
   Callers retain the returned registries, including late async helper identities
   and physical storage layouts. *)
val buildAnf :
  ?asyncDefinitions:ANF.functionDef StringOrder.Map.t ->
  ?asyncRuntime:asyncRuntime ->
  ?automaticTaskEntry:bool ->
  int ->
  CompilerOptions.compilerOptions ->
  (unit -> float) ->
  AST_to_ANF.registries ->
  int64 ->
  InliningCommon.inliningConfig ->
  InliningCommon.functionInfo FunctionIdMap.t ->
  ANF.functionDef StringOrder.Map.t ->
  SpecializationIdentity.FunctionSet.t ->
  ANF.functionDef list ->
  OwnedIR.callSignature FunctionIdMap.t ->
  bool ->
  CompilerOptions.passTimingRecorder option ->
  ( ANF.functionDef list
    * SSAANF.functionDef list
    * ANF.typeMap
    * AST_to_ANF.registries,
    string )
  result
