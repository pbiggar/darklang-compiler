(* PreambleCompilation.mli - Compile reusable preamble contexts and their dependencies. *)
val buildPreambleContext :
  bool ->
  CompilationContexts.stdlibResult ->
  string ->
  string ->
  int StringOrder.Map.t ->
  CompilerOptions.passTimingRecorder option ->
  ( CompilationContexts.stdlibResult * CompilationContexts.preambleContext,
    string )
  result

val buildPreambleContextFromAnalysis :
  CompilationContexts.stdlibResult ->
  CompilationContexts.preambleAnalysis ->
  SpecializationIdentity.specializationResult ->
  string ->
  int StringOrder.Map.t ->
  CompilerOptions.passTimingRecorder option ->
  ( CompilationContexts.stdlibResult * CompilationContexts.preambleContext,
    string )
  result
