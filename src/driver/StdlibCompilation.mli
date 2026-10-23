(* StdlibCompilation.mli - Build reusable standard-library functions and concrete specializations. *)
val buildStdlibWithTrace :
  Platform.target ->
  CompilerOptions.passTimingRecorder option ->
  (CompilationContexts.stdlibResult, string) result

(* Imported test helpers are checked together without entering application scope. *)
val buildImportedTestStdlibWithTrace :
  Platform.target ->
  CompilerOptions.passTimingRecorder option ->
  (CompilationContexts.stdlibResult, string) result

val buildStdlib :
  Platform.target -> (CompilationContexts.stdlibResult, string) result

val buildStdlibSpecializations :
  CompilationContexts.stdlibResult ->
  SpecializationIdentity.SpecSet.t ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  CheckedAST.symbols ->
  CompilerOptions.passTimingRecorder option ->
  (CompilationContexts.stdlibResult, string) result
