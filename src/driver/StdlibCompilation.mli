(* StdlibCompilation.mli - Build reusable standard-library functions and concrete specializations. *)
val buildStdlibWithTrace : Platform.target -> CompilerOptions.passTimingRecorder option -> (CompilationContexts.stdlibResult,string) result
val buildStdlib : Platform.target -> (CompilationContexts.stdlibResult,string) result
val buildStdlibSpecializations : CompilationContexts.stdlibResult -> SpecializationIdentity.SpecSet.t -> TypeRegistries.typeRegistry -> LoweringPrimitives.variantLookup -> CompilerOptions.passTimingRecorder option -> (CompilationContexts.stdlibResult,string) result
