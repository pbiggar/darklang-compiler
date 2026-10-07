(* CompilerLibrary.mli - Compile a validated request using its explicit source-context plan. *)
val compile :
  CompilationContexts.compileRequest -> CompilerOptions.compileReport
