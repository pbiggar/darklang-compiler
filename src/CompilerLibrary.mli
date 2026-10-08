(* CompilerLibrary.mli - Compile a validated request using its explicit source-context plan. *)
val compile :
  CompilationContexts.compileRequest -> CompilerOptions.compileReport

(* Source trees correspond one-for-one to the request's named source units. *)
val compileWritten :
  CompilationContexts.compileRequest ->
  WrittenTypes.sourceFile list -> CompilerOptions.compileReport
