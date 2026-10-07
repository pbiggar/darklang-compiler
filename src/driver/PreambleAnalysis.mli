(* PreambleAnalysis.mli - Check reusable source preambles against explicit base environments. *)
val analyzePreamble :
  bool ->
  CompilationContexts.stdlibResult ->
  string ->
  (CompilationContexts.preambleAnalysis, string) result
