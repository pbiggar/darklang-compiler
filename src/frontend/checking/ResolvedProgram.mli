(* ResolvedProgram.mli - Check resolved declarations and expressions against explicit environments. *)
val checkResolvedProgramInternal :
  Types.typeCheckEnv option ->
  bool ->
  AST.warningSettings ->
  bool ->
  AST.program ->
  ( AST.semanticType * AST.program * Types.typeCheckEnv,
    CheckingDiagnostics.typeError )
  result

val checkResolvedExpressionWithBaseEnv :
  Types.typeCheckEnv ->
  NameResolution.resolutionEnvironment ->
  bool ->
  AST.warningSettings ->
  AST.expr ->
  ( AST.semanticType * AST.program * Types.typeCheckEnv,
    CheckingDiagnostics.typeError )
  result
