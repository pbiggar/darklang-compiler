(* Construct checked source from validated interpreter syntax. *)
type environment

val includeAllocatedFunctions : CheckedAST.symbols -> environment -> environment

val checkClosedProgram :
  Validation.validatedSourceFile ->
  (AST.semanticType * CheckedAST.program, string) result

val checkSimpleProgram :
  bool ->
  Validation.validatedSourceFile ->
  (AST.semanticType * CheckedAST.program, string) result

val checkSourceUnitsWithBase :
  environment option ->
  bool ->
  bool ->
  Validation.validatedSourceFile list ->
  (AST.semanticType * CheckedAST.program * environment, string) result

val checkSourceUnits :
  bool ->
  bool ->
  Validation.validatedSourceFile list ->
  (AST.semanticType * CheckedAST.program, string) result

val typeCheckEnvironment : CheckedAST.program -> Types.typeCheckEnv
