(* Binding, match-pattern checking, and exhaustiveness from WrittenPatternSupport.mli. *)
val floatLiteral : bool -> string -> string -> float option

val checkLetPattern :
  WrittenTypes.letPattern ->
  AST.semanticType ->
  WrittenCheckingState.t ->
  ( CheckedAST.letPattern * WrittenTypeSupport.locals * WrittenCheckingState.t,
    string )
  result

val mergePatternBindings :
  WrittenTypeSupport.locals ->
  WrittenTypeSupport.locals ->
  (WrittenTypeSupport.locals, string) result

val checkMatchPattern :
  WrittenTypeSupport.globals ->
  WrittenCheckingState.t ->
  WrittenTypeSupport.locals option ->
  AST.semanticType ->
  WrittenTypes.matchPattern ->
  ( CheckedAST.pattern * WrittenTypeSupport.locals * WrittenCheckingState.t,
    string )
  result

val patternAlternatives : CheckedAST.pattern -> CheckedAST.pattern list
val patternCoversAny : CheckedAST.pattern -> bool
val patternCoversLiteral : CheckedAST.expr -> CheckedAST.pattern -> bool

val matchIsExhaustive :
  WrittenTypeSupport.globals ->
  WrittenCheckingState.t ->
  AST.semanticType ->
  CheckedAST.expr ->
  CheckedAST.matchCase list ->
  bool
