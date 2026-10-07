(* Binding, match-pattern checking, and exhaustiveness from WrittenPatternSupport.mli. *)
val floatLiteral : bool -> string -> string -> float option

val checkLetPattern :
  WrittenTypes.letPattern ->
  AST.semanticType ->
  CheckedAST.symbols ->
  ( CheckedAST.letPattern * WrittenTypeSupport.locals * CheckedAST.symbols,
    string )
  result

val mergePatternBindings :
  WrittenTypeSupport.locals ->
  WrittenTypeSupport.locals ->
  (WrittenTypeSupport.locals, string) result

val checkMatchPattern :
  WrittenTypeSupport.globals ->
  CheckedAST.symbols ->
  WrittenTypeSupport.locals option ->
  AST.semanticType ->
  WrittenTypes.matchPattern ->
  ( CheckedAST.pattern * WrittenTypeSupport.locals * CheckedAST.symbols,
    string )
  result

val patternAlternatives : CheckedAST.pattern -> CheckedAST.pattern list
val patternCoversAny : CheckedAST.pattern -> bool
val patternCoversLiteral : CheckedAST.expr -> CheckedAST.pattern -> bool

val matchIsExhaustive :
  WrittenTypeSupport.globals ->
  CheckedAST.symbols ->
  AST.semanticType ->
  CheckedAST.expr ->
  CheckedAST.matchCase list ->
  bool
