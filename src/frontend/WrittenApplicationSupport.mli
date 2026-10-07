(* Builtin and indirect applications in the direct source checker. *)
val builtin :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  CheckedAST.symbols ->
  AST.semanticType option ->
  string ->
  WrittenTypes.typeReference list ->
  WrittenTypes.expr list ->
  (WrittenTypeSupport.checkedExpression, string) result

val indirect :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  CheckedAST.symbols ->
  AST.semanticType option ->
  WrittenTypes.range ->
  WrittenTypes.expr ->
  WrittenTypes.typeReference list ->
  WrittenTypes.expr list ->
  (WrittenTypeSupport.checkedExpression, string) result
