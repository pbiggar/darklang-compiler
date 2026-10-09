(* Declared full and partial applications from WrittenCallSupport.mli. *)
type literalChecker =
  AST.semanticType option ->
  WrittenCheckingState.t ->
  AST.semanticType ->
  CheckedAST.expr ->
  (WrittenTypeSupport.checkedExpression, string) result

val checkNamed :
  WrittenLambdaSupport.expressionChecker ->
  literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.range ->
  WrittenTypes.qualifiedFnIdentifier ->
  WrittenTypes.typeReference list ->
  WrittenTypes.expr list ->
  (WrittenTypeSupport.checkedExpression, string) result
