(* Direct checking of tuples, lists, dictionaries, and match arms. *)
val tuple :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr list ->
  (WrittenTypeSupport.checkedExpression, string) result

val list :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  (WrittenTypes.expr * WrittenTypes.range option) list ->
  (WrittenTypeSupport.checkedExpression, string) result

val dict :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  (WrittenTypes.range
  * WrittenTypes.expr
  * WrittenTypes.range
  * WrittenTypes.expr)
  list ->
  (WrittenTypeSupport.checkedExpression, string) result

val matchExpression :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  WrittenTypes.matchCase list ->
  (WrittenTypeSupport.checkedExpression, string) result
