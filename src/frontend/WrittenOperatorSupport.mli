(* Source operator checking from WrittenOperatorSupport.mli, including structural equality. *)
val check :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.infix ->
  WrittenTypes.expr ->
  WrittenTypes.expr ->
  (WrittenTypeSupport.checkedExpression, string) result
