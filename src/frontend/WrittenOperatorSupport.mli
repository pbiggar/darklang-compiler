(* Source operator checking from WrittenOperatorSupport.mli, including structural equality. *)
val check :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  CheckedAST.symbols ->
  AST.semanticType option ->
  WrittenTypes.infix ->
  WrittenTypes.expr ->
  WrittenTypes.expr ->
  (WrittenTypeSupport.checkedExpression, string) result
