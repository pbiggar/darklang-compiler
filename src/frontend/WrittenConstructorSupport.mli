(* Constructor ownership, alias inference, and ordered checked payloads. *)
val check :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.qualifiedTypeIdentifier ->
  string ->
  WrittenTypes.expr list ->
  (WrittenTypeSupport.checkedExpression, string) result
