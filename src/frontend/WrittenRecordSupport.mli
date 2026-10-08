(* Named records, field access, and source-ordered record updates. *)
val record :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.qualifiedTypeIdentifier ->
  (WrittenTypes.range * (WrittenTypes.range * string) * WrittenTypes.expr) list ->
  (WrittenTypeSupport.checkedExpression, string) result

val access :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  string ->
  (WrittenTypeSupport.checkedExpression, string) result

val update :
  WrittenLambdaSupport.expressionChecker ->
  WrittenCallSupport.literalChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  ((WrittenTypes.range * string) * WrittenTypes.range * WrittenTypes.expr) list ->
  (WrittenTypeSupport.checkedExpression, string) result
