(* Continuation inference and local recursive bindings from WrittenLetSupport.mli. *)
val check :
  WrittenLambdaSupport.expressionChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.range ->
  WrittenTypes.letPattern ->
  WrittenTypes.expr ->
  WrittenTypes.expr ->
  (WrittenTypeSupport.checkedExpression, string) result
