(* Contextual lambda inference and currying from WrittenLambdaSupport.mli. *)
type expressionChecker =
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  (WrittenTypeSupport.checkedExpression, string) result

val check :
  expressionChecker ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.range ->
  WrittenTypes.letPattern list ->
  WrittenTypes.expr ->
  WrittenTypes.range ->
  WrittenTypes.range ->
  (WrittenTypeSupport.checkedExpression, string) result
