(* WrittenUnwrapSupport.mli - Static Option/Result propagation checks. *)
val check :
  (WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  (WrittenTypeSupport.checkedExpression, string) result) ->
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WrittenTypes.expr ->
  (WrittenTypeSupport.checkedExpression, string) result

val returnHint :
  WrittenTypeSupport.globals -> WrittenTypes.expr -> AST.semanticType option
