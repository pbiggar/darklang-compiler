(* CheckLambdas.mli - Infer lambda binders, constraints, currying, and return types. *)
val check :
  ExpressionSupport.expressionChecker ->
  Types.typeEnv ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.genericFuncRegistry ->
  AST.warningSettings ->
  AST.moduleRegistry ->
  Types.aliasRegistry ->
  AST.semanticType option ->
  AST.lambdaParameter NonEmptyList.t ->
  AST.semanticType option ->
  AST.expr ->
  (AST.semanticType * AST.expr, CheckingDiagnostics.typeError) result
