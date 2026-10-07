(* CheckMatches.mli - Check patterns, guards, result inference, and exhaustiveness. *)
val check :
  ExpressionSupport.expressionChecker ->
  StringOrder.Set.t ->
  Types.indexedSumTypeRegistry ->
  Types.typeEnv ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.genericFuncRegistry ->
  AST.warningSettings ->
  AST.moduleRegistry ->
  Types.aliasRegistry ->
  AST.semanticType option ->
  AST.expr ->
  AST.matchCase list ->
  (AST.semanticType * AST.expr, CheckingDiagnostics.typeError) result
