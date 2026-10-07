(* ExpressionSupport.mli - Recursive checking contract and call-argument names. *)
type expressionChecker =
  AST.expr ->
  Types.typeEnv ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.genericFuncRegistry ->
  AST.warningSettings ->
  AST.moduleRegistry ->
  Types.aliasRegistry ->
  AST.semanticType option ->
  (AST.semanticType * AST.expr, CheckingDiagnostics.typeError) result

val paramNameForLegacyError :
  string list StringOrder.Map.t -> string -> int -> string
