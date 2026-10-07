(* EqualityHelpers.mli - Generate complete structural equality source expressions. *)
type eqHelperExprMode = ExpandCurrent | UseHelperCall

val makeSimpleMatchCase : AST.pattern -> AST.expr -> AST.matchCase

val buildEqHelperExpr :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.indexedSumTypeRegistry ->
  eqHelperExprMode ->
  AST.semanticType ->
  AST.expr ->
  AST.expr ->
  AST.expr
