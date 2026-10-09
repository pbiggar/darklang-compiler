(* OrderingHelpers.mli - Generate complete canonical structural ordering expressions. *)
val buildCompareHelperExpr :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.indexedSumTypeRegistry ->
  EqualityHelpers.eqHelperExprMode ->
  AST.semanticType ->
  AST.expr ->
  AST.expr ->
  AST.expr
