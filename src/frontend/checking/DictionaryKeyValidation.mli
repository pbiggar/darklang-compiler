(* Public key validation is specialized to the occupied parts of each key value. *)
val name : AST.semanticType -> string

val children :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  AST.semanticType ->
  AST.semanticType list

val needsValidation :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  AST.semanticType ->
  bool

val generate :
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  HelperDependencies.TypeSet.t ->
  AST.functionDef StringOrder.Map.t
