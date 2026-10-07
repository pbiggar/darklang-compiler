(* Substitute concrete types and instantiate generic checked functions. *)
type substitution = AST.semanticType StringOrder.Map.t

val applySubstToType : substitution -> AST.semanticType -> AST.semanticType

val firstDeclaredRecordFields :
  (string * AST.semanticType) list -> (string * AST.semanticType) list

val buildDeclaredRecordFieldSubst :
  TypeRegistries.recordTypeInfo -> AST.semanticType list -> substitution option

val recordDescriptor :
  string ->
  AST.semanticType list ->
  TypeRegistries.recordTypeInfo ->
  ANF.recordDescriptor

val boxedSumDescriptor :
  string ->
  string list ->
  AST.semanticType list ->
  AST.semanticType list ->
  (ANF.recordDescriptor, string) result

val matchTypePattern :
  AST.semanticType ->
  AST.semanticType ->
  ((string * AST.semanticType) list, string) result

val consolidateTypeBindings :
  (string * AST.semanticType) list -> (substitution, string) result

val applySubstToExpr : substitution -> CheckedAST.expr -> CheckedAST.expr

val resolveAliasType :
  TypeRegistries.aliasRegistry -> AST.semanticType -> AST.semanticType

val resolveAliasesInTypeRegistry :
  TypeRegistries.aliasRegistry ->
  TypeRegistries.typeRegistry ->
  TypeRegistries.typeRegistry

val resolveAliasesInFunction :
  TypeRegistries.aliasRegistry ->
  CheckedAST.functionDef ->
  CheckedAST.functionDef

val specializeFunction :
  AST.functionId ->
  CheckedAST.functionDef ->
  AST.semanticType list ->
  CheckedAST.functionDef
