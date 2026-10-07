(* CheckFunctions.mli - Check function bodies and collect concrete declaration specializations. *)
module SpecificationSet : Set.S with type elt = string * AST.semanticType list

val checkFunctionDefWithSumTypeNames :
  Types.funcParamNameRegistry ->
  StringOrder.Set.t ->
  Types.indexedSumTypeRegistry ->
  AST.functionDef ->
  Types.typeEnv ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.genericFuncRegistry ->
  AST.warningSettings ->
  AST.moduleRegistry ->
  Types.aliasRegistry ->
  (AST.functionDef, CheckingDiagnostics.typeError) result

val specializeFunctionForTypeCheck :
  AST.functionDef ->
  AST.semanticType list ->
  (AST.functionDef, CheckingDiagnostics.typeError) result

val collectTypeAppSpecs : AST.expr -> SpecificationSet.t
