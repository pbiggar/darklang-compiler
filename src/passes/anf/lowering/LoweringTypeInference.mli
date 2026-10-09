(* LoweringTypeInference.mli - Recover checked expression types for representation-directed ANF lowering. *)
val inferTypeCore :
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  CheckedAST.expr ->
  AST.semanticType CheckedAST.BindingIdMap.t ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  AST.moduleRegistry ->
  (AST.semanticType, string) result
