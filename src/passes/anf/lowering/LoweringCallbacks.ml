(* LoweringCallbacks.ml - Typed recursive entry points shared by expression-family handlers. *)
type expressionLowerer =
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  SpecializationIdentity.FunctionSet.t ->
  CheckedAST.expr ->
  ANF.varGen ->
  TypeRegistries.varEnv ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  AST.moduleRegistry ->
  (ANF.aExpr * ANF.varGen, string) result

type atomLowerer =
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  SpecializationIdentity.FunctionSet.t ->
  CheckedAST.expr ->
  ANF.varGen ->
  TypeRegistries.varEnv ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  AST.moduleRegistry ->
  (ANF.atom * (ANF.tempId * ANF.cExpr) list * ANF.varGen, string) result

type boundAtomLowerer =
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  SpecializationIdentity.FunctionSet.t ->
  CheckedAST.expr ->
  ANF.varGen ->
  TypeRegistries.varEnv ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  AST.moduleRegistry ->
  (ANF.aExpr * ANF.atom * ANF.varGen, string) result
