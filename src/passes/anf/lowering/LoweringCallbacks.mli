(* LoweringCallbacks.mli - Typed recursive entry points shared by expression-family handlers. *)
type listRegionPolicy =
  | PreserveValues
  | OptimizeLists of SpecializationIdentity.FunctionSet.t

val listRegions :
  TypeRegistries.functionIdRegistry ->
  SpecializationIdentity.FunctionSet.t ->
  listRegionPolicy

type expressionLowerer =
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  listRegionPolicy ->
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
  listRegionPolicy ->
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
  listRegionPolicy ->
  CheckedAST.expr ->
  ANF.varGen ->
  TypeRegistries.varEnv ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  AST.moduleRegistry ->
  (ANF.aExpr * ANF.atom * ANF.varGen, string) result
