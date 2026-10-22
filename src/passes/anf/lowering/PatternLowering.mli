(* PatternLowering.mli - Lower ordered match alternatives and typed pattern projections. *)
val lowerMatch :
  LoweringCallbacks.expressionLowerer ->
  LoweringCallbacks.boundAtomLowerer ->
  TypeRegistries.functionIdRegistry ->
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  LoweringCallbacks.listRegionPolicy ->
  CheckedAST.expr ->
  CheckedAST.matchCase list ->
  ANF.varGen ->
  TypeRegistries.varEnv ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  AST.moduleRegistry ->
  (ANF.aExpr * ANF.varGen, string) result
