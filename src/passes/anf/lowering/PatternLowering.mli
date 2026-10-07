(* PatternLowering.mli - Lower ordered match alternatives and typed pattern projections. *)
val lowerMatch :
  LoweringCallbacks.expressionLowerer ->
  LoweringCallbacks.atomLowerer ->
  LoweringCallbacks.boundAtomLowerer ->
  TypeRegistries.functionIdRegistry ->
  LoweringPrimitives.sumMetadata ->
  TypeRegistries.typeNameRegistry ->
  SpecializationIdentity.FunctionSet.t ->
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
