(* LowerListRegions.mli - Lower verified owned arrays and scalar joins to native ANF operations. *)
type lowerScalar =
  CheckedAST.expr ->
  ANF.varGen ->
  TypeRegistries.varEnv ->
  (ANF.aExpr * ANF.varGen, string) result

val releaseRuntimeSmall : ANF.atom -> ANF.cExpr

val lower :
  (string -> AST.functionId) ->
  lowerScalar ->
  TypeRegistries.varEnv ->
  ANF.varGen ->
  ListRegion.ownedRegion ->
  (ANF.aExpr * ANF.varGen, string) result
