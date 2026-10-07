(* Monomorphization.mli - Solve reachable generic instances and replace type applications. *)
val collectTypeApps :
  CheckedAST.symbols -> CheckedAST.expr -> SpecializationIdentity.SpecSet.t

val collectTypeAppsFromFunc :
  CheckedAST.symbols ->
  CheckedAST.functionDef ->
  SpecializationIdentity.SpecSet.t

val collectCalledFunctions :
  CheckedAST.expr -> SpecializationIdentity.FunctionSet.t

val specializeFromSpecs :
  CheckedAST.symbols ->
  SpecializationIdentity.genericFuncDefs ->
  SpecializationIdentity.SpecSet.t ->
  SpecializationIdentity.specializationResult

val replaceTypeApps : CheckedAST.symbols -> CheckedAST.expr -> CheckedAST.expr

val replaceTypeAppsWithRegistry :
  CheckedAST.symbols ->
  SpecializationIdentity.specRegistry ->
  CheckedAST.expr ->
  (CheckedAST.expr, string) result

val replaceTypeAppsInFunc :
  CheckedAST.symbols -> CheckedAST.functionDef -> CheckedAST.functionDef

val replaceTypeAppsInFuncWithRegistry :
  CheckedAST.symbols ->
  SpecializationIdentity.specRegistry ->
  CheckedAST.functionDef ->
  (CheckedAST.functionDef, string) result

val replaceTypeAppsInProgramWithRegistry :
  SpecializationIdentity.specRegistry ->
  CheckedAST.program ->
  (CheckedAST.program, string) result

val monomorphizeWithGenericFuncDefs :
  SpecializationIdentity.genericFuncDefs ->
  CheckedAST.program ->
  CheckedAST.program

val programNeedsLambdaLowering : StringOrder.Set.t -> CheckedAST.program -> bool
