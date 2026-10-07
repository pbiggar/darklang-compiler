(* ExtractListRegions.mli - Recognize closed list computations and prove scalar scope eligibility. *)
val scopeContracts :
  (AST.semanticType CheckedAST.BindingIdMap.t ->
  CheckedAST.expr ->
  (AST.semanticType, string) result) ->
  CheckedAST.functionDef list ->
  Destruction.functionScopeContract FunctionIdMap.t

val tryExtract :
  SpecializationIdentity.FunctionSet.t ->
  string FunctionIdMap.t ->
  AST.semanticType CheckedAST.BindingIdMap.t ->
  (AST.semanticType CheckedAST.BindingIdMap.t ->
  CheckedAST.expr ->
  (AST.semanticType, string) result) ->
  (CheckedAST.expr -> ClosureAnalysis.BindingSet.t) ->
  CheckedAST.expr ->
  ListRegion.functionalRegion option
