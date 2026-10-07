(* ResolveDeclarations.mli - Resolve declaration identities, recursive groups, and source names. *)
type topLevelDeclarationSummary = {
  typeReg : Types.typeRegistry;
  recordTypeParams : string list StringOrder.Map.t;
  aliasReg : Types.aliasRegistry;
  variantLookup : Types.variantLookup;
  funcSigs : (AST.semanticType list * AST.semanticType) StringOrder.Map.t;
  funcParamNames : Types.funcParamNameRegistry;
  genericFuncs : string list StringOrder.Map.t;
}

val declarationResolutionEnvironment :
  AST.topLevel list ->
  AST.moduleRegistry ->
  bool ->
  NameResolution.resolutionEnvironment

val resolveRecursiveDeclarationGroups : AST.topLevel list -> AST.topLevel list

val resolveProgramNames :
  NameResolution.resolutionEnvironment ->
  Types.aliasRegistry ->
  StringOrder.Set.t ->
  AST.program ->
  (AST.program, CheckingDiagnostics.typeError) result
