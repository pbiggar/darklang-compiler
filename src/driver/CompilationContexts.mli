(* CompilationContexts.mli - Define stdlib, preamble, and user-compilation interfaces. *)
[@@@warning "-30"]

val buildBaseFuncNames : AST_to_ANF.registries -> StringOrder.Set.t

val buildLambdaLiftFunctionCatalog :
  AST_to_ANF.registries ->
  StringOrder.Set.t ->
  (string * AST.semanticType) FunctionIdMap.t ->
  LiftFunctions.functionCatalog

val mergeReturnTypes :
  (string * AST.semanticType) FunctionIdMap.t ->
  (string * AST.semanticType) FunctionIdMap.t ->
  (string * AST.semanticType) FunctionIdMap.t

val packageCatalogFunctionNames : StringOrder.Set.t

val buildPackageCatalogGenericCallers :
  SpecializationIdentity.genericFuncDefs -> StringOrder.Set.t

type checkedValueArtifact = {
  bindingCursor : int;
  typ : AST.semanticType;
  body : CheckedAST.expr;
}

val checkedValueArtifacts :
  CheckedAST.program -> checkedValueArtifact StringOrder.Map.t

type pipelineContext = {
  symbols : CheckedAST.symbols;
  target : Platform.target;
  typeCheckEnv : Types.typeCheckEnv;
  writtenEnvironment : WrittenChecking.environment option;
  checkedValues : checkedValueArtifact StringOrder.Map.t;
  genericFuncDefs : SpecializationIdentity.genericFuncDefs;
  specRegistry : SpecializationIdentity.specRegistry;
  registries : AST_to_ANF.registries;
  baseFuncNames : StringOrder.Set.t;
  lambdaLiftFunctions : LiftFunctions.functionCatalog;
  lambdaLiftTypeReg : TypeRegistries.typeRegistry;
  lambdaLiftVariantLookup : LoweringPrimitives.variantLookup;
  projectedMirRegistries : MIR.variantRegistry * MIR.recordRegistry;
  returnTypes : (string * AST.semanticType) FunctionIdMap.t;
  packageCatalogGenericCallers : StringOrder.Set.t;
}

val includeCompiledFunctions :
  ANF.functionDef list -> pipelineContext -> pipelineContext

val buildContext :
  Platform.target ->
  CheckedAST.symbols ->
  Types.typeCheckEnv ->
  checkedValueArtifact StringOrder.Map.t ->
  SpecializationIdentity.genericFuncDefs ->
  SpecializationIdentity.specRegistry ->
  AST_to_ANF.registries ->
  StringOrder.Set.t ->
  (string * AST.semanticType) FunctionIdMap.t ->
  pipelineContext

type preambleContext = {
  context : pipelineContext;
  anfFunctions : ANF.functionDef list;
  typeMap : ANF.typeMap;
  symbolicFunctions : LIR.functionDef list;
  callGraphSummaries : CompilationCacheIdentity.functionSummary FunctionIdMap.t;
  symbolicCallGraph : SpecializationIdentity.FunctionSet.t FunctionIdMap.t;
}

type preambleAnalysis = {
  typedAST : CheckedAST.program;
  typeCheckEnv : Types.typeCheckEnv;
  writtenEnvironment : WrittenChecking.environment option;
  genericFuncDefs : SpecializationIdentity.genericFuncDefs;
}

type stdlibResult = {
  typedAST : CheckedAST.program;
  context : pipelineContext;
  allocatedFunctions : LIR.functionDef list;
  callGraphSummaries : CompilationCacheIdentity.functionSummary FunctionIdMap.t;
  stdlibCallGraph : SpecializationIdentity.FunctionSet.t FunctionIdMap.t;
  stdlibAnfFunctions : ANF.functionDef StringOrder.Map.t;
  stdlibAnfOptimizationCandidates : ANF.functionDef StringOrder.Map.t;
  stdlibInlineCandidates : InliningCommon.functionInfo FunctionIdMap.t;
  stdlibAnfCallGraph : SpecializationIdentity.FunctionSet.t FunctionIdMap.t;
  stdlibTypeMap : ANF.typeMap;
}

type compileContext =
  | StdlibOnly of stdlibResult
  | StdlibWithPreamble of stdlibResult * preambleContext

type packageCustomType = {
  hash : string;
  typeArguments : packageCustomType list;
}

type catalogPackageLocation = {
  visibleInBranches : string list;
  owner : string;
  modules : string list;
  name : string;
}

type packageValueEvaluatorState =
  | Available of AST.expr
  | Unavailable
  | EvaluationFailure

type typedPackageValueEvaluator = {
  resultType : AST.semanticType;
  state : packageValueEvaluatorState;
}

type packageValueCatalogEntry = {
  valueHash : string;
  runtimeType : packageCustomType;
  locations : catalogPackageLocation list;
  evaluator : typedPackageValueEvaluator;
}

type packageValueCatalog =
  | PackageValueCatalog of packageValueCatalogEntry list

val emptyPackageValueCatalog : packageValueCatalog

type sourceUnit = {
  name : string;
  purpose : NameSyntax.SourceUnitPurpose.t;
  source : string;
}

type compileRequest = {
  context : compileContext;
  mode : CompilerOptions.compileMode;
  sources : sourceUnit AST.nonEmptyList;
  allowInternal : bool;
  verbosity : int;
  options : CompilerOptions.compilerOptions;
  packageValues : packageValueCatalog;
  packageManager : PackageManager.config option;
  passTimingRecorder : CompilerOptions.passTimingRecorder option;
  session : CompilationSession.compilationSession option;
}
