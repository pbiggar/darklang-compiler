(* CompilationCacheIdentity.mli - Define stable dependency and native-code cache identities. *)
[@@@warning "-30"]

type 'a comparer = { equals : 'a -> 'a -> bool; getHashCode : 'a -> int }

type functionVersion = private {
  unitName : string;
  functionId : AST.functionId;
  target : Platform.target;
  options : CompilerOptions.compilerOptions;
  body : LIR.functionDef;
}

val functionVersion :
  string ->
  AST.functionId ->
  Platform.target ->
  CompilerOptions.compilerOptions ->
  LIR.functionDef ->
  functionVersion

val functionVersionEquals : functionVersion -> functionVersion -> bool
val functionVersionHashCode : functionVersion -> int

type functionSummary = {
  version : functionVersion option;
  purity : MIROptimizationFacts.puritySummary;
  constantReturn : (AST.semanticType * MIR.operand) option;
  arm64Writes : ARM64CalleeClobbers.writes option;
  x64Writes : X64CalleeClobbers.writes option;
}

type functionSummaryFacts = {
  purity : MIROptimizationFacts.puritySummary;
  constantReturn : (AST.semanticType * MIR.operand) option;
  arm64Writes : ARM64CalleeClobbers.writes option;
  x64Writes : X64CalleeClobbers.writes option;
}

val summaryFacts : functionSummary -> functionSummaryFacts
val unknownSummary : functionSummary
val functionSummaryEquals : functionSummary -> functionSummary -> bool

val mergeFunctionSummaries :
  functionSummary FunctionIdMap.t ->
  functionSummary FunctionIdMap.t ->
  functionSummary FunctionIdMap.t

val lirFunctionReferenceComparer : LIR.functionDef comparer

type allocatedLirFunctionKey = { arch : Platform.arch; func : LIR.functionDef }

val allocatedLirFunctionKeyNameHashComparer : allocatedLirFunctionKey comparer
val objectReferenceComparer : Obj.t comparer

type anfDependencyKey = {
  functions : CheckedAST.functionDef list;
  localRegistries : AST_to_ANF.registries;
  nonInlineableFunctionNames : SpecializationIdentity.FunctionSet.t;
}

val anfDependencyKeyNameHashComparer : anfDependencyKey comparer

type compiledDependencyConfig = {
  target : Platform.target;
  options : CompilerOptions.compilerOptions;
  nonInlineableFunctionNames : SpecializationIdentity.FunctionSet.t;
  knownSummaries : functionSummaryFacts FunctionIdMap.t;
}

val compiledDependencyConfigComparer : compiledDependencyConfig comparer

type mirOptimizationKey = {
  func : MIR.functionDef;
  options : MIROptimizationFacts.optimizeOptions;
  effectFreeCalls : SpecializationIdentity.FunctionSet.t;
}

val mirOptimizationKeyNameHashComparer : mirOptimizationKey comparer

type mirOptimizationCache =
  mirOptimizationKey -> (unit -> MIR.functionDef) -> MIR.functionDef

type allocatedLirFunctionCache =
  Platform.arch ->
  LIR.functionDef ->
  (unit -> LIR.functionDef) ->
  LIR.functionDef

type callAwareLirFunctionCache =
  LIR.functionDef ->
  ARM64CalleeClobbers.writes FunctionIdMap.t ->
  (unit -> LIR.functionDef) ->
  LIR.functionDef

type callAwareLirFunctionKey = {
  base : LIR.functionDef;
  callees : ARM64CalleeClobbers.writes FunctionIdMap.t;
}

val callAwareLirFunctionKeyComparer : callAwareLirFunctionKey comparer

type functionCompilationCaches = {
  optimizeMir : mirOptimizationCache;
  allocateLir : allocatedLirFunctionCache;
  allocateCallAwareLir : callAwareLirFunctionCache;
}

val arm64InstructionChunkReferenceComparer : Symbolic.instr list comparer

val arm64InstructionChunkGroupReferenceComparer :
  Symbolic.instr list list comparer

type arm64MetadataGroupKey = { functions : LIR.functionDef list }

val arm64MetadataGroupKeyComparer : arm64MetadataGroupKey comparer

type arm64FunctionGroupKey = {
  functions : LIR.functionDef list;
  target : ARM64.targetConfig;
  options : ARM64CodeGenTypes.codeGenOptions;
}

val arm64FunctionGroupKeyComparer : arm64FunctionGroupKey comparer

type arm64HelperCacheKey = {
  target : ARM64.targetConfig;
  options : ARM64CodeGenTypes.codeGenOptions;
  helper : Backend_Arm64_CodeGen.helperCacheKey;
}

val arm64HelperCacheKeyComparer : arm64HelperCacheKey comparer
val lirFunctionEquals : LIR.functionDef -> LIR.functionDef -> bool

val projectMirRegistryOverlay :
  MIR.variantRegistry * MIR.recordRegistry ->
  LoweringPrimitives.variantLookup ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  MIR.variantRegistry * MIR.recordRegistry
