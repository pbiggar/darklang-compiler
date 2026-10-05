(* CacheIdentity.fs - Define stable dependency and native-code cache identities. *)
[@@@warning "-30"]
type 'a comparer = {equals : 'a -> 'a -> bool; getHashCode : 'a -> int}

(* Object identity hashes stay stable while the object lives. Weak keys avoid
   extending a compilation session's lifetime. Hashes are runtime-local, as
   are RuntimeHelpers.GetHashCode and the source's salted string hashes. *)
module Identities = Ephemeron.K1.Make(struct
  type t = Obj.t
  let equal left right = left == right
  let hash _ = 0
end)
let identities = Identities.create 127
let nextIdentity = ref 0
let identityHash value =
  let key = Obj.repr value in
  match Identities.find_opt identities key with
  | Some hash -> hash
  | None -> incr nextIdentity; Identities.add identities key !nextIdentity; !nextIdentity
let wrap32 value = Int32.to_int value
let addHash hash value = wrap32 (Int32.logxor (Int32.mul (Int32.of_int hash) 397l) (Int32.of_int value))
let idHash id =
  let ordinal = AST.functionIdValue id in
  wrap32 (Int32.logxor (Int64.to_int32 ordinal) (Int64.to_int32 (Int64.shift_right_logical ordinal 32)))
(* OCaml hashes a string's complete bytes, but only a bounded number of array
   elements. Hashing decoded unit arrays made long shared-prefix function
   names collide, defeating the source's name-indexed cache fast path. These
   function-key equality contracts compare the complete stored strings, so
   the runtime-local byte hash is congruent with equality without decoding. *)
let stringHash value = Hashtbl.hash value
let optionEqual equal left right = match left, right with
  | None, None -> true | Some left, Some right -> equal left right
  | None, Some _ | Some _, None -> false
let canonicalStringMap entries = StringOrder.Map.of_seq (StringOrder.Map.to_seq entries)
let canonicalFunctionMap entries = FunctionIdMap.ofSeq (FunctionIdMap.toSeq entries)
module Functions = SpecializationIdentity.FunctionSet

(* Normalize ordered containers before structural equality. Their balancing
   history is invisible to F# Map/Set equality. No producer object is replaced
   at the reference-identity comparison boundaries below. *)
let canonicalReleaseSummary (value:LIR.arm64ReleasePlanSummary) =
  {value with LIR.listDecHelperLabels=StringOrder.Set.of_seq (StringOrder.Set.to_seq value.LIR.listDecHelperLabels);
    plannedListDecHelpers=canonicalStringMap value.LIR.plannedListDecHelpers;
    dictDecHelperLabels=StringOrder.Set.of_seq (StringOrder.Set.to_seq value.LIR.dictDecHelperLabels);
    plannedDictDecHelpers=canonicalStringMap value.LIR.plannedDictDecHelpers}
let canonicalRequirements (value:LIR.arm64RcHelperRequirements) =
  {value with LIR.listDecHelperLabels=StringOrder.Set.of_seq (StringOrder.Set.to_seq value.LIR.listDecHelperLabels);
    plannedListDecHelpers=canonicalStringMap value.LIR.plannedListDecHelpers;
    plannedGenericDecHelpers=canonicalStringMap (StringOrder.Map.map
      (fun (helper:LIR.arm64PlannedGenericDecHelper) -> {helper with LIR.releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.of_seq (LIR.RcReleasePlanMemoKeySet.to_seq helper.LIR.releasePlanMemoKeys)})
      value.LIR.plannedGenericDecHelpers);
    plannedDictDecHelpers=canonicalStringMap value.LIR.plannedDictDecHelpers;
    dictDecHelperLabels=StringOrder.Set.of_seq (StringOrder.Set.to_seq value.LIR.dictDecHelperLabels);
    releasePlanSummaries=LIR.ReleasePlanSummaryMap.of_seq (Seq.map
      (fun (key, summary) -> key, canonicalReleaseSummary summary)
      (LIR.ReleasePlanSummaryMap.to_seq value.LIR.releasePlanSummaries))}
let canonicalCodegenFacts (value:LIR.functionCodegenFacts) =
  {value with LIR.recursiveReleaseTypes=MemoryPlanning.SemanticTypeSet.of_seq (MemoryPlanning.SemanticTypeSet.to_seq value.LIR.recursiveReleaseTypes);
    refCountDecRequirements=LIR.RefCountDecRequirementMap.of_seq (LIR.RefCountDecRequirementMap.to_seq value.LIR.refCountDecRequirements);
    refCountIncRequirements=LIR.RcKindSet.of_seq (LIR.RcKindSet.to_seq value.LIR.refCountIncRequirements);
    rawSlotInitTypes=MemoryPlanning.SemanticTypeSet.of_seq (MemoryPlanning.SemanticTypeSet.to_seq value.LIR.rawSlotInitTypes);
    arm64RawSlotInitRetainTargets=Option.map (fun entries -> LIR.SemanticTypeMap.of_seq (LIR.SemanticTypeMap.to_seq entries)) value.LIR.arm64RawSlotInitRetainTargets;
    arm64RcHelperRequirements=Option.map canonicalRequirements value.LIR.arm64RcHelperRequirements;
    arm64GenericHelperIds=canonicalStringMap value.LIR.arm64GenericHelperIds}
let canonicalLirFunction (func:LIR.functionDef) =
  {func with LIR.cfg={func.LIR.cfg with LIR.blocks=LIR.LabelMap.of_seq (LIR.LabelMap.to_seq func.LIR.cfg.LIR.blocks)};
    codegenFacts=Option.map canonicalCodegenFacts func.LIR.codegenFacts}
let lirFunctionEquals left right = canonicalLirFunction left = canonicalLirFunction right
let canonicalMirFunction (func:MIR.functionDef) =
  {func with MIR.cfg={func.MIR.cfg with MIR.blocks=MIR.LabelMap.of_seq (MIR.LabelMap.to_seq func.MIR.cfg.MIR.blocks)};
    floatRegs=MIR.IntSet.of_seq (MIR.IntSet.to_seq func.MIR.floatRegs)}
let canonicalRegistries (value:AST_to_ANF.registries) =
  {AST_to_ANF.scopeContracts=canonicalFunctionMap (FunctionIdMap.map
      (fun _ (contract:Destruction.functionScopeContract) -> {contract with Destruction.calls=Functions.of_seq (Functions.to_seq contract.Destruction.calls)}) value.AST_to_ANF.scopeContracts);
    inertFunctionScopes=Functions.of_seq (Functions.to_seq value.AST_to_ANF.inertFunctionScopes);
    typeReg=canonicalStringMap value.AST_to_ANF.typeReg;
    typeNames={CheckedAST.typeNames=CheckedAST.TypeIdMap.of_seq (CheckedAST.TypeIdMap.to_seq value.AST_to_ANF.typeNames.CheckedAST.typeNames)};
    recordFieldsReg=canonicalStringMap value.AST_to_ANF.recordFieldsReg;
    recordTypeParamsReg=canonicalStringMap value.AST_to_ANF.recordTypeParamsReg;
    variantLookup=canonicalStringMap value.AST_to_ANF.variantLookup;
    sumMetadata={LoweringPrimitives.names=StringOrder.Set.of_seq (StringOrder.Set.to_seq value.AST_to_ANF.sumMetadata.LoweringPrimitives.names);
      cases=canonicalStringMap (StringOrder.Map.map canonicalStringMap value.AST_to_ANF.sumMetadata.LoweringPrimitives.cases)};
    rcSumShapeReg=canonicalStringMap (StringOrder.Map.map
      (fun (shape:MemoryModel.rcSumShapeInfo) -> {shape with MemoryModel.unaryPayloadTags=MemoryModel.IntSet.of_seq (MemoryModel.IntSet.to_seq shape.MemoryModel.unaryPayloadTags)}) value.AST_to_ANF.rcSumShapeReg);
    funcReg=canonicalFunctionMap value.AST_to_ANF.funcReg;
    functionIds=canonicalStringMap value.AST_to_ANF.functionIds;
    functionNames=canonicalFunctionMap value.AST_to_ANF.functionNames;
    funcParams=canonicalStringMap value.AST_to_ANF.funcParams;
    moduleRegistry=canonicalStringMap value.AST_to_ANF.moduleRegistry;
    recursiveMembers=canonicalFunctionMap value.AST_to_ANF.recursiveMembers}

(* The finalized immutable LIR object is the version token. A function cache
   hit retains that object, while a changed body produces another one. The
   target and options complete the identity without hashing a whole LIR body
   on every session-cache lookup. *)
type functionVersion = {unitName : string; functionId : AST.functionId; target : Platform.target; options : CompilerOptions.compilerOptions; body : LIR.functionDef}
let functionVersion unitName functionId target options body = {unitName;functionId;target;options;body}
let functionVersionEquals left right =
  left.unitName=right.unitName && left.functionId=right.functionId
  && left.target=right.target && left.options=right.options && left.body == right.body
let functionVersionHashCode version =
  addHash (Hashtbl.hash (version.unitName,version.functionId,version.target,version.options)) (identityHash version.body)

(* The catalog indexes by canonical ID for direct-call lookup, while Version
   retains the exact producer identity. None marks a collision or ambiguity. *)
type functionSummary = {version : functionVersion option; purity : MIROptimizationFacts.puritySummary; constantReturn : (AST.semanticType * MIR.operand) option; arm64Writes : ARM64CalleeClobbers.writes option; x64Writes : X64CalleeClobbers.writes option}
(* Cache reuse depends on the facts a pass can observe. A new producer body
   with identical facts leaves its callers' optimized output unchanged. *)
type functionSummaryFacts = {purity : MIROptimizationFacts.puritySummary; constantReturn : (AST.semanticType * MIR.operand) option; arm64Writes : ARM64CalleeClobbers.writes option; x64Writes : X64CalleeClobbers.writes option}
let summaryFacts (summary:functionSummary) : functionSummaryFacts =
  {purity=summary.purity;constantReturn=summary.constantReturn;arm64Writes=summary.arm64Writes;x64Writes=summary.x64Writes}
(* An explicit pessimistic result for a direct callee whose body is unavailable
   or whose canonical ID resolves to more than one body. *)
let unknownSummary : functionSummary = {version=None;
  purity={MIROptimizationFacts.observableEffects=true;readsMutableState=true;mayTrap=true;mayDiverge=true};
  constantReturn=None;arm64Writes=None;x64Writes=None}
let functionSummaryEquals (left:functionSummary) (right:functionSummary) =
  optionEqual functionVersionEquals left.version right.version && summaryFacts left=summaryFacts right
let mergeFunctionSummaries left right =
  FunctionIdMap.fold (fun summaries id summary -> match FunctionIdMap.tryFind id summaries with
    | None -> FunctionIdMap.add id summary summaries
    | Some existing when functionSummaryEquals existing summary && Option.is_some summary.version -> summaries
    | Some _ -> FunctionIdMap.add id unknownSummary summaries) left right
let lirFunctionReferenceComparer : LIR.functionDef comparer = {equals=(==);getHashCode=identityHash}
type allocatedLirFunctionKey = {arch : Platform.arch; func : LIR.functionDef}
let allocatedLirFunctionKeyNameHashComparer : allocatedLirFunctionKey comparer =
  {equals=(fun left right -> left.arch=right.arch && lirFunctionEquals left.func right.func);
    getHashCode=(fun key -> stringHash key.func.LIR.name)}
let objectReferenceComparer : Obj.t comparer = {equals=(==);getHashCode=identityHash}
type anfDependencyKey = {functions : CheckedAST.functionDef list; localRegistries : AST_to_ANF.registries; nonInlineableFunctionNames : Functions.t}
let anfDependencyKeyNameHashComparer : anfDependencyKey comparer =
  {equals=(fun left right -> left.functions=right.functions
      && canonicalRegistries left.localRegistries=canonicalRegistries right.localRegistries
      && Functions.equal left.nonInlineableFunctionNames right.nonInlineableFunctionNames);
    getHashCode=(fun key -> let hash=List.fold_left (fun hash (func:CheckedAST.functionDef) -> addHash hash (idHash func.CheckedAST.id)) 17 key.functions in
      Functions.fold (fun id hash -> addHash hash (idHash id)) key.nonInlineableFunctionNames hash)}
type compiledDependencyConfig = {target : Platform.target; options : CompilerOptions.compilerOptions; nonInlineableFunctionNames : Functions.t; knownSummaries : functionSummaryFacts FunctionIdMap.t}
let compiledDependencyConfigComparer : compiledDependencyConfig comparer =
  {equals=(fun left right -> left.target=right.target && left.options=right.options
      && Functions.equal left.nonInlineableFunctionNames right.nonInlineableFunctionNames
      && FunctionIdMap.toList left.knownSummaries=FunctionIdMap.toList right.knownSummaries);
    getHashCode=(fun key -> Hashtbl.hash (key.target,key.options,Functions.elements key.nonInlineableFunctionNames,FunctionIdMap.toList key.knownSummaries))}
type mirOptimizationKey = {func : MIR.functionDef; options : MIROptimizationFacts.optimizeOptions; effectFreeCalls : Functions.t}
let mirOptimizationKeyNameHashComparer : mirOptimizationKey comparer =
  {equals=(fun left right -> canonicalMirFunction left.func=canonicalMirFunction right.func
      && left.options=right.options && Functions.equal left.effectFreeCalls right.effectFreeCalls);
    getHashCode=(fun key -> stringHash key.func.MIR.name)}
type mirOptimizationCache = mirOptimizationKey -> (unit -> MIR.functionDef) -> MIR.functionDef
type allocatedLirFunctionCache = Platform.arch -> LIR.functionDef -> (unit -> LIR.functionDef) -> LIR.functionDef
type callAwareLirFunctionCache = LIR.functionDef -> ARM64CalleeClobbers.writes FunctionIdMap.t -> (unit -> LIR.functionDef) -> LIR.functionDef
type callAwareLirFunctionKey = {base : LIR.functionDef; callees : ARM64CalleeClobbers.writes FunctionIdMap.t}
let callAwareLirFunctionKeyComparer : callAwareLirFunctionKey comparer =
  {equals=(fun left right -> left.base == right.base && FunctionIdMap.toList left.callees=FunctionIdMap.toList right.callees);
    getHashCode=(fun key -> addHash (identityHash key.base) (Hashtbl.hash (FunctionIdMap.toList key.callees)))}
type functionCompilationCaches = {optimizeMir : mirOptimizationCache; allocateLir : allocatedLirFunctionCache; allocateCallAwareLir : callAwareLirFunctionCache}
let arm64InstructionChunkReferenceComparer : Symbolic.instr list comparer = {equals=(==);getHashCode=identityHash}
let arm64InstructionChunkGroupReferenceComparer : Symbolic.instr list list comparer = {equals=(==);getHashCode=identityHash}
type arm64MetadataGroupKey = {functions : LIR.functionDef list}
let sameFunctionObjects left right = List.length left=List.length right && List.for_all2 (==) left right
let functionObjectsHash functions = List.fold_left (fun hash func -> addHash hash (identityHash func)) 17 functions
let arm64MetadataGroupKeyComparer : arm64MetadataGroupKey comparer =
  {equals=(fun left right -> sameFunctionObjects left.functions right.functions);
    getHashCode=(fun key -> functionObjectsHash key.functions)}
type arm64FunctionGroupKey = {functions : LIR.functionDef list; target : ARM64.targetConfig; options : ARM64CodeGenTypes.codeGenOptions}
let arm64FunctionGroupKeyComparer : arm64FunctionGroupKey comparer =
  {equals=(fun left right -> left.target=right.target && left.options=right.options && sameFunctionObjects left.functions right.functions);
    getHashCode=(fun key -> addHash (functionObjectsHash key.functions) (Hashtbl.hash (key.target,key.options)))}
type arm64HelperCacheKey = {target : ARM64.targetConfig; options : ARM64CodeGenTypes.codeGenOptions; helper : Backend_Arm64_CodeGen.helperCacheKey}
let arm64HelperCacheKeyComparer : arm64HelperCacheKey comparer = {equals=(=);getHashCode=Hashtbl.hash}
let mergeMirRegistryOverlay baseRegistry overlay =
  StringOrder.Map.fold (fun name value merged -> StringOrder.Map.add name value merged) overlay baseRegistry
let projectMirRegistryOverlay (baseVariants,baseRecords) localVariantLookup localRecordFields =
  let localVariants=ANF_to_MIR.buildVariantRegistry localVariantLookup in
  let localRecords=ANF_to_MIR.buildRecordRegistry localRecordFields in
  mergeMirRegistryOverlay baseVariants localVariants, mergeMirRegistryOverlay baseRecords localRecords
