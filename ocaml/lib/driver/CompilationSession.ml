(* CompilationSession.fs - Own bounded compilation caches and their explicit session lifetime. *)
module C=CompilationCacheIdentity
module F=SpecializationIdentity.FunctionSet
(* Dictionary keys use the source's equality contracts. Preserve insertion
   order for observable metrics; replacing an entry retains its position. *)
type ('key,'value) cache={equals:'key->'key->bool;mutable entries:('key*'value) list}
let cache equals={equals;entries=[]}
let find cache key=List.find_map (fun (existing,value)->if cache.equals existing key then Some value else None) cache.entries
let store cache key value=
 let rec replace=function []->[key,value]|(existing,_)::rest when cache.equals existing key->(existing,value)::rest|entry::rest->entry::replace rest in cache.entries<-replace cache.entries
let clear cache=cache.entries<-[]
let count cache=List.length cache.entries
let contextEntries contexts identity equals=match find contexts identity with Some entries->entries|None->let entries=cache equals in store contexts identity entries;entries
let nestedCount contexts=List.fold_left (fun sum (_,entries)->sum+count entries) 0 contexts.entries
let token ()=Obj.repr (ref ())
let addCount left right=Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let increment count=count:=addCount !count 1
class compilationSession ?(collectCodegenMetrics=false) () =
 let jsonPlanning=new JsonPlanning.planningSession in
 let anfDependenciesByContext=cache (==) in
 let compiledDependenciesByIdentity=cache (==) in
 let optimizedMirFunctions=cache C.mirOptimizationKeyNameHashComparer.C.equals in
 let allocatedLirFunctions=cache C.allocatedLirFunctionKeyNameHashComparer.C.equals in
 let callAwareLirFunctions=cache C.callAwareLirFunctionKeyComparer.C.equals in
 let refinedLirFunctions=cache C.callAwareLirFunctionKeyComparer.C.equals in
 let reachableStdlibFunctionsByContext=cache (==) in
 let stdlibFunctionInventoryByContext=cache (==) in
 let reachableStdlibNamesByRootAndContext=cache (==) in
 let mirRegistriesByContext=cache (==) in
 let arm64MetadataGroupsByContext=cache (==) in
 let arm64FunctionGroupsByContext=cache (==) in
 let arm64FunctionsByContext=cache (==) in
 let arm64HelpersByContext=cache (==) in
 (* Prebuilt stdlib and preamble functions retain object identity across a
    compilation session. Keep an identity-indexed fast lane for functions
    that populated the structural cache, avoiding repeated deep CFG equality
    checks without retaining transient structurally equivalent functions. *)
 let arm64FunctionsByReferenceAndContext=cache (==) in
 let arm64StartContextIdentity=token () in
 let arm64RegistryIndependentFunctionContextIdentity=token () in
 (* Finalized functions carry every input needed by ARM64 conversion except
    RawSlotInit's nominal-type lookup. Share all other functions across
    executable registry contexts while retaining target/options segregation. *)
 let arm64GenericReleaseHelperContextIdentity=token () in
 let arm64RegistryIndependentHelperContextIdentity=token () in
 let arm64EmissionChunks=cache (==) in
 let arm64EmissionChunkGroups=cache (==) in
 let arm64ReleasePlanSummaries=cache (=) in
 let arm64CodegenMetrics=ref [] in
 let arm64LirOpMetrics=cache (=) in
 let arm64CodegenHitCount=ref 0 and arm64CodegenMissCount=ref 0 in
 let arm64ReleasePlanSummaryHitCount=ref 0 and arm64ReleasePlanSummaryMissCount=ref 0 in
 let anfDependencyHitCount=ref 0 and anfDependencyMissCount=ref 0 in
 let compiledDependencyHitCount=ref 0 and compiledDependencyMissCount=ref 0 in
 let mirOptimizationHitCount=ref 0 and mirOptimizationMissCount=ref 0 in
 let allocatedLirFunctionHitCount=ref 0 and allocatedLirFunctionMissCount=ref 0 in
 let stdlibReachabilityHitCount=ref 0 and stdlibReachabilityMissCount=ref 0 in
 let mirRegistryProjectionHitCount=ref 0 and mirRegistryProjectionMissCount=ref 0 in
 let arm64StartCodegenHitCount=ref 0 in
 let arm64MetadataGroupHitCount=ref 0 and arm64MetadataGroupMissCount=ref 0 in
 let arm64FunctionGroupHitCount=ref 0 and arm64FunctionGroupMissCount=ref 0 in
 let arm64HelperHitCount=ref 0 and arm64HelperMissCount=ref 0 in
 object
 val mutable disposed=false
 method jsonPlanning=jsonPlanning
 method arm64GenericReleaseHelperContextIdentity=arm64GenericReleaseHelperContextIdentity
 method convertAnfDependencies (contextIdentity:Obj.t) (key:C.anfDependencyKey) (convert:unit->(AST_to_ANF.functionConversion,string) result)=
  if disposed then Result.map (fun converted->converted,token ()) (convert ()) else
  let entries=contextEntries anfDependenciesByContext contextIdentity C.anfDependencyKeyNameHashComparer.C.equals in
  match find entries key with
  | Some result->increment anfDependencyHitCount;result
  | None->let result=Result.map (fun converted->converted,token ()) (convert ()) in store entries key result;increment anfDependencyMissCount;result
 method compileDependencies (dependencyIdentity:Obj.t) (config:C.compiledDependencyConfig) (compile:unit->(LIR.functionDef list*C.functionSummary FunctionIdMap.t,string) result)=
  if disposed || config.C.options.CompilerOptions.enableCoverage then compile () else
  let entries=contextEntries compiledDependenciesByIdentity dependencyIdentity C.compiledDependencyConfigComparer.C.equals in
  match find entries config with
  | Some result->increment compiledDependencyHitCount;result
  | None->let result=compile () in store entries config result;increment compiledDependencyMissCount;result
 method arm64LirOpExpansionRecorder : ARM64CodeGenTypes.lirOpExpansionRecorder option=
  if disposed || not collectCodegenMetrics then None else Some (fun functionName opcode detail symbolicInstructionCount elapsedTicks->
   let key=functionName,opcode,detail in
   match find arm64LirOpMetrics key with
   | Some (occurrences,symbolicInstructions,ticks)->store arm64LirOpMetrics key (addCount occurrences 1,addCount symbolicInstructions symbolicInstructionCount,Int64.add ticks elapsedTicks)
   | None->store arm64LirOpMetrics key (1,symbolicInstructionCount,elapsedTicks))
 method optimizeMirFunction (key:C.mirOptimizationKey) (optimize:unit->MIR.functionDef)=
  if disposed then optimize () else match find optimizedMirFunctions key with
  | Some optimized->increment mirOptimizationHitCount;optimized
  | None->let optimized=optimize () in store optimizedMirFunctions key optimized;increment mirOptimizationMissCount;optimized
 method allocateLirFunction arch func (allocate:unit->LIR.functionDef)=
  let key={C.arch;func} in if disposed then allocate () else match find allocatedLirFunctions key with
  | Some allocated->increment allocatedLirFunctionHitCount;allocated
  | None->let allocated=allocate () in store allocatedLirFunctions key allocated;increment allocatedLirFunctionMissCount;allocated
 method allocateCallAwareLirFunction base callees (allocate:unit->LIR.functionDef)=
  let key={C.base;callees} in if disposed then allocate () else match find callAwareLirFunctions key with
  | Some allocated->allocated|None->let allocated=allocate () in store callAwareLirFunctions key allocated;allocated
 method refineArm64LirFunction base callees (refine:unit->LIR.functionDef)=
  let key={C.base;callees} in if disposed then refine () else match find refinedLirFunctions key with
  | Some refined->refined|None->let refined=refine () in store refinedLirFunctions key refined;refined
 method reachableStdlibFunctions (contextIdentity:Obj.t) userCallGraph userFunctions stdlibCallGraph (stdlibFunctions:LIR.functionDef list)=
  let directCalls=DeadCodeElimination.directCallsFromFunctions userCallGraph userFunctions
   (* Calls between user functions cannot lead into the stdlib graph:
      every direct user-to-stdlib edge is already present in this set.
      Excluding those user-local names makes equivalent stdlib queries
      share one session entry instead of fragmenting the cache by each
      compilation's generated function names. *)
   |> F.filter (fun name->FunctionIdMap.containsKey name stdlibCallGraph) in
  if disposed then let reachable=DeadCodeElimination.findReachable stdlibCallGraph directCalls in List.filter (fun (func:LIR.functionDef)->F.mem func.LIR.id reachable) stdlibFunctions else
  let context=contextEntries reachableStdlibFunctionsByContext contextIdentity F.equal in
  match find context directCalls with
  | Some functions->increment stdlibReachabilityHitCount;functions
  | None->
   let roots=contextEntries reachableStdlibNamesByRootAndContext contextIdentity (=) in
   let reachable=F.fold (fun root reachable->let fromRoot=match find roots root with Some names->names|None->let names=DeadCodeElimination.findReachable stdlibCallGraph (F.singleton root) in store roots root names;names in F.union reachable fromRoot) directCalls F.empty in
   let inventory=match find stdlibFunctionInventoryByContext contextIdentity with
    | Some inventory->inventory
    | None->let inventory=cache (=) in List.iteri (fun index (func:LIR.functionDef)->store inventory func.LIR.id (index,func)) stdlibFunctions;store stdlibFunctionInventoryByContext contextIdentity inventory;inventory in
   let functions=F.elements reachable |> List.filter_map (find inventory) |> List.stable_sort (fun (a,_) (b,_)->compare a b) |> List.map snd in
   store context directCalls functions;increment stdlibReachabilityMissCount;functions
 method projectMirRegistries (contextIdentity:Obj.t) baseRegistries localVariantLookup localRecordFields=
  let projectLocalOverlay ()=C.projectMirRegistryOverlay baseRegistries localVariantLookup localRecordFields in
  if disposed then projectLocalOverlay () else match find mirRegistriesByContext contextIdentity with
  | Some registries->increment mirRegistryProjectionHitCount;registries
  | None->let registries=projectLocalOverlay () in store mirRegistriesByContext contextIdentity registries;increment mirRegistryProjectionMissCount;registries
 method arm64MetadataGroup (contextIdentity:Obj.t) functions (summarize:unit->ARM64CodeGenTypes.arm64ProgramMetadata)=
  if disposed then summarize () else
  let entries=contextEntries arm64MetadataGroupsByContext contextIdentity C.arm64MetadataGroupKeyComparer.C.equals in
  let key: C.arm64MetadataGroupKey={C.functions} in
  match find entries key with Some metadata->increment arm64MetadataGroupHitCount;metadata
  | None->let metadata=summarize () in store entries key metadata;increment arm64MetadataGroupMissCount;metadata
 method codegenFunctionGroup (contextIdentity:Obj.t) target (options:ARM64CodeGenTypes.codeGenOptions) functions (generate:unit->(Backend_Arm64_CodeGen.generatedChunk list,string) result)=
  if disposed || options.ARM64CodeGenTypes.enableCoverage then generate () else
  let entries=contextEntries arm64FunctionGroupsByContext contextIdentity C.arm64FunctionGroupKeyComparer.C.equals in
  let key:C.arm64FunctionGroupKey={C.functions;target;options} in
  match find entries key with Some result->increment arm64FunctionGroupHitCount;result
  | None->let result=generate () in store entries key result;increment arm64FunctionGroupMissCount;result
 method arm64ReleasePlanSummary (includeStaticRootDependencies:bool) (releasePlanCacheKey:string) (releasePlan:MemoryModel.rcReleasePlan) (generate:unit->LIR.arm64ReleasePlanSummary)=
  if disposed then generate () else
  let key=includeStaticRootDependencies,releasePlanCacheKey in
  match find arm64ReleasePlanSummaries key with
  | Some entries->(match List.find_opt (fun (existingPlan,_)->existingPlan == releasePlan || existingPlan=releasePlan) entries with
   | Some (_,summary)->increment arm64ReleasePlanSummaryHitCount;summary
   | None->let summary=generate () in store arm64ReleasePlanSummaries key ((releasePlan,summary)::entries);increment arm64ReleasePlanSummaryMissCount;summary)
  | None->let summary=generate () in store arm64ReleasePlanSummaries key [releasePlan,summary];increment arm64ReleasePlanSummaryMissCount;summary
 method codegenFunction (contextIdentity:Obj.t) (target:ARM64.targetConfig) (options:ARM64CodeGenTypes.codeGenOptions) (func:LIR.functionDef) (generate:unit->(Symbolic.instr list,string) result)=
  if disposed || options.ARM64CodeGenTypes.enableCoverage then generate () else
  let contextIdentity=
   if func.LIR.name="_start" then arm64StartContextIdentity
   else if func.LIR.name="__dark_compiler_program_entry" then contextIdentity
   else if Option.fold ~none:false ~some:(fun facts->MemoryPlanning.SemanticTypeSet.is_empty facts.LIR.rawSlotInitTypes || Option.is_some facts.LIR.arm64RawSlotInitRetainTargets) func.LIR.codegenFacts then arm64RegistryIndependentFunctionContextIdentity
   else contextIdentity in
  let structuralEntries=contextEntries arm64FunctionsByContext contextIdentity (fun (left,target,options) (right,otherTarget,otherOptions)->target=otherTarget && options=otherOptions && C.lirFunctionEquals left right) in
  let references=contextEntries arm64FunctionsByReferenceAndContext contextIdentity (==) in
  let key=func,target,options in
  let targetOptions=target,options in
  let referenceResult=Option.bind (find references func) (fun entries->find entries targetOptions) in
  let hit result=increment arm64CodegenHitCount;if func.LIR.name="_start" then increment arm64StartCodegenHitCount;result in
  match referenceResult with
  | Some result->hit result
  | None->match find structuralEntries key with
   | Some result->hit result
   | None->
    let timer=if collectCodegenMetrics then Some (HostClock.ticks ()) else None in
    let result=generate () in
    (match timer with
    | Some timer->
     let elapsed=Int64.sub (HostClock.ticks ()) timer in
     let lirInstructionCount=LIR.LabelMap.fold (fun _ (block:LIR.basicBlock) count->addCount count (List.length block.LIR.instrs+1)) func.LIR.cfg.LIR.blocks 0 in
     let symbolicInstructionCount=match result with Ok instructions->List.length instructions|Error _->0 in
     let metric={CompilerOptions.functionName=func.LIR.name;elapsed=HostTimeSpan.fromSeconds (Int64.to_float elapsed/.1000000000.);lirInstructionCount;symbolicInstructionCount} in
     arm64CodegenMetrics:=metric:: !arm64CodegenMetrics
    | None->());
    store structuralEntries key result;
    let referenceEntries=contextEntries references func (=) in
    store referenceEntries targetOptions result;
    increment arm64CodegenMissCount;result
 method arm64Helpers (contextIdentity:Obj.t) target (options:ARM64CodeGenTypes.codeGenOptions) (helperKey:Backend_Arm64_CodeGen.helperCacheKey) (generate:unit->Symbolic.instr list)=
  if disposed || options.ARM64CodeGenTypes.enableCoverage then generate () else
  let contextIdentity=if helperKey.Backend_Arm64_CodeGen.recursiveReleaseTypes=[] && helperKey.Backend_Arm64_CodeGen.closureCaptureTypes=[] then arm64RegistryIndependentHelperContextIdentity else contextIdentity in
  let entries=contextEntries arm64HelpersByContext contextIdentity C.arm64HelperCacheKeyComparer.C.equals in
  let key:C.arm64HelperCacheKey={C.target;options;helper=helperKey} in
  match find entries key with Some instructions->increment arm64HelperHitCount;instructions
  | None->let instructions=generate () in store entries key instructions;increment arm64HelperMissCount;instructions
 method prepareArm64EmissionChunk (instructions:Symbolic.instr list) (prepare:unit->ARM64_Encoding.preparedChunk)=
  if disposed then prepare () else match find arm64EmissionChunks instructions with Some prepared->prepared|None->let prepared=prepare () in store arm64EmissionChunks instructions prepared;prepared
 method prepareArm64EmissionChunkGroup (instructionParts:Symbolic.instr list list) (prepare:unit->ARM64_Encoding.preparedChunk)=
  if disposed then prepare () else match find arm64EmissionChunkGroups instructionParts with Some prepared->prepared|None->let prepared=prepare () in store arm64EmissionChunkGroups instructionParts prepared;prepared
 method cachedArm64FunctionCount=if disposed then 0 else nestedCount arm64FunctionsByContext
 method cachedArm64HelperCount=if disposed then 0 else nestedCount arm64HelpersByContext
 method cachedAnfDependencyCount=if disposed then 0 else nestedCount anfDependenciesByContext
 method cachedCompiledDependencyCount=if disposed then 0 else nestedCount compiledDependenciesByIdentity
 method cachedMirOptimizationCount=if disposed then 0 else count optimizedMirFunctions
 method cachedAllocatedLirFunctionCount=if disposed then 0 else count allocatedLirFunctions
 method cachedStdlibReachabilityCount=if disposed then 0 else nestedCount reachableStdlibFunctionsByContext
 method cachedMirRegistryProjectionCount=if disposed then 0 else count mirRegistriesByContext
 method cachedArm64MetadataGroupCount=if disposed then 0 else nestedCount arm64MetadataGroupsByContext
 method cachedArm64FunctionGroupCount=if disposed then 0 else nestedCount arm64FunctionGroupsByContext
 method cachedArm64EmissionChunkCount=if disposed then 0 else count arm64EmissionChunks+count arm64EmissionChunkGroups
 method cachedArm64ReleasePlanSummaryCount=if disposed then 0 else List.fold_left (fun count (_,entries)->count+List.length entries) 0 arm64ReleasePlanSummaries.entries
 method cachedJsonPlanCount=jsonPlanning#count
 method jsonPlanHitCount=jsonPlanning#hitCount
 method jsonPlanMissCount=jsonPlanning#missCount
 method anfDependencyHitCount= !anfDependencyHitCount
 method anfDependencyMissCount= !anfDependencyMissCount
 method compiledDependencyHitCount= !compiledDependencyHitCount
 method compiledDependencyMissCount= !compiledDependencyMissCount
 method mirOptimizationHitCount= !mirOptimizationHitCount
 method mirOptimizationMissCount= !mirOptimizationMissCount
 method allocatedLirFunctionHitCount= !allocatedLirFunctionHitCount
 method allocatedLirFunctionMissCount= !allocatedLirFunctionMissCount
 method stdlibReachabilityHitCount= !stdlibReachabilityHitCount
 method stdlibReachabilityMissCount= !stdlibReachabilityMissCount
 method mirRegistryProjectionHitCount= !mirRegistryProjectionHitCount
 method mirRegistryProjectionMissCount= !mirRegistryProjectionMissCount
 method arm64CodegenHitCount= !arm64CodegenHitCount
 method arm64CodegenMissCount= !arm64CodegenMissCount
 method arm64StartCodegenHitCount= !arm64StartCodegenHitCount
 method arm64MetadataGroupHitCount= !arm64MetadataGroupHitCount
 method arm64MetadataGroupMissCount= !arm64MetadataGroupMissCount
 method arm64FunctionGroupHitCount= !arm64FunctionGroupHitCount
 method arm64FunctionGroupMissCount= !arm64FunctionGroupMissCount
 method arm64HelperHitCount= !arm64HelperHitCount
 method arm64HelperMissCount= !arm64HelperMissCount
 method arm64ReleasePlanSummaryHitCount= !arm64ReleasePlanSummaryHitCount
 method arm64ReleasePlanSummaryMissCount= !arm64ReleasePlanSummaryMissCount
 method arm64CodegenMetrics=List.rev !arm64CodegenMetrics
 method arm64LirOpMetrics=List.map (fun ((functionName,opcode,detail),(occurrences,symbolicInstructionCount,ticks))->
  {CompilerOptions.functionName;opcode;detail;occurrences;symbolicInstructionCount;elapsed=HostTimeSpan.fromSeconds (Int64.to_float ticks/.1000000000.)}) arm64LirOpMetrics.entries
 method dispose=
  jsonPlanning#dispose;
  clear anfDependenciesByContext;
  clear compiledDependenciesByIdentity;
  clear optimizedMirFunctions;
  clear allocatedLirFunctions;
  clear callAwareLirFunctions;
  clear refinedLirFunctions;
  clear reachableStdlibFunctionsByContext;
  clear stdlibFunctionInventoryByContext;
  clear reachableStdlibNamesByRootAndContext;
  clear mirRegistriesByContext;
  clear arm64MetadataGroupsByContext;
  clear arm64FunctionGroupsByContext;
  clear arm64FunctionsByContext;
  clear arm64HelpersByContext;
  clear arm64FunctionsByReferenceAndContext;
  clear arm64EmissionChunks;
  clear arm64EmissionChunkGroups;
  clear arm64ReleasePlanSummaries;
  arm64CodegenMetrics:=[];
  clear arm64LirOpMetrics;
  disposed<-true
end
