(* CodeGen.fs - Assemble planned function and runtime-helper instruction chunks. *)
[@@@warning "-4-30"]
open ARM64CodeGenTypes
open HeapAllocation
open ARM64ListReferenceCounts
open ARM64ClosureReferenceCounts
open ARM64ReleaseSelection
open ARM64DictReferenceCounts
open GenericReferenceCounts
open ProcessLifecycle
open RunProcess
open ExecuteProcess
open ARM64Functions
open Peephole
open ReleasePlanSummary
(*
   Convert LIR program to ARM64 instructions with options
   Caller-owned conversion cache. Program-wide helper and layout generation is
   intentionally outside this hook and is performed for every executable.
*)
type functionCodegenCache = LIR.functionDef -> (unit -> (Symbolic.instr list,string) result) -> (Symbolic.instr list,string) result
(*
   Caller-owned cache for summaries of reusable function groups. Group
   summaries form a monoid, so an executable can merge cached dependency and
   stdlib facts with the small fresh program fragment.
*)
type metadataGroup = {contextIdentity:Obj.t;functions:LIR.functionDef list}
(*
   Must uniquely identify this exact ordered function sequence when reusable.
*)
type functionGroup = {contextIdentity:Obj.t;reusableAcrossCompilations:bool;functions:LIR.functionDef list}
type metadataGroupCache = Obj.t -> LIR.functionDef list -> (unit -> arm64ProgramMetadata) -> arm64ProgramMetadata
type helperCacheKey = {
 closurePayloadSizesFromParams:(string*int) list;closurePayloadSizesFromAllocs:(AST.functionId*int) list;
 closureCaptureTypes:(string*AST.semanticType list) list;recursiveReleaseTypes:AST.semanticType list;
 cliArgvHelperLabels:string list;needsCliExecuteHelper:bool;needsCliRunProcessHelper:bool;
 needsCliProcessLifecycleHelpers:bool;needsRuntimeErrorHelper:bool;listDecHelperLabels:string list;
 plannedListDecHelpers:(string*int) list;plannedGenericDecHelperLabels:string list;plannedDictDecHelperLabels:string list;
 dictDecHelperLabels:string list;needsListRcIncHelper:bool;needsDictRcIncHelper:bool;
 needsClosureRcIncHelper:bool;needsClosureRcDecHelper:bool;needsStreamRcDecHelper:bool
}
type helperCodegenCache = helperCacheKey -> (unit -> Symbolic.instr list) -> Symbolic.instr list
type generatedChunk = {instructionParts:Symbolic.instr list list;reusableAcrossCompilations:bool}
type functionGroupCodegenCache = Obj.t -> LIR.functionDef list -> (unit -> (generatedChunk list,string) result) -> (generatedChunk list,string) result
type generatedProgram=GeneratedProgram of generatedChunk list
let generatedProgramChunks (GeneratedProgram chunks)=chunks
let generatedProgramInstructions (GeneratedProgram chunks)=List.concat_map (fun chunk -> List.concat chunk.instructionParts) chunks
(*
   Ensure _start is first (entry point)
   No _start, keep original order
   Retain source function order: map entries intentionally preserve the
   original last-writer-wins behavior for compiler-generated labels.
   Functions with no contribution are the identity element. Leaving
   them out makes cache keys reflect metadata semantics rather than
   incidental reachability, so overlapping stdlib subsets reuse the
   same immutable summary.
   StackSize and UsedCalleeSaved are set per-function in convertFunction.
   Recursive nominal dec helpers can call nested list and dict dec helpers.
   Their source plans are not present among the direct LIR instruction plans.
   Function chunks are closed by their epilogue (or _start exit), so
   peephole patterns cannot span into the next function's entry label.
   The compiler's fixed _start trampoline is reusable too: the changing
   user expression lives behind its __dark_compiler_program_entry call.
   Cache each finalized chunk and never rescan it per executable.
   Compilation assembly already knows the exact stdlib/program/dependency
   boundaries. Consume those ordered groups directly instead of rescanning
   every function to rediscover them during codegen.
   Retain the cached function instruction lists as separate
   preparation parts. Emission can compose their already-
   encoded templates once per reusable group without
   re-encoding every fixed instruction in each group shape.
   The helper cache key below completely describes helper planning
   as well as the emitted helper instructions. Keep the dependency
   closure inside the cache miss path so repeated executables do
   not rediscover an already-generated plan.
*)
let releaseKindDescription kind=StructuralValue.Union ((match kind with MemoryModel.GenericHeap->"GenericHeap"|MemoryModel.StreamHeap->"StreamHeap"|MemoryModel.TaggedList->"TaggedList"|MemoryModel.DictHeap->"DictHeap"|MemoryModel.ClosureHeap->"ClosureHeap"),[])
let releaseOperationDescription=function
 | MemoryModel.FixedSizeRoot (size,kind)->StructuralValue.Union ("FixedSizeRoot",[StructuralValue.Scalar (string_of_int size);releaseKindDescription kind])
 | MemoryModel.DynamicStringBuffer->StructuralValue.Union ("DynamicStringBuffer",[])
 | MemoryModel.DynamicBlobBuffer->StructuralValue.Union ("DynamicBlobBuffer",[])
 | MemoryModel.DynamicIntBuffer->StructuralValue.Union ("DynamicIntBuffer",[])
let rec releasePlanDescription=function
 | MemoryModel.NoReleasePlan->StructuralValue.Union ("NoReleasePlan",[])
 | MemoryModel.DynamicBufferRelease operation->StructuralValue.Union ("DynamicBufferRelease",[releaseOperationDescription operation])
 | MemoryModel.RecursiveRelease typ->StructuralValue.Union ("RecursiveRelease",[StructuralFormat.semanticValue typ])
 | MemoryModel.RootRelease (size,kind,payload)->StructuralValue.Union ("RootRelease",[StructuralValue.Scalar (string_of_int size);releaseKindDescription kind;releasePayloadDescription payload])
and releasePayloadDescription=function
 | MemoryModel.NoPayloadRelease->StructuralValue.Union ("NoPayloadRelease",[])
 | MemoryModel.FixedBlockPayloadRelease (size,fields)->StructuralValue.Union ("FixedBlockPayloadRelease",[StructuralValue.Scalar (string_of_int size);StructuralValue.Sequence (List.map releaseFieldDescription fields)])
 | MemoryModel.BoxedSumPayloadRelease (size,fields,variants)->StructuralValue.Union ("BoxedSumPayloadRelease",[StructuralValue.Scalar (string_of_int size);StructuralValue.Sequence (List.map releaseFieldDescription fields);StructuralValue.Sequence (List.map (fun (variant:MemoryModel.rcBoxedSumVariantRelease) -> StructuralValue.Record ["Tag",StructuralValue.Scalar (string_of_int variant.MemoryModel.tag);"FieldReleases",StructuralValue.Sequence (List.map releaseFieldDescription variant.MemoryModel.fieldReleases)]) variants)])
 | MemoryModel.TaggedListPayloadRelease element->StructuralValue.Union ("TaggedListPayloadRelease",[releasePlanDescription element])
 | MemoryModel.DictPayloadRelease (key,value)->StructuralValue.Union ("DictPayloadRelease",[releasePlanDescription key;releasePlanDescription value])
 | MemoryModel.ClosurePayloadRelease fields->StructuralValue.Union ("ClosurePayloadRelease",[StructuralValue.Sequence (List.map releaseFieldDescription fields)])
and releaseFieldDescription (MemoryModel.FieldRelease (offset,plan))=StructuralValue.Union ("FieldRelease",[StructuralValue.Scalar (string_of_int offset);releasePlanDescription plan])
let generatePreparedARM64WithOptionsAndCache target options preparedSumShapeRegistry functionCache functionGroupCache (functionGroups:functionGroup list) metadataGroupCache helperCache (metadataGroups:metadataGroup list) lirOpExpansionRecorder phaseRecorder (LIR.Program (functions,variantRegistry,recordRegistry))=
 let startPhase ()=Option.map (fun _ -> (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)) phaseRecorder in
 let recordPhase name timer=match phaseRecorder,timer with Some record,Some started->record name ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. started)|_->() in
 let metadataTimer=startPhase () in
 let registrySetupTimer=startPhase () in
 let heapOverflowTrapBody=preparedHeapOverflowTrapBody target in
 let sumShapeRegistry=match preparedSumShapeRegistry with Some registry->registry|None->rcSumShapeRegistryFromVariantRegistry variantRegistry in
 recordPhase "ARM64 Metadata Registry Setup" registrySetupTimer;
 let functionInventoryTimer=startPhase () in
 let sortedFunctions=match List.partition (fun (f:LIR.functionDef) -> f.LIR.name="_start") functions with startFunc::_,otherFuncs->startFunc::otherFuncs|[],_->functions in
 recordPhase "ARM64 Metadata Function Inventory" functionInventoryTimer;
 let unionLabelSets sets=List.fold_left StringOrder.Set.union StringOrder.Set.empty sets in
 let listDecHelperDictDependencyLabels helperLabel=if helperLabel=listRefCountDecDictListHelperLabel then StringOrder.Set.singleton dictRefCountDecListValueHelperLabel else StringOrder.Set.empty in
 let summarizeReleasePlan=summarizePrecomputedReleasePlan in
 let emptyRcHelperRequirements=precomputedEmptyRcHelperRequirements in
 let emptyProgramMetadata={facts={closurePayloadSizesFromParams=StringOrder.Map.empty;closurePayloadSizesFromAllocs=FunctionIdMap.empty;closureCaptureTypes=StringOrder.Map.empty;recursiveReleaseTypes=MemoryPlanning.SemanticTypeSet.empty;cliArgvHelperLabels=StringOrder.Set.empty;needsCliExecuteHelper=false;needsCliRunProcessHelper=false;needsCliProcessLifecycleHelpers=false;needsRuntimeErrorHelper=false};rcHelperRequirements=emptyRcHelperRequirements} in
 let hasRcHelperRequirements (requirements:rcHelperRequirements)=
  not (StringOrder.Set.is_empty requirements.LIR.listDecHelperLabels) || not (StringOrder.Map.is_empty requirements.LIR.plannedListDecHelpers) || not (StringOrder.Map.is_empty requirements.LIR.plannedGenericDecHelpers) || not (StringOrder.Map.is_empty requirements.LIR.plannedDictDecHelpers) || not (StringOrder.Set.is_empty requirements.LIR.dictDecHelperLabels) || requirements.LIR.needsListRcIncHelper || requirements.LIR.needsDictRcIncHelper || requirements.LIR.needsClosureRcIncHelper || requirements.LIR.needsClosureRcDecHelper || requirements.LIR.needsStreamRcDecHelper || not (LIR.ReleasePlanSummaryMap.is_empty requirements.LIR.releasePlanSummaries) in
 let contributesProgramMetadata (func:LIR.functionDef)=
  let facts=match func.LIR.codegenFacts with Some facts->facts|None->Crash.crash "ARM64 codegen invariant: missing validated function facts" in
  let contributesRcHelpers=match facts.LIR.arm64RcHelperRequirements with Some requirements->hasRcHelperRequirements requirements|None->Crash.crash "ARM64 codegen invariant: missing validated RC helper requirements" in
  Option.is_some facts.LIR.closurePayloadSizeFromParams || facts.LIR.closurePayloadSizesFromAllocs<>[] || not (MemoryPlanning.SemanticTypeSet.is_empty facts.LIR.recursiveReleaseTypes) || not (MemoryPlanning.SemanticTypeSet.is_empty facts.LIR.rawSlotInitTypes) || facts.LIR.needsCliArgvHelper || facts.LIR.needsCliExecuteHelper || facts.LIR.needsCliRunProcessHelper || facts.LIR.needsCliProcessLifecycleHelpers || facts.LIR.needsRuntimeErrorHelper || contributesRcHelpers in
 let releasePlanSummary=precomputedReleasePlanSummary in
 let addReleasePlanRequirements=addPrecomputedReleasePlanRequirements in
 let collectRawSlotInitRequirement (requirements:rcHelperRequirements)=function
  | Some LIR.SlotInitListRootRetain->{requirements with LIR.needsListRcIncHelper=true}
  | Some LIR.SlotInitDictRootRetain->{requirements with LIR.needsDictRcIncHelper=true}
  | Some LIR.SlotInitClosureRootRetain->{requirements with LIR.needsClosureRcIncHelper=true}
  | Some LIR.SlotInitDynamicBufferRetain|Some (LIR.SlotInitGenericRootRetain _)|None->requirements in
 let collectFunctionMetadata metadata ((func:LIR.functionDef),(facts:LIR.functionCodegenFacts))=
  let withClosureParams=match facts.LIR.closurePayloadSizeFromParams with Some payloadSize->{metadata with facts={metadata.facts with closurePayloadSizesFromParams=StringOrder.Map.add func.LIR.name payloadSize metadata.facts.closurePayloadSizesFromParams}}|None->metadata in
  let withClosureCaptures=match facts.LIR.closureCaptureTypes with Some captures->{withClosureParams with facts={withClosureParams.facts with closureCaptureTypes=StringOrder.Map.add func.LIR.name captures withClosureParams.facts.closureCaptureTypes}}|None->withClosureParams in
  let withAllocSizes=List.fold_left (fun metadata (funcName,payloadSize) -> {metadata with facts={metadata.facts with closurePayloadSizesFromAllocs=FunctionIdMap.add funcName payloadSize metadata.facts.closurePayloadSizesFromAllocs}}) withClosureCaptures facts.LIR.closurePayloadSizesFromAllocs in
  let requirements=match facts.LIR.arm64RcHelperRequirements with Some planned->mergePrecomputedRcHelperRequirements withAllocSizes.rcHelperRequirements planned|None->Crash.crash "ARM64 codegen invariant: missing validated RC helper requirements" in
  let requirements=match facts.LIR.arm64RawSlotInitRetainTargets with
   | Some targets->LIR.SemanticTypeMap.to_seq targets |> Seq.map snd |> Seq.fold_left collectRawSlotInitRequirement requirements
   | None->MemoryPlanning.SemanticTypeSet.fold (fun valueType current -> collectRawSlotInitRequirement current (slotInitRootRetainTarget recordRegistry sumShapeRegistry valueType)) facts.LIR.rawSlotInitTypes requirements in
  {facts={withAllocSizes.facts with recursiveReleaseTypes=MemoryPlanning.SemanticTypeSet.union withAllocSizes.facts.recursiveReleaseTypes facts.LIR.recursiveReleaseTypes;cliArgvHelperLabels=(if facts.LIR.needsCliArgvHelper then StringOrder.Set.add ("__dark_cli_argv_"^func.LIR.name) withAllocSizes.facts.cliArgvHelperLabels else withAllocSizes.facts.cliArgvHelperLabels);needsCliExecuteHelper=withAllocSizes.facts.needsCliExecuteHelper || facts.LIR.needsCliExecuteHelper;needsCliRunProcessHelper=withAllocSizes.facts.needsCliRunProcessHelper || facts.LIR.needsCliRunProcessHelper;needsCliProcessLifecycleHelpers=withAllocSizes.facts.needsCliProcessLifecycleHelpers || facts.LIR.needsCliProcessLifecycleHelpers;needsRuntimeErrorHelper=withAllocSizes.facts.needsRuntimeErrorHelper || facts.LIR.needsRuntimeErrorHelper};rcHelperRequirements=requirements} in
 let finishMetadata instructionMetadata=
  let rcHelperRequirements=StringOrder.Map.fold (fun _ captureTypes requirements -> List.fold_left (fun requirements captureType ->
   match tryRcReleasePlanOfType recordRegistry sumShapeRegistry captureType with
   | None->requirements
   | Some releasePlan->let summary,requirements=releasePlanSummary true (LIR.StructuralReleasePlan (Some releasePlan)) releasePlan requirements in
    let withPlanRequirements=addReleasePlanRequirements summary requirements in
    {withPlanRequirements with LIR.listDecHelperLabels=StringOrder.Set.union withPlanRequirements.LIR.listDecHelperLabels summary.LIR.listDecHelperLabels;dictDecHelperLabels=StringOrder.Set.union withPlanRequirements.LIR.dictDecHelperLabels summary.LIR.dictDecHelperLabels}) requirements captureTypes) instructionMetadata.facts.closureCaptureTypes instructionMetadata.rcHelperRequirements in
  {instructionMetadata with rcHelperRequirements} in
 let summarizeGroup group=
  let summaryTimer=startPhase () in
  let withFacts=List.map (fun (func:LIR.functionDef) -> let facts=match func.LIR.codegenFacts with Some facts->facts|None->Crash.crash "ARM64 codegen invariant: missing validated function facts" in func,facts) group in
  let summary=List.fold_left collectFunctionMetadata emptyProgramMetadata withFacts |> finishMetadata in
  recordPhase "ARM64 Metadata Group Summarization" summaryTimer;summary in
 let mergeMaps left right=StringOrder.Map.fold StringOrder.Map.add right left in
 let isEmptyMetadata metadata=StringOrder.Map.is_empty metadata.facts.closurePayloadSizesFromParams && FunctionIdMap.isEmpty metadata.facts.closurePayloadSizesFromAllocs && StringOrder.Map.is_empty metadata.facts.closureCaptureTypes && MemoryPlanning.SemanticTypeSet.is_empty metadata.facts.recursiveReleaseTypes && StringOrder.Set.is_empty metadata.facts.cliArgvHelperLabels && not metadata.facts.needsCliExecuteHelper && not metadata.facts.needsCliRunProcessHelper && not metadata.facts.needsCliProcessLifecycleHelpers && not metadata.facts.needsRuntimeErrorHelper && not (hasRcHelperRequirements metadata.rcHelperRequirements) in
 let mergeMetadata left right=if isEmptyMetadata left then right else if isEmptyMetadata right then left else
  {facts={closurePayloadSizesFromParams=mergeMaps left.facts.closurePayloadSizesFromParams right.facts.closurePayloadSizesFromParams;closurePayloadSizesFromAllocs=FunctionIdMap.merge left.facts.closurePayloadSizesFromAllocs right.facts.closurePayloadSizesFromAllocs;closureCaptureTypes=mergeMaps left.facts.closureCaptureTypes right.facts.closureCaptureTypes;recursiveReleaseTypes=MemoryPlanning.SemanticTypeSet.union left.facts.recursiveReleaseTypes right.facts.recursiveReleaseTypes;cliArgvHelperLabels=StringOrder.Set.union left.facts.cliArgvHelperLabels right.facts.cliArgvHelperLabels;needsCliExecuteHelper=left.facts.needsCliExecuteHelper || right.facts.needsCliExecuteHelper;needsCliRunProcessHelper=left.facts.needsCliRunProcessHelper || right.facts.needsCliRunProcessHelper;needsCliProcessLifecycleHelpers=left.facts.needsCliProcessLifecycleHelpers || right.facts.needsCliProcessLifecycleHelpers;needsRuntimeErrorHelper=left.facts.needsRuntimeErrorHelper || right.facts.needsRuntimeErrorHelper};rcHelperRequirements=mergePrecomputedRcHelperRequirements left.rcHelperRequirements right.rcHelperRequirements} in
 let groupCompositionTimer=startPhase () in
 let groups=match metadataGroups with []->[{contextIdentity=Obj.repr (ref ());functions}]|groups->groups in
 let programMetadata=List.fold_left (fun metadata (group:metadataGroup) ->
  let contributingFunctions=List.filter contributesProgramMetadata group.functions in
  let groupMetadata=match metadataGroupCache with Some cache->cache group.contextIdentity contributingFunctions (fun () -> summarizeGroup contributingFunctions)|None->summarizeGroup contributingFunctions in
  mergeMetadata metadata groupMetadata) emptyProgramMetadata groups in
 recordPhase "ARM64 Metadata Group Composition" groupCompositionTimer;
 let rcHelperRequirements=programMetadata.rcHelperRequirements in
 let needsCliExecuteHelper=programMetadata.facts.needsCliExecuteHelper in
 let needsCliRunProcessHelper=programMetadata.facts.needsCliRunProcessHelper in
 let needsCliProcessLifecycleHelpers=programMetadata.facts.needsCliProcessLifecycleHelpers in
 let closurePayloadSizes=
  let functionNames=functions |> List.map (fun (func:LIR.functionDef) -> func.LIR.id,func.LIR.name) |> FunctionIdMap.ofList in
  FunctionIdMap.fold (fun acc funcId payloadSize -> match FunctionIdMap.tryFind funcId functionNames with Some funcName->StringOrder.Map.add funcName payloadSize acc|None->Crash.crash (Printf.sprintf "ARM64 metadata: missing closure target name for identity %Lu" (AST.functionIdValue funcId))) programMetadata.facts.closurePayloadSizesFromParams programMetadata.facts.closurePayloadSizesFromAllocs in
 let plannedListDecHelpers=rcHelperRequirements.LIR.plannedListDecHelpers in
 let plannedGenericDecHelpers=rcHelperRequirements.LIR.plannedGenericDecHelpers in
 let plannedDictDecHelpers=rcHelperRequirements.LIR.plannedDictDecHelpers in
 let helperIds=List.fold_left (fun ids (func:LIR.functionDef) ->
  let localIds=Option.map (fun facts -> facts.LIR.arm64GenericHelperIds) func.LIR.codegenFacts |> Option.value ~default:StringOrder.Map.empty in
  StringOrder.Map.fold (fun label id ids -> match StringOrder.Map.find_opt label ids with Some existing when existing<>id->Crash.crash ("ARM64 generic helper '"^label^"' has conflicting identities")|_->StringOrder.Map.add label id ids) localIds ids) StringOrder.Map.empty functions in
 let functionNames=StringOrder.Map.fold (fun name id names -> FunctionIdMap.add id name names) helperIds (FunctionIdMap.ofList (List.map (fun (func:LIR.functionDef) -> func.LIR.id,func.LIR.name) functions)) in
 let ctx={target;options;sumShapeRegistry;recordRegistry;rawSlotInitRetainTargets=None;closurePayloadSizes;closureCaptureTypes=programMetadata.facts.closureCaptureTypes;functionNames;functionName="";instructionSite="";stackSize=0;usedCalleeSaved=[];usedCalleeSavedF=[];heapOverflowLabel="";recordLirOpExpansion=lirOpExpansionRecorder} in
 let recursiveReleaseSummaries=MemoryPlanning.SemanticTypeSet.elements programMetadata.facts.recursiveReleaseTypes |> List.map (fun sourceType -> MemoryPlanning.rcReleasePlanOfTypeWithSums recordRegistry sumShapeRegistry sourceType |> summarizePrecomputedReleasePlan true) in
 let plannedListDecHelpers=List.fold_left (fun helpers (summary:LIR.arm64ReleasePlanSummary) -> StringOrder.Map.fold StringOrder.Map.add summary.LIR.plannedListDecHelpers helpers) plannedListDecHelpers recursiveReleaseSummaries in
 let plannedDictDecHelpers=List.fold_left (fun helpers (summary:LIR.arm64ReleasePlanSummary) -> StringOrder.Map.fold StringOrder.Map.add summary.LIR.plannedDictDecHelpers helpers) plannedDictDecHelpers recursiveReleaseSummaries in
 recordPhase "ARM64 Codegen Metadata" metadataTimer;
 let convertCached (func:LIR.functionDef)=
  let generate ()=convertFunction heapOverflowTrapBody ctx func |> Result.map peepholeOptimize in
  let reusableAcrossCompilations=Option.is_some functionCache in
  let converted=match functionCache with Some cache when reusableAcrossCompilations->cache func generate|_->generate () in
  converted |> Result.map (fun instructions -> {instructionParts=[instructions];reusableAcrossCompilations}) in
 let functionTimer=startPhase () in
 let functionRuns=match functionGroups with
  | []->[None,sortedFunctions]
  | groups->
   let groupedFunctions=List.concat_map (fun (group:functionGroup) -> group.functions) groups in
   let orderMatches=List.length groupedFunctions=List.length sortedFunctions && List.for_all2 (==) groupedFunctions sortedFunctions in
   if not orderMatches then Crash.crash "ARM64 codegen invariant: function groups do not match program order";
   List.map (fun (group:functionGroup) -> Some group,group.functions) groups in
 let convertRun ((group:functionGroup option),runFunctions)=
  let generate ()=ResultList.mapResults convertCached runFunctions |> Result.map (fun chunks -> match group,functionGroupCache,chunks with
   | Some group,Some _,_::_ when group.reusableAcrossCompilations->[{instructionParts=List.concat_map (fun chunk -> chunk.instructionParts) chunks;reusableAcrossCompilations=true}]
   | _->chunks) in
  match group,functionGroupCache with Some group,Some cache when group.reusableAcrossCompilations->cache group.contextIdentity runFunctions generate|_->generate () in
 let convertedFunctionChunks=ResultList.mapResults convertRun functionRuns |> Result.map List.concat in
 recordPhase "ARM64 Codegen Functions" functionTimer;
 convertedFunctionChunks |> Result.map (fun functionChunks ->
  let functionChunks=if needsCliProcessLifecycleHelpers && ARM64.targetOS target=Platform.Linux then List.map (fun chunk ->
   let containsStartEpilogue=List.exists (List.mem (Symbolic.Label "_epilogue__start")) chunk.instructionParts in
   if containsStartEpilogue then {instructionParts=List.map (List.concat_map (fun instruction -> if instruction=Symbolic.Label "_epilogue__start" then [instruction;Symbolic.BL "__dark_cli_cleanup_processes"] else [instruction])) chunk.instructionParts;reusableAcrossCompilations=false} else chunk) functionChunks else functionChunks in
  let helperTimer=startPhase () in
  let generateHelperInstructions ()=
   let helperMetadataTimer=startPhase () in
   let listDecHelperDependencyLabels helperLabel=match StringOrder.Map.find_opt helperLabel plannedListDecHelpers with Some (_,releasePlan)->StringOrder.Set.remove helperLabel (summarizeReleasePlan false releasePlan).LIR.listDecHelperLabels|None->StringOrder.Set.empty in
   let rec expandListDecHelperDependencies selectedLabels=function
    | []->selectedLabels
    | helperLabel::rest->let dependencies=listDecHelperDependencyLabels helperLabel |> StringOrder.Set.filter (fun dependencyLabel -> not (StringOrder.Set.mem dependencyLabel selectedLabels)) in expandListDecHelperDependencies (StringOrder.Set.union selectedLabels dependencies) (rest@StringOrder.Set.elements dependencies) in
   let neededListRcDecHelperLabels=let calledLabels=rcHelperRequirements.LIR.listDecHelperLabels in if StringOrder.Set.is_empty calledLabels then StringOrder.Set.empty else let rootLabels=StringOrder.Set.add listRefCountDecHelperLabel calledLabels in expandListDecHelperDependencies rootLabels (StringOrder.Set.elements rootLabels) in
   let rec rcReleasePlanContains predicate releasePlan=if predicate releasePlan then true else match releasePlan with
    | MemoryModel.RootRelease (_,_,MemoryModel.FixedBlockPayloadRelease (_,fields))|MemoryModel.RootRelease (_,_,MemoryModel.BoxedSumPayloadRelease (_,fields,_))|MemoryModel.RootRelease (_,_,MemoryModel.ClosurePayloadRelease fields)->List.exists (fun (MemoryModel.FieldRelease (_,plan)) -> rcReleasePlanContains predicate plan) fields
    | MemoryModel.RootRelease (_,_,MemoryModel.DictPayloadRelease (keyRelease,valueRelease))->rcReleasePlanContains predicate keyRelease || rcReleasePlanContains predicate valueRelease
    | MemoryModel.RootRelease (_,_,MemoryModel.TaggedListPayloadRelease elementRelease)->rcReleasePlanContains predicate elementRelease
    | _->false in
   let selectedPlannedListHelpersNeed predicate selectedLabels=StringOrder.Map.bindings plannedListDecHelpers |> List.exists (fun (helperLabel,(_,plan)) -> StringOrder.Set.mem helperLabel selectedLabels && rcReleasePlanContains predicate plan) in
   let selectedStaticListHelpersNeed directSpecNeed selectedLabels=List.exists (fun (spec:listRefCountDecHelperSpec) -> StringOrder.Set.mem spec.label selectedLabels && directSpecNeed spec) listRefCountDecHelperSpecs in
   let dictDecHelperDependencyLabels helperLabel=match StringOrder.Map.find_opt helperLabel plannedDictDecHelpers with
    | Some (MemoryModel.RootRelease (_,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (_,valueRelease)))->(match valueRelease with
     | MemoryModel.RootRelease (_,MemoryModel.TaggedList,_)->StringOrder.Set.singleton dictRefCountDecListValueHelperLabel
     | MemoryModel.RootRelease (_,MemoryModel.DictHeap,_)->StringOrder.Set.singleton (dictDecHelperForReleasePlan valueRelease)
     | MemoryModel.RootRelease (_,MemoryModel.GenericHeap,_)->let lists=(summarizeReleasePlan false valueRelease).LIR.listDecHelperLabels in let dicts=(summarizeReleasePlan true valueRelease).LIR.dictDecHelperLabels in StringOrder.Set.union lists dicts
     | _->StringOrder.Set.empty)
    | Some other->Crash.crash ("ARM64 planned dict dependency labels require a DictHeap release plan, got "^StructuralFormat.format (releasePlanDescription other))
    | None->if helperLabel=dictRefCountDecListValueHelperLabel then StringOrder.Set.singleton listRefCountDecHelperLabel else if helperLabel=dictRefCountDecDictValueHelperLabel then StringOrder.Set.singleton dictRefCountDecHelperLabel else if helperLabel=dictRefCountDecDictListValueHelperLabel then StringOrder.Set.singleton dictRefCountDecListValueHelperLabel else if helperLabel=dictRefCountDecTupleStringListValueHelperLabel then StringOrder.Set.singleton listRefCountDecHelperLabel else if helperLabel=dictRefCountDecTupleStringListDictValueHelperLabel then StringOrder.Set.of_list [listRefCountDecHelperLabel;dictRefCountDecHelperLabel] else StringOrder.Set.empty in
   let neededDictRcDecHelperLabels=
    let listHelperDictLabels=StringOrder.Set.elements neededListRcDecHelperLabels |> List.map (fun helperLabel ->
     let staticLabels=listDecHelperDictDependencyLabels helperLabel in
     let plannedLabels=StringOrder.Map.find_opt helperLabel plannedListDecHelpers |> Option.map (fun (_,plan) -> (summarizeReleasePlan false plan).LIR.dictDecHelperLabels) |> Option.value ~default:StringOrder.Set.empty in StringOrder.Set.union staticLabels plannedLabels) |> unionLabelSets in
    let directLabels=StringOrder.Set.union listHelperDictLabels rcHelperRequirements.LIR.dictDecHelperLabels in
    let rec expandDependencies selectedLabels=function []->selectedLabels|helperLabel::rest->let dependencies=dictDecHelperDependencyLabels helperLabel |> StringOrder.Set.filter (fun dependency -> not (StringOrder.Set.mem dependency selectedLabels)) in expandDependencies (StringOrder.Set.union selectedLabels dependencies) (rest@StringOrder.Set.elements dependencies) in
    expandDependencies directLabels (StringOrder.Set.elements directLabels) in
   let listRcDecHelperLabelsFromDictHelpers=StringOrder.Set.elements neededDictRcDecHelperLabels |> List.map dictDecHelperDependencyLabels |> unionLabelSets |> StringOrder.Set.filter (fun label -> label=listRefCountDecHelperLabel) in
   let selectedListRcDecHelperLabels=StringOrder.Set.union neededListRcDecHelperLabels listRcDecHelperLabelsFromDictHelpers in
   let needsDictRcDecHelper=StringOrder.Set.mem dictRefCountDecHelperLabel neededDictRcDecHelperLabels in
   let needsDictRcDecListValueHelper=StringOrder.Set.mem dictRefCountDecListValueHelperLabel neededDictRcDecHelperLabels in
   let needsDictRcDecDictValueHelper=StringOrder.Set.mem dictRefCountDecDictValueHelperLabel neededDictRcDecHelperLabels in
   let needsDictRcDecDictListValueHelper=StringOrder.Set.mem dictRefCountDecDictListValueHelperLabel neededDictRcDecHelperLabels in
   let needsDictRcDecTupleStringListValueHelper=StringOrder.Set.mem dictRefCountDecTupleStringListValueHelperLabel neededDictRcDecHelperLabels in
   let needsDictRcDecTupleStringListDictValueHelper=StringOrder.Set.mem dictRefCountDecTupleStringListDictValueHelperLabel neededDictRcDecHelperLabels in
   let needsDictRcDecSumStringValueHelper=StringOrder.Set.mem dictRefCountDecSumStringValueHelperLabel neededDictRcDecHelperLabels in
   recordPhase "ARM64 Metadata Helper Planning" helperMetadataTimer;
   let helperSelectionTimer=startPhase () in
   let containsRoot kind=function MemoryModel.RootRelease (_,actual,_)->kind=actual|_->false in
   let selectedListHelpersNeedDictDecHelper=selectedStaticListHelpersNeed (fun spec -> spec.releaseLeafDictPayload) selectedListRcDecHelperLabels || selectedPlannedListHelpersNeed (containsRoot MemoryModel.DictHeap) selectedListRcDecHelperLabels in
   let selectedListHelpersNeedClosureDecHelper=selectedStaticListHelpersNeed (fun spec -> spec.releaseLeafClosurePayload) selectedListRcDecHelperLabels || selectedPlannedListHelpersNeed (containsRoot MemoryModel.ClosureHeap) selectedListRcDecHelperLabels in
   let plannedDictHelpersNeedClosureDecHelper=StringOrder.Map.exists (fun _ plan -> rcReleasePlanContains (containsRoot MemoryModel.ClosureHeap) plan) plannedDictDecHelpers in
   let selectedListHelpersNeedStreamDecHelper=selectedPlannedListHelpersNeed (containsRoot MemoryModel.StreamHeap) selectedListRcDecHelperLabels in
   let plannedDictHelpersNeedStreamDecHelper=StringOrder.Map.exists (fun _ plan -> rcReleasePlanContains (containsRoot MemoryModel.StreamHeap) plan) plannedDictDecHelpers in
   let needsClosureRcDecHelper=rcHelperRequirements.LIR.needsClosureRcDecHelper in
   let selectedClosureHelpersNeedStreamDecHelper=if needsClosureRcDecHelper || selectedListHelpersNeedClosureDecHelper || plannedDictHelpersNeedClosureDecHelper then StringOrder.Map.exists (fun _ captureTypes -> List.exists (fun captureType -> tryRcReleasePlanOfType ctx.recordRegistry ctx.sumShapeRegistry captureType |> Option.fold ~none:false ~some:(rcReleasePlanContains (containsRoot MemoryModel.StreamHeap))) captureTypes) ctx.closureCaptureTypes else false in
   let needsStreamRcDecHelper=rcHelperRequirements.LIR.needsStreamRcDecHelper || selectedListHelpersNeedStreamDecHelper || plannedDictHelpersNeedStreamDecHelper || selectedClosureHelpersNeedStreamDecHelper in
   let emitClosureRcDecHelper=needsClosureRcDecHelper || selectedListHelpersNeedClosureDecHelper || plannedDictHelpersNeedClosureDecHelper || needsStreamRcDecHelper in
   recordPhase "ARM64 Helper Selection" helperSelectionTimer;
   let listHelperTimer=startPhase () in
   let listRcHelpers=(if rcHelperRequirements.LIR.needsListRcIncHelper then generateListRefCountIncHelper () else []) @ generateNeededListRefCountDecHelpers ctx selectedListRcDecHelperLabels plannedListDecHelpers in
   recordPhase "ARM64 Helper List Generation" listHelperTimer;
   let genericHelperTimer=startPhase () in
   let genericRcHelpers=StringOrder.Map.bindings plannedGenericDecHelpers |> List.concat_map (fun (helperLabel,spec) ->
    let generate ()=Ok (generatePlannedGenericRefCountDecHelper helperLabel spec ctx) in
    let generated=match functionCache with Some cache->cache (plannedGenericRefCountDecHelperCacheKey (StringOrder.Map.find helperLabel helperIds) helperLabel) generate|None->generate () in
    match generated with Ok instructions->instructions|Error error->Crash.crash ("ARM64 cached generic release helper generation failed for "^helperLabel^": "^error)) in
   recordPhase "ARM64 Helper Generic Release Generation" genericHelperTimer;
   let dictHelperTimer=startPhase () in
   let dictRcHelpers=
    (if rcHelperRequirements.LIR.needsDictRcIncHelper then generateDictRefCountIncHelper () else [])
    @ (StringOrder.Map.bindings plannedDictDecHelpers |> List.concat_map (fun (helperLabel,releasePlan) -> generatePlannedDictRefCountDecHelper helperLabel releasePlan ctx))
    @ (if needsDictRcDecHelper || selectedListHelpersNeedDictDecHelper || not (StringOrder.Map.is_empty plannedDictDecHelpers) then generateDictRefCountDecHelper dictRefCountDecHelperLabel MemoryModel.NoReleasePlan false false None false false None false false false ctx else [])
    @ (if needsDictRcDecListValueHelper then generateDictRefCountDecHelper dictRefCountDecListValueHelperLabel MemoryModel.NoReleasePlan false true None false false None false false false ctx else [])
    @ (if needsDictRcDecDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDictValueHelperLabel MemoryModel.NoReleasePlan false false (Some dictRefCountDecHelperLabel) false false None false false false ctx else [])
    @ (if needsDictRcDecDictListValueHelper then generateDictRefCountDecHelper dictRefCountDecDictListValueHelperLabel MemoryModel.NoReleasePlan false false (Some dictRefCountDecListValueHelperLabel) false false None false false false ctx else [])
    @ (if needsDictRcDecTupleStringListValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListValueHelperLabel MemoryModel.NoReleasePlan false false None false false None true false false ctx else [])
    @ (if needsDictRcDecTupleStringListDictValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListDictValueHelperLabel MemoryModel.NoReleasePlan false false None false false None false true false ctx else [])
    @ (if needsDictRcDecSumStringValueHelper then generateDictRefCountDecHelper dictRefCountDecSumStringValueHelperLabel MemoryModel.NoReleasePlan false false None false false None false false true ctx else []) in
   recordPhase "ARM64 Helper Dict Generation" dictHelperTimer;
   let closureHelperTimer=startPhase () in
   let closureRcHelpers=(if rcHelperRequirements.LIR.needsClosureRcIncHelper then generateClosureRefCountIncHelper ctx else []) @ (if emitClosureRcDecHelper then generateClosureRefCountDecHelper dictDecHelperForReleasePlan ctx else []) in
   let streamRcHelpers=if needsStreamRcDecHelper then generateStreamRefCountDecHelper ctx else [] in
   recordPhase "ARM64 Helper Closure Stream Generation" closureHelperTimer;
   let recursiveHelperTimer=startPhase () in
   let recursiveNominalRcHelpers=MemoryPlanning.SemanticTypeSet.elements programMetadata.facts.recursiveReleaseTypes |> List.concat_map (generateRecursiveNominalRefCountDecHelper dictDecHelperForReleasePlan ctx) in
   recordPhase "ARM64 Helper Recursive Nominal Generation" recursiveHelperTimer;
   let cliHelperTimer=startPhase () in
   let cliArgvHelpers=StringOrder.Set.elements programMetadata.facts.cliArgvHelperLabels |> List.concat_map (generateCliArgvHelper ctx) in
   let cliHelpers=cliArgvHelpers @ (if needsCliProcessLifecycleHelpers && ARM64.targetOS target=Platform.Linux then generateLinuxCliSpawnProcessHelper () else []) @ (if needsCliProcessLifecycleHelpers && ARM64.targetOS target=Platform.Linux then generateLinuxCliProcessLifecycleHelpers ctx else []) @ (if needsCliRunProcessHelper && ARM64.targetOS target=Platform.Linux then generateLinuxCliRunProcessHelper () else []) @ (if needsCliExecuteHelper && ARM64.targetOS target=Platform.Linux then generateLinuxCliExecuteHelper () else []) in
   recordPhase "ARM64 Helper CLI Generation" cliHelperTimer;
   let runtimeErrorHelper=if programMetadata.facts.needsRuntimeErrorHelper then generateRuntimeErrorHelper target else [] in
   let helperInstructions=listRcHelpers@genericRcHelpers@dictRcHelpers@closureRcHelpers@streamRcHelpers@recursiveNominalRcHelpers@cliHelpers@runtimeErrorHelper in
   let peepholeTimer=startPhase () in
   let optimized=peepholeOptimize helperInstructions in
   recordPhase "ARM64 Codegen Peephole" peepholeTimer;optimized in
  let helperCacheKey={closurePayloadSizesFromParams=StringOrder.Map.bindings programMetadata.facts.closurePayloadSizesFromParams;closurePayloadSizesFromAllocs=FunctionIdMap.toList programMetadata.facts.closurePayloadSizesFromAllocs;closureCaptureTypes=StringOrder.Map.bindings programMetadata.facts.closureCaptureTypes;recursiveReleaseTypes=MemoryPlanning.SemanticTypeSet.elements programMetadata.facts.recursiveReleaseTypes;cliArgvHelperLabels=StringOrder.Set.elements programMetadata.facts.cliArgvHelperLabels;needsCliExecuteHelper=programMetadata.facts.needsCliExecuteHelper;needsCliRunProcessHelper=programMetadata.facts.needsCliRunProcessHelper;needsCliProcessLifecycleHelpers=programMetadata.facts.needsCliProcessLifecycleHelpers;needsRuntimeErrorHelper=programMetadata.facts.needsRuntimeErrorHelper;listDecHelperLabels=StringOrder.Set.elements rcHelperRequirements.LIR.listDecHelperLabels;plannedListDecHelpers=StringOrder.Map.bindings rcHelperRequirements.LIR.plannedListDecHelpers |> List.map (fun (label,(payloadSize,_releasePlan)) -> label,payloadSize);plannedGenericDecHelperLabels=StringOrder.Map.bindings rcHelperRequirements.LIR.plannedGenericDecHelpers |> List.map fst;plannedDictDecHelperLabels=StringOrder.Map.bindings rcHelperRequirements.LIR.plannedDictDecHelpers |> List.map fst;dictDecHelperLabels=StringOrder.Set.elements rcHelperRequirements.LIR.dictDecHelperLabels;needsListRcIncHelper=rcHelperRequirements.LIR.needsListRcIncHelper;needsDictRcIncHelper=rcHelperRequirements.LIR.needsDictRcIncHelper;needsClosureRcIncHelper=rcHelperRequirements.LIR.needsClosureRcIncHelper;needsClosureRcDecHelper=rcHelperRequirements.LIR.needsClosureRcDecHelper;needsStreamRcDecHelper=rcHelperRequirements.LIR.needsStreamRcDecHelper} in
  let optimizedHelperInstructions=match helperCache with Some cache->cache helperCacheKey generateHelperInstructions|None->generateHelperInstructions () in
  recordPhase "ARM64 Codegen Helpers" helperTimer;
  let assemblyTimer=startPhase () in
  let generated=GeneratedProgram (functionChunks@[{instructionParts=[optimizedHelperInstructions];reusableAcrossCompilations=Option.is_some helperCache}]) in
  recordPhase "ARM64 Codegen Assembly" assemblyTimer;generated)
(*
   Group order may differ from program order because _start is emitted first.
   IDs can also recur across separately compiled units, so match the exact
   original node within each ID bucket when retaining cache boundaries.
*)
let generateARM64WithOptionsAndCaches target options preparedSumShapeRegistry knownCalleeWrites functionCache refinementCache functionGroupCache (functionGroups:functionGroup list) metadataGroupCache helperCache metadataGroups lirOpExpansionRecorder phaseRecorder (LIR.Program (functions,variants,records))=
 let refinedFunctions=ARM64CalleeClobbers.refineWithCache refinementCache knownCalleeWrites functions in
 let refinedProgram=LIR.Program (refinedFunctions,variants,records) in
 let byId=List.fold_left (fun table ((original:LIR.functionDef),refined) ->
  let bucket=FunctionIdMap.tryFind original.LIR.id table |> Option.value ~default:[] in FunctionIdMap.add original.LIR.id (bucket@[original,refined]) table) FunctionIdMap.empty (List.combine functions refinedFunctions) in
 let refinedGroups=List.map (fun (group:functionGroup) ->
  let groupFunctions=List.map (fun (func:LIR.functionDef) ->
   let found=Option.bind (FunctionIdMap.tryFind func.LIR.id byId) (List.find_map (fun (original,refined) -> if original==func then Some refined else None)) in
   match found with Some refined->refined|None->Crash.crash ("ARM64 callee summaries: missing function "^func.LIR.name)) group.functions in {group with functions=groupFunctions}) functionGroups in
 let missingFacts=List.find_map (fun (func:LIR.functionDef) -> match func.LIR.codegenFacts with
  | None->Some ("ARM64 codegen requires prepared LIR; function '"^func.LIR.name^"' has no codegen facts")
  | Some facts when Option.is_none facts.LIR.arm64RcHelperRequirements->Some ("ARM64 codegen requires prepared LIR; function '"^func.LIR.name^"' has no ARM64 helper plan")
  | Some _->None) functions in
 match missingFacts with Some error->Error error|None->generatePreparedARM64WithOptionsAndCache target options preparedSumShapeRegistry functionCache functionGroupCache refinedGroups metadataGroupCache helperCache metadataGroups lirOpExpansionRecorder phaseRecorder refinedProgram
let generateARM64WithOptionsAndCache target options functionCache phaseRecorder program=generateARM64WithOptionsAndCaches target options None None functionCache None None [] None None [] None phaseRecorder program
let generateARM64WithOptions target options program=generateARM64WithOptionsAndCache target options None None program
(*
   Convert LIR program to ARM64 instructions (uses default options)
*)
let generateARM64 target program=generateARM64WithOptions target defaultOptions program
