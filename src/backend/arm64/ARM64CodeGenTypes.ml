(*
   CodeGenTypes.fs - Define target code generation options, contexts, and planning facts.
*)
[@@@warning "-4"]
(*
   Disable free list memory reuse (always bump allocate)
   Enable coverage instrumentation
   Number of coverage expressions (determines buffer size)
   Enable leak checking instrumentation
*)
type codeGenOptions={disableFreeList:bool;enableCoverage:bool;coverageExprCount:int;enableLeakCheck:bool}
(*
   Caller-owned reuse for expensive, immutable release-plan summaries.
   The cache validates complete release-plan shapes before returning a value.
*)
type releasePlanSummaryCache=bool -> string -> MemoryModel.rcReleasePlan -> (unit -> LIR.arm64ReleasePlanSummary) -> LIR.arm64ReleasePlanSummary
(*
   Opt-in attribution for one freshly generated LIR instruction. The elapsed
   value uses Stopwatch timestamp ticks, and the instruction count is measured
   before the function-level ARM64 peephole pass.
*)
type lirOpExpansionRecorder=string -> string -> string -> int -> int64 -> unit
(*
   Code generation context (passed through to instruction conversion)
   Deterministic block/instruction identity for labels emitted by an effect.
   One source effect can be cloned into multiple CFG locations.
   Function context for tail call epilogue generation
*)
type codeGenContext={target:ARM64.targetConfig;options:codeGenOptions;sumShapeRegistry:MemoryModel.rcSumShapeRegistry;recordRegistry:LIR.recordRegistry;rawSlotInitRetainTargets:LIR.arm64SlotInitRootRetainTarget option LIR.SemanticTypeMap.t option;closurePayloadSizes:int StringOrder.Map.t;closureCaptureTypes:AST.semanticType list StringOrder.Map.t;functionNames:string FunctionIdMap.t;functionName:string;instructionSite:string;stackSize:int;usedCalleeSaved:LIR.physReg list;usedCalleeSavedF:LIR.physFPReg list;heapOverflowLabel:string;recordLirOpExpansion:lirOpExpansionRecorder option}
(*
   Default code generation options
*)
let defaultOptions={disableFreeList=false;enableCoverage=false;coverageExprCount=0;enableLeakCheck=false}
let functionName (ctx:codeGenContext) functionId =
 match FunctionIdMap.tryFind functionId ctx.functionNames with Some name -> name | None -> Crash.crash (Printf.sprintf "ARM64 code generation: missing function name for identity %Lu" (AST.functionIdValue functionId))
let rcSumShapeRegistryFromVariantRegistry variantRegistry =
 StringOrder.Map.map (fun (typeVariants:LIR.typeVariants) ->
 let sorted=List.stable_sort (fun (left:LIR.variantInfo) (right:LIR.variantInfo) -> Int.compare left.LIR.tag right.LIR.tag) typeVariants.LIR.variants in
 {MemoryModel.typeParams=typeVariants.LIR.typeParams;payloads=List.map (fun (variant:LIR.variantInfo) -> variant.LIR.tag,variant.LIR.payload) sorted;
 unaryPayloadTags=MemoryModel.IntSet.of_list (List.filter_map (fun (variant:LIR.variantInfo) -> if variant.LIR.fieldCount=1 then Some variant.LIR.tag else None) typeVariants.LIR.variants)}) variantRegistry
let leakCounterLabel=Symbolic.leakCounterLabelName
let heapOutOfMemoryMessage="Out of heap memory"
let heapMmapSizeBytes=Int64.mul (Int64.mul 512L 1024L) 1024L
(*
   512MB == 0x20000000
*)
let heapMmapSizeMovzImm16=0x2000
let heapOverflowLabelPrefix="__heap_oom_"
let runtimeErrorHelperLabel="__dark_runtime_error"
let listRefCountIncHelperLabel="__dark_list_refcount_inc_helper"
let listRefCountDecHelperLabel="__dark_list_refcount_dec_helper"
let plannedListRefCountDecHelperLabelPrefix="__dark_list_refcount_dec_plan_"
let plannedGenericRefCountDecHelperLabelPrefix="__dark_generic_refcount_dec_plan_"
let listRefCountDecStringHelperLabel="__dark_list_refcount_dec_string_helper"
let listRefCountDecBlobHelperLabel="__dark_list_refcount_dec_blob_helper"
let listRefCountDecListHelperLabel="__dark_list_refcount_dec_list_helper"
let listRefCountDecDictHelperLabel="__dark_list_refcount_dec_dict_helper"
let listRefCountDecDictListHelperLabel="__dark_list_refcount_dec_dict_list_helper"
let listRefCountDecClosureHelperLabel="__dark_list_refcount_dec_closure_helper"
let dictRefCountIncHelperLabel="__dark_dict_refcount_inc_helper"
let dictRefCountDecHelperLabel="__dark_dict_refcount_dec_helper"
let dictRefCountDecListValueHelperLabel="__dark_dict_refcount_dec_list_value_helper"
let dictRefCountDecDictValueHelperLabel="__dark_dict_refcount_dec_dict_value_helper"
let dictRefCountDecDictListValueHelperLabel="__dark_dict_refcount_dec_dict_list_value_helper"
let dictRefCountDecTupleStringListValueHelperLabel="__dark_dict_refcount_dec_tuple_string_list_value_helper"
let dictRefCountDecTupleStringListDictValueHelperLabel="__dark_dict_refcount_dec_tuple_string_list_dict_value_helper"
let dictRefCountDecSumStringValueHelperLabel="__dark_dict_refcount_dec_sum_string_value_helper"
let plannedDictRefCountDecHelperLabelPrefix="__dark_dict_refcount_dec_plan_"
let closureRefCountIncHelperLabel="__dark_closure_refcount_inc_helper"
let closureRefCountDecHelperLabel="__dark_closure_refcount_dec_helper"
let streamRefCountDecHelperLabel="__dark_stream_refcount_dec_helper"
(*
   Small fixed blocks are cheaper to build directly than to name and cache.
   Stop counting as soon as repeated recursive expansion becomes the dominant
   cost and an outlined helper is worthwhile.
*)
let genericReleaseHelperNodeThreshold=ReleasePlanFingerprint.rcReleasePlanCompactKeyNodeThreshold
let genericReleasePlanIsExpensive releasePlan=ReleasePlanFingerprint.rcReleasePlanExceedsNodeCount genericReleaseHelperNodeThreshold releasePlan
let callerOwnsSinglePayloadSum functionName=Text.startsWith functionName "Darklang.Stdlib.Dict." || not (Text.startsWith functionName "Darklang.Stdlib.")
let recursiveNominalRefCountDecHelperLabel sourceType="__dark_recursive_nominal_rc_dec_"^ReleasePlanFingerprint.rcSourceTypeFingerprint sourceType
let plannedListDecHelperLabelForFingerprint fingerprint=plannedListRefCountDecHelperLabelPrefix^fingerprint
let plannedListDecHelperLabelForReleasePlan releasePlan=plannedListDecHelperLabelForFingerprint (ReleasePlanFingerprint.rcReleasePlanFingerprint releasePlan)
let plannedGenericDecHelperBaseLabelForFingerprint fingerprint=plannedGenericRefCountDecHelperLabelPrefix^fingerprint
let specializePlannedGenericDecHelperLabel ownsSinglePayloadSum baseLabel=let ownership=if ownsSinglePayloadSum then "owned" else "borrowed" in baseLabel^"_"^ownership
let plannedDictDecHelperLabelForFingerprint fingerprint=plannedDictRefCountDecHelperLabelPrefix^fingerprint
let plannedDictDecHelperLabelForReleasePlan releasePlan=plannedDictDecHelperLabelForFingerprint (ReleasePlanFingerprint.rcReleasePlanFingerprint releasePlan)
type rcReleasePlanSummary=LIR.arm64ReleasePlanSummary
type rcHelperRequirements=LIR.arm64RcHelperRequirements
(*
   ARM64-only metadata assembled from the reachable functions' carried facts.
*)
type arm64ProgramFacts={closurePayloadSizesFromParams:int StringOrder.Map.t;closurePayloadSizesFromAllocs:int FunctionIdMap.t;closureCaptureTypes:AST.semanticType list StringOrder.Map.t;recursiveReleaseTypes:MemoryPlanning.SemanticTypeSet.t;cliArgvHelperLabels:StringOrder.Set.t;needsCliExecuteHelper:bool;needsCliRunProcessHelper:bool;needsCliProcessLifecycleHelpers:bool;needsRuntimeErrorHelper:bool}
type arm64ProgramMetadata={facts:arm64ProgramFacts;rcHelperRequirements:rcHelperRequirements}
let slotInitRootRetainTarget recordRegistry sumShapeRegistry valueType =
 let shapeOfKnownType typ=match typ with AST.TRecord (name,_) when not (StringOrder.Map.mem name recordRegistry) -> None | _ -> Some (MemoryPlanning.rcShapeOfTypeWithSums recordRegistry (MemoryPlanning.inferredRecordTypeParamsRegistry recordRegistry) sumShapeRegistry typ) in
 Option.bind (shapeOfKnownType valueType) (function
 | MemoryModel.TaggedListShape _ -> Some LIR.SlotInitListRootRetain
 | MemoryModel.DictRoot _ -> Some LIR.SlotInitDictRootRetain
 | MemoryModel.DynamicString | MemoryModel.DynamicBlob | MemoryModel.DynamicInt -> Some LIR.SlotInitDynamicBufferRetain
 | MemoryModel.ClosureShape _ -> Some LIR.SlotInitClosureRootRetain
 | MemoryModel.FixedBlock (payloadSize,_) -> (match valueType with AST.TTuple _ | AST.TRecord _ | AST.TInt128 | AST.TUInt128 -> Some (LIR.SlotInitGenericRootRetain payloadSize) | _ -> None)
 | MemoryModel.StreamRoot -> Some (LIR.SlotInitGenericRootRetain 24)
 | MemoryModel.BoxedSum (payloadSize,_,_) -> (match valueType with AST.TSum _ -> Some (LIR.SlotInitGenericRootRetain payloadSize) | _ -> None)
 | MemoryModel.RecursiveNominalRef _ -> Some (LIR.SlotInitGenericRootRetain 16)
 | MemoryModel.Immediate | MemoryModel.StaticString | MemoryModel.RawUnmanaged -> None)
