type codeGenOptions={disableFreeList:bool;enableCoverage:bool;coverageExprCount:int;enableLeakCheck:bool}
type releasePlanSummaryCache=bool -> string -> MemoryModel.rcReleasePlan -> (unit -> LIR.arm64ReleasePlanSummary) -> LIR.arm64ReleasePlanSummary
type lirOpExpansionRecorder=string -> string -> string -> int -> int64 -> unit
type codeGenContext={target:ARM64.targetConfig;options:codeGenOptions;sumShapeRegistry:MemoryModel.rcSumShapeRegistry;recordRegistry:LIR.recordRegistry;rawSlotInitRetainTargets:LIR.arm64SlotInitRootRetainTarget option LIR.SemanticTypeMap.t option;closurePayloadSizes:int StringOrder.Map.t;closureCaptureTypes:AST.semanticType list StringOrder.Map.t;functionNames:string FunctionIdMap.t;functionName:string;instructionSite:string;stackSize:int;usedCalleeSaved:LIR.physReg list;usedCalleeSavedF:LIR.physFPReg list;heapOverflowLabel:string;recordLirOpExpansion:lirOpExpansionRecorder option}
val defaultOptions : codeGenOptions
val functionName : codeGenContext -> AST.functionId -> string
val rcSumShapeRegistryFromVariantRegistry : LIR.variantRegistry -> MemoryModel.rcSumShapeRegistry
val leakCounterLabel : string
val heapOutOfMemoryMessage : string
val heapMmapSizeBytes : int64
val heapMmapSizeMovzImm16 : int
val heapOverflowLabelPrefix : string
val runtimeErrorHelperLabel : string
val listRefCountIncHelperLabel : string
val listRefCountDecHelperLabel : string
val plannedGenericRefCountDecHelperLabelPrefix : string
val listRefCountDecStringHelperLabel : string
val listRefCountDecBlobHelperLabel : string
val listRefCountDecListHelperLabel : string
val listRefCountDecDictHelperLabel : string
val listRefCountDecDictListHelperLabel : string
val listRefCountDecClosureHelperLabel : string
val dictRefCountIncHelperLabel : string
val dictRefCountDecHelperLabel : string
val dictRefCountDecListValueHelperLabel : string
val dictRefCountDecDictValueHelperLabel : string
val dictRefCountDecDictListValueHelperLabel : string
val dictRefCountDecTupleStringListValueHelperLabel : string
val dictRefCountDecTupleStringListDictValueHelperLabel : string
val dictRefCountDecSumStringValueHelperLabel : string
val closureRefCountIncHelperLabel : string
val closureRefCountDecHelperLabel : string
val streamRefCountDecHelperLabel : string
val genericReleasePlanIsExpensive : MemoryModel.rcReleasePlan -> bool
val callerOwnsSinglePayloadSum : string -> bool
val recursiveNominalRefCountDecHelperLabel : AST.semanticType -> string
val plannedListDecHelperLabelForFingerprint : string -> string
val plannedListDecHelperLabelForReleasePlan : MemoryModel.rcReleasePlan -> string
val plannedGenericDecHelperBaseLabelForFingerprint : string -> string
val specializePlannedGenericDecHelperLabel : bool -> string -> string
val plannedDictDecHelperLabelForFingerprint : string -> string
val plannedDictDecHelperLabelForReleasePlan : MemoryModel.rcReleasePlan -> string
type rcReleasePlanSummary=LIR.arm64ReleasePlanSummary
type rcHelperRequirements=LIR.arm64RcHelperRequirements
type arm64ProgramFacts={closurePayloadSizesFromParams:int StringOrder.Map.t;closurePayloadSizesFromAllocs:int FunctionIdMap.t;closureCaptureTypes:AST.semanticType list StringOrder.Map.t;recursiveReleaseTypes:MemoryPlanning.SemanticTypeSet.t;cliArgvHelperLabels:StringOrder.Set.t;needsCliExecuteHelper:bool;needsCliRunProcessHelper:bool;needsCliProcessLifecycleHelpers:bool;needsRuntimeErrorHelper:bool}
type arm64ProgramMetadata={facts:arm64ProgramFacts;rcHelperRequirements:rcHelperRequirements}
val slotInitRootRetainTarget : LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> AST.semanticType -> LIR.arm64SlotInitRootRetainTarget option
