val genDynamicBufferFieldRelease : X64CodeGenTypes.funcCtx -> bool -> int -> X86_64.instr list
val listRefCountDecHelperLabel : string
val listRefCountDecListHelperLabel : string
val listRefCountDecClosureHelperLabel : string
val listRefCountDecDictHelperLabel : string
val listRefCountDecDictListHelperLabel : string
val listRefCountDecDynamicBufferHelperLabel : string
val listRefCountDecDynamicIntHelperLabel : string
val dictRefCountIncHelperLabel : string
val dictRefCountDecHelperLabel : string
val dictRefCountDecDynamicKeyHelperLabel : string
val dictRefCountDecDynamicValueHelperLabel : string
val dictRefCountDecDynamicKeyValueHelperLabel : string
val dictRefCountDecDynamicKeyListValueHelperLabel : string
val dictRefCountDecDynamicKeyDictValueHelperLabel : string
val dictRefCountDecDynamicKeyDictListValueHelperLabel : string
val dictRefCountDecListValueHelperLabel : string
val dictRefCountDecDictValueHelperLabel : string
val dictRefCountDecDictListValueHelperLabel : string
val dictRefCountDecTupleStringListValueHelperLabel : string
val dictRefCountDecTupleStringListDictValueHelperLabel : string
val dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel : string
val dictRefCountDecSumStringValueHelperLabel : string
val closureRefCountIncHelperLabel : string
val closureRefCountDecHelperLabel : string
val streamRefCountDecHelperLabel : string
type slotInitRootRetainTarget = SlotInitListRootRetain | SlotInitDictRootRetain | SlotInitDynamicBufferRetain | SlotInitClosureRootRetain | SlotInitGenericRootRetain of int
val slotInitRootRetainTarget : LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> AST.semanticType -> slotInitRootRetainTarget option
val tryRcReleasePlanOfType : LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> AST.semanticType -> MemoryModel.rcReleasePlan option
val rcMetadataReleasePlan : MemoryModel.rcMetadata option -> MemoryModel.rcReleasePlan option
val requiredRcMetadataReleasePlan : string -> MemoryModel.rcMetadata option -> MemoryModel.rcReleasePlan
val releasePlanIsRootKind : MemoryModel.rcKind -> MemoryModel.rcReleasePlan -> bool
val releasePlanIsDictWithListValue : MemoryModel.rcReleasePlan -> bool
val recursiveNominalRefCountDecHelperLabel : AST.semanticType -> string
val plannedListDecHelperLabelForReleasePlan : MemoryModel.rcReleasePlan -> string
val rcReleasePlanContains : (MemoryModel.rcReleasePlan -> bool) -> MemoryModel.rcReleasePlan -> bool
val genClosureFieldRelease : int -> X86_64.instr list
val listDecHelperForReleasePlan : MemoryModel.rcReleasePlan -> string
val listDecHelperForType : LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> AST.semanticType -> string
val dictPayloadReleaseNeedsPlannedHelper : MemoryModel.rcReleasePlan -> MemoryModel.rcReleasePlan -> bool
val dictDecHelperForReleasePlan : MemoryModel.rcReleasePlan -> string
val dictTupleStringListValueReleasePlan : MemoryModel.rcReleasePlan
val dictTupleStringListDictValueReleasePlan : MemoryModel.rcReleasePlan
val dictSumStringValueReleasePlan : MemoryModel.rcReleasePlan
val genDictFieldRelease : int -> MemoryModel.rcReleasePlan -> X86_64.instr list
val genListFieldRelease : int -> MemoryModel.rcReleasePlan -> X86_64.instr list
