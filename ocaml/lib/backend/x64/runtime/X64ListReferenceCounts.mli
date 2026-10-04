type listLeafPayloadRelease =
 | NoLeafPayloadRelease
 | FixedBlockPlannedLeafPayload of int * MemoryModel.rcReleasePlan
 | RecursivePlannedLeafPayload of AST.semanticType
 | ListLeafPayload
 | PlannedListLeafPayload of MemoryModel.rcReleasePlan
 | ClosureLeafPayload
 | DictLeafPayload
 | DictListLeafPayload
 | PlannedDictLeafPayload of MemoryModel.rcReleasePlan
 | DynamicBufferLeafPayload
 | DynamicIntLeafPayload
val listRefCountDecHelperSpecs : (string * listLeafPayloadRelease) list
val generateNeededListRefCountDecHelpers : StringOrder.Set.t -> (int * MemoryModel.rcReleasePlan) StringOrder.Map.t -> bool -> LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> X86_64.instr list
val selectedListRefCountDecHelpersNeedDictDecHelper : StringOrder.Set.t -> bool
val selectedListRefCountDecHelpersNeedDictListValueDecHelper : StringOrder.Set.t -> bool
val selectedListRefCountDecHelpersNeedClosureDecHelper : StringOrder.Set.t -> bool
val listRefCountIncHelperLabel : string
val generateListRefCountIncHelper : unit -> X86_64.instr list
