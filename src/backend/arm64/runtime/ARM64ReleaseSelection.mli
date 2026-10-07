val releasePlanRootKindAt :
  int -> MemoryModel.rcKind -> MemoryModel.rcFieldRelease list -> bool

val releasePlanDynamicOperationAt :
  int -> MemoryModel.rcOperation -> MemoryModel.rcFieldRelease list -> bool

val listDecHelperForElementRelease :
  string -> MemoryModel.rcReleasePlan -> string

val listDecHelperForReleasePlan : MemoryModel.rcReleasePlan -> string

val dictPayloadReleaseNeedsPlannedHelper :
  MemoryModel.rcReleasePlan -> MemoryModel.rcReleasePlan -> bool

val dictDecHelperForReleasePlanWithFingerprint :
  string -> MemoryModel.rcReleasePlan -> string

val dictDecHelperForReleasePlan : MemoryModel.rcReleasePlan -> string
