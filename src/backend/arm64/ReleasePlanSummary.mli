val summarizePrecomputedReleasePlan : bool -> MemoryModel.rcReleasePlan -> ARM64CodeGenTypes.rcReleasePlanSummary
val precomputedEmptyRcHelperRequirements : ARM64CodeGenTypes.rcHelperRequirements
val precomputedReleasePlanSummary : bool -> LIR.rcReleasePlanMemoKey -> MemoryModel.rcReleasePlan -> ARM64CodeGenTypes.rcHelperRequirements -> ARM64CodeGenTypes.rcReleasePlanSummary * ARM64CodeGenTypes.rcHelperRequirements
val addPrecomputedReleasePlanRequirements : ARM64CodeGenTypes.rcReleasePlanSummary -> ARM64CodeGenTypes.rcHelperRequirements -> ARM64CodeGenTypes.rcHelperRequirements
val planFunctionArm64RcRequirements : ARM64CodeGenTypes.releasePlanSummaryCache option -> LIR.arm64ReleasePlanSummary LIR.ReleasePlanSummaryMap.t -> string -> LIR.functionCodegenFacts -> ARM64CodeGenTypes.rcHelperRequirements
val planRawSlotInitRetainTargets : LIR.recordRegistry -> MemoryModel.rcSumShapeRegistry -> LIR.functionCodegenFacts -> LIR.functionCodegenFacts
val mergePrecomputedRcHelperRequirements : ARM64CodeGenTypes.rcHelperRequirements -> ARM64CodeGenTypes.rcHelperRequirements -> ARM64CodeGenTypes.rcHelperRequirements
