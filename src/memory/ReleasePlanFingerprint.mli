(* ReleasePlanFingerprint.mli - ANF-independent memory representation and release contracts. *)
val rcSourceTypeFingerprint : AST.semanticType -> string
val rcReleasePlanFingerprintHashFromChildren : MemoryModel.rcReleasePlan -> int64 list -> int64
val rcReleasePlanFingerprintString : int64 -> string
val rcReleasePlanFingerprintHash : MemoryModel.rcReleasePlan -> int64
val rcReleasePlanFingerprint : MemoryModel.rcReleasePlan -> string
val rcReleasePlanExceedsNodeCount : int -> MemoryModel.rcReleasePlan -> bool
val rcReleasePlanCompactKeyNodeThreshold : int
val rcReleasePlanCacheKey : AST.semanticType -> MemoryModel.rcReleasePlan -> string option
