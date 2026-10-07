(* ANF-independent memory representation and release contracts. *)
module SemanticTypeSet : Set.S with type elt = AST.semanticType

type recordRegistry = (string * AST.semanticType) list StringOrder.Map.t

val canUseTransparentSumPayload : AST.semanticType -> bool

val nullablePointerSumPayloadType :
  MemoryModel.rcSumShapeRegistry -> AST.semanticType -> AST.semanticType option

val isNullablePointerSumType :
  MemoryModel.rcSumShapeRegistry -> AST.semanticType -> bool

val isSpareImmediateSumType :
  MemoryModel.rcSumShapeRegistry -> AST.semanticType -> bool

val rcShapeOfType : recordRegistry -> AST.semanticType -> MemoryModel.rcShape

val inferredRecordTypeParamsRegistry :
  recordRegistry -> string list StringOrder.Map.t

val rcShapeOfTypeWithSums :
  recordRegistry ->
  string list StringOrder.Map.t ->
  MemoryModel.rcSumShapeRegistry ->
  AST.semanticType ->
  MemoryModel.rcShape

val rcShapeNeedsOwnedScopeRelease : MemoryModel.rcShape -> bool
val rcShapeIsRootManaged : MemoryModel.rcShape -> bool
val rcShapeNeedsRecursiveRelease : MemoryModel.rcShape -> bool
val rcShapeRootKind : MemoryModel.rcShape -> MemoryModel.rcKind option
val rcShapePayloadSize : MemoryModel.rcShape -> int option
val rcShapeStorageClass : MemoryModel.rcShape -> MemoryModel.rcStorageClass
val rcShapeIsOwnershipTransferRoot : MemoryModel.rcShape -> bool

val rcShapeRetainOperation :
  MemoryModel.rcShape -> MemoryModel.rcOperation option

val rcShapeReleaseOperation :
  MemoryModel.rcShape -> MemoryModel.rcOperation option

val rcShapeNeedsBorrowedRetain : MemoryModel.rcShape -> bool
val rcShapeNeedsAutomaticBindingDec : MemoryModel.rcShape -> bool
val rcShapeNeedsManagedAliasRootPreservation : MemoryModel.rcShape -> bool
val rcShapeReleasePlan : MemoryModel.rcShape -> MemoryModel.rcReleasePlan

val rcReleasePlanOfType :
  recordRegistry -> AST.semanticType -> MemoryModel.rcReleasePlan

val rcReleasePlanOfTypeWithSums :
  recordRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  AST.semanticType ->
  MemoryModel.rcReleasePlan

val recursiveReleaseTypes : MemoryModel.rcReleasePlan -> SemanticTypeSet.t
