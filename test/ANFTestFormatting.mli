(* Complete typed ANF and memory descriptions for original test diagnostics. *)
open Dark_compiler

val memoryModel_canonicalBufferKind :
  MemoryModel.canonicalBufferKind -> StructuralValue.value

val memoryModel_rcKind : MemoryModel.rcKind -> StructuralValue.value
val memoryModel_rcShape : MemoryModel.rcShape -> StructuralValue.value

val memoryModel_rcBoxedSumVariantShape :
  MemoryModel.rcBoxedSumVariantShape -> StructuralValue.value

val memoryModel_rcSumShapeInfo :
  MemoryModel.rcSumShapeInfo -> StructuralValue.value

val memoryModel_rcSumShapeRegistry :
  MemoryModel.rcSumShapeRegistry -> StructuralValue.value

val memoryModel_rcOperation : MemoryModel.rcOperation -> StructuralValue.value

val memoryModel_rcStorageClass :
  MemoryModel.rcStorageClass -> StructuralValue.value

val memoryModel_rcReleasePlan :
  MemoryModel.rcReleasePlan -> StructuralValue.value

val memoryModel_rcPayloadReleasePlan :
  MemoryModel.rcPayloadReleasePlan -> StructuralValue.value

val memoryModel_rcFieldRelease :
  MemoryModel.rcFieldRelease -> StructuralValue.value

val memoryModel_rcBoxedSumVariantRelease :
  MemoryModel.rcBoxedSumVariantRelease -> StructuralValue.value

val memoryModel_rcMetadata : MemoryModel.rcMetadata -> StructuralValue.value
val aNF_tempId : ANF.tempId -> StructuralValue.value
val aNF_typedParam : ANF.typedParam -> StructuralValue.value
val aNF_sizedInt : ANF.sizedInt -> StructuralValue.value
val aNF_atom : ANF.atom -> StructuralValue.value
val aNF_binOp : ANF.binOp -> StructuralValue.value
val aNF_unaryOp : ANF.unaryOp -> StructuralValue.value
val aNF_returnOwnership : ANF.returnOwnership -> StructuralValue.value
val aNF_cliOperation : ANF.cliOperation -> StructuralValue.value
val aNF_recordDescriptor : ANF.recordDescriptor -> StructuralValue.value
val aNF_cExpr : ANF.cExpr -> StructuralValue.value
val aNF_aExpr : ANF.aExpr -> StructuralValue.value
val aNF_functionDef : ANF.functionDef -> StructuralValue.value
val aNF_program : ANF.program -> StructuralValue.value
val aNF_varGen : ANF.varGen -> StructuralValue.value
val aNF_exprId : ANF.exprId -> StructuralValue.value
val aNF_exprIdGen : ANF.exprIdGen -> StructuralValue.value
val aNF_coverageMapping : ANF.coverageMapping -> StructuralValue.value
val expr : ANF.aExpr -> string
