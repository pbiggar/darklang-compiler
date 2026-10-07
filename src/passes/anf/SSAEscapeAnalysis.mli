(* SSAEscapeAnalysis.mli - Scalar replacement and unique fixed-block reuse on SSA ANF. *)
val optimizeFunction :
  TypeRegistries.typeRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  SSAANF.functionDef ->
  SSAANF.functionDef
