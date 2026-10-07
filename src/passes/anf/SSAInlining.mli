(* SSAInlining.mli - Inline eligible typed SSA functions at direct call sites. *)
val inlineProgramWithExternalCandidatesAndExclusions :
  InliningCommon.inliningConfig ->
  InliningCommon.functionInfo FunctionIdMap.t ->
  SSAANF.functionDef list ->
  SpecializationIdentity.FunctionSet.t ->
  ANF.functionDef list ->
  SSAANF.functionDef list ->
  SSAANF.functionDef list
