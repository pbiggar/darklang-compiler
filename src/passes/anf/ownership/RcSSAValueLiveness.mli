(* SSAValueLiveness.fs - Compute operation and edge liveness for SSA ownership cleanup. *)
type facts = {atEntry : RcReturnAnalysis.TempSet.t SSAANF.LabelMap.t; atTerminator : RcReturnAnalysis.TempSet.t SSAANF.LabelMap.t; afterDefinition : RcReturnAnalysis.TempSet.t RcTypeFacts.TempMap.t}
val analyze : SSAANF.functionDef -> facts
