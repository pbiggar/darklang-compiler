(* RcSSAReturnAnalysis.mli - Track values that flow to a return across SSA block edges. *)
type facts = {atEntry : RcReturnAnalysis.TempSet.t SSAANF.LabelMap.t; atTerminator : RcReturnAnalysis.TempSet.t SSAANF.LabelMap.t; afterDefinition : RcReturnAnalysis.TempSet.t RcTypeFacts.TempMap.t}
val analyze : SSAANF.functionDef -> facts
