(* RcSSARefCountInsertion.mli - Insert ownership operations directly in SSA ANF blocks. *)
val insertBlockLocal :
  RcTypeFacts.typeContext ->
  RcReturnAnalysis.TempSet.t ->
  SSAANF.functionDef ->
  SSAANF.functionDef
