(* LowerSumRepresentations.mli - Select native sum layouts after async interface inference. *)
val lower :
  LoweringPrimitives.sumRepresentationIndex ->
  ANF.functionDef list ->
  ANF.functionDef list
