(* LowerSumRepresentations.mli - Select native sum layouts after async interface inference. *)
val lowerProgram :
  LoweringPrimitives.sumRepresentationIndex -> ANF.program -> ANF.program

val lower :
  LoweringPrimitives.sumRepresentationIndex ->
  ANF.functionDef list ->
  ANF.functionDef list
