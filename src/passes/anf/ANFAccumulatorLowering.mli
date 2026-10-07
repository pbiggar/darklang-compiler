(* ANFAccumulatorLowering.mli - Generate recursion helpers before SSA construction. *)
val lower : int64 -> ANFConstants.optimizeContext -> SpecializationIdentity.FunctionSet.t -> ANF.functionDef StringOrder.Map.t -> ANF.program -> ANF.program
