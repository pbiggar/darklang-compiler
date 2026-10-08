(* SSAOptimization.mli - Simplify typed high-level SSA before specialization. *)
val optimizeFunctions :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  SSAANF.functionDef list ->
  SSAANF.functionDef list
