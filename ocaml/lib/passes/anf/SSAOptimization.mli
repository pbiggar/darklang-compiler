(* SSAOptimization.fs - Simplify typed high-level SSA before specialization. *)
val optimizeFunction : ANFConstants.optimizeContext -> ANFConstants.optimizeOptions -> SSAANF.functionDef -> SSAANF.functionDef
