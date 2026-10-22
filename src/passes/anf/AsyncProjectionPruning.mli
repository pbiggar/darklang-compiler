(* AsyncProjectionPruning.mli - Remove unused typed projections before they introduce async demands. *)
val prune : ANF.functionDef -> ANF.functionDef
