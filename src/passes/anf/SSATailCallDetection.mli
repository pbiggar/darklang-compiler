(* SSATailCallDetection.mli - Preserve ownership-safe tail calls on SSA ANF blocks. *)
val detect : AST.loweredRecursiveMember FunctionIdMap.t -> SSAANF.functionDef -> SSAANF.functionDef
