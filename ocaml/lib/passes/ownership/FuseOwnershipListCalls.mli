(* FuseOwnershipListCalls.fs - Inline selected List<Int64> boundaries before region extraction. *)
type fusionResult = {functions : CheckedAST.functionDef list; fusedSites : LowerOwnershipVariants.SiteSet.t}
val fuse : string FunctionIdMap.t -> ('leaf, 'id) MaterializeOwnershipVariants.plan -> CheckedAST.functionDef list -> fusionResult
