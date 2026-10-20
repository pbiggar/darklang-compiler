(* AsyncGraphTypes.mli - Resolve finite transport graphs into physical types and ownership metadata. *)
val resolve :
  (AsyncStorageKind.t, 'a) AsyncStorageGraph.t -> AST.semanticType ->
  (AST.semanticType * MemoryModel.rcSumShapeRegistry, string) result
