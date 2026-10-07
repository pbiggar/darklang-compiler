(* EscapeAnalysisFacts.fs - Type and destruction proofs shared by SSA escape analysis. *)
val isScalarType : AST.semanticType -> bool
val hasNonObservableDestruction : TypeRegistries.typeRegistry -> MemoryModel.rcSumShapeRegistry -> bool -> AST.semanticType -> bool
val descriptorHasNonObservableDestruction : TypeRegistries.typeRegistry -> MemoryModel.rcSumShapeRegistry -> ANF.recordDescriptor -> bool
