(* Frozen structural layouts for whole-function ownership and lowering failures. *)
val analysisError : AnalyzeFunctionOwnership.analysisError -> string
val loweringError : LowerOwnershipVariants.loweringError -> string
