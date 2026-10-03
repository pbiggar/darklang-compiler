(* Materialize comparison helpers after specialization entirely in checked syntax. *)
val materializeEqHelpersInTopLevelsWithIndexedSums : CheckedAST.symbols -> Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> Types.indexedSumTypeRegistry -> CheckedAST.topLevel list -> CheckedAST.symbols * CheckedAST.topLevel list
val materializeEqHelpersInTopLevels : CheckedAST.symbols -> Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> CheckedAST.topLevel list -> CheckedAST.symbols * CheckedAST.topLevel list
