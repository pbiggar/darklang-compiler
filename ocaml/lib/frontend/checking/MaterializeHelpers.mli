(* MaterializeHelpers.mli - Insert reachable comparison definitions and calls. *)
val materializeEqHelpersInTopLevelsWithIndexedSums : Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> Types.indexedSumTypeRegistry -> AST.topLevel list -> AST.topLevel list
val materializeEqHelpersInTopLevels : Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> AST.topLevel list -> AST.topLevel list
val materializeCompareHelpersInTopLevels : Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> AST.topLevel list -> AST.topLevel list
