(* HelperDependencies.mli - Solve transitive generated comparison dependencies. *)
module TypeSet : Set.S with type elt = AST.semanticType
type eqHelperGenerationState = {inProgress : StringOrder.Set.t; generated : AST.functionDef StringOrder.Map.t}
type compareHelperGenerationState = {inProgress : StringOrder.Set.t; generated : AST.functionDef StringOrder.Map.t}
val ensureEqHelperForType : Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> Types.indexedSumTypeRegistry -> AST.semanticType -> eqHelperGenerationState -> eqHelperGenerationState
val ensureCompareHelperForType : Types.aliasRegistry -> Types.indexedTypeRegistry -> Types.variantLookup -> Types.indexedSumTypeRegistry -> AST.semanticType -> compareHelperGenerationState -> compareHelperGenerationState
val collectCompareHelperTypesFromExpr : Types.aliasRegistry -> AST.expr -> TypeSet.t
val collectEqHelperTypesFromExpr : Types.aliasRegistry -> AST.expr -> TypeSet.t
