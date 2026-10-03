(* ClosureComparisons.fs - Plan equality for lifted closures and their captures. *)
type lambdaComparisonPlan = {
 identity : AST.functionId option; captureNames : AST.bindingId list;
 captureTypes : AST.semanticType list; captureExprs : CheckedAST.expr list;
 body : CheckedAST.expr; compareCaptures : bool
}
val comparisonNameForIdentity : AST.functionId option -> AST.semanticType list -> ClosureAnalysis.liftState -> string * bool * ClosureAnalysis.liftState
val planLambdaComparison : CheckedAST.lambdaParameter NonEmptyList.t -> CheckedAST.expr -> ClosureAnalysis.liftState -> (lambdaComparisonPlan * ClosureAnalysis.liftState, string) result
val makeClosureComparator : string -> AST.semanticType list -> bool -> LoweringPrimitives.variantLookup -> CheckedAST.symbols -> CheckedAST.functionDef * CheckedAST.symbols
val rewriteRecursiveSelfReferences : AST.bindingId -> AST.bindingId -> CheckedAST.expr -> CheckedAST.expr
val rewriteLiftedSelfCalls : AST.functionId -> AST.bindingId -> CheckedAST.expr -> CheckedAST.expr
