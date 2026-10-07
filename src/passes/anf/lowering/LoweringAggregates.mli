(* LoweringAggregates.mli - Build skew-list storage and bind typed deconstruction patterns. *)
val buildSkewListLiteral : AST.semanticType -> (ANF.atom * AST.semanticType) list -> ANF.varGen -> (ANF.tempId * ANF.cExpr) list -> ANF.atom * (ANF.tempId * ANF.cExpr) list * ANF.varGen
val lowerLetPatternBindings : CheckedAST.letPattern -> ANF.atom -> AST.semanticType -> TypeRegistries.varEnv -> (ANF.tempId * ANF.cExpr) list -> ANF.varGen -> (TypeRegistries.varEnv * (ANF.tempId * ANF.cExpr) list * ANF.varGen, string) result
val letPatternAcceptsType : CheckedAST.letPattern -> AST.semanticType -> bool
