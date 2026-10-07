(* Continuations.mli - Substitute ANF return continuations without changing lexical joins. *)
val isSupportedJoinArgumentType : AST.semanticType -> bool
val bindReturns : ANF.aExpr -> (ANF.atom -> ANF.aExpr) -> ANF.aExpr
val wrapBindings : (ANF.tempId * ANF.cExpr) list -> ANF.aExpr -> ANF.aExpr
