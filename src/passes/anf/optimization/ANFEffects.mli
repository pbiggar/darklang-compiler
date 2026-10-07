(* Effects.fs - Describe ANF evaluation effects and temporary uses. *)
module TempSet : Set.S with type elt = ANF.tempId
val canForwardTupleElement : ANFConstants.optimizeContext -> ANFConstants.typeEnv -> ANF.atom -> bool
val mustPreserveEvaluation : ANFConstants.optimizeContext -> ANF.cExpr -> bool
val foldAtomTempIds : (ANF.tempId -> 'a -> 'a) -> ANF.atom -> 'a -> 'a
val addAtomUse : ANF.atom -> TempSet.t -> TempSet.t
val atomUsesTemp : ANF.tempId -> ANF.atom -> bool
val atomsUseTemp : ANF.tempId -> ANF.atom list -> bool
val foldCExprTempIds : (ANF.tempId -> 'a -> 'a) -> ANF.cExpr -> 'a -> 'a
val addCExprUses : ANF.cExpr -> TempSet.t -> TempSet.t
val cexprTempUses : ANF.cExpr -> TempSet.t
val cexprUsesTemp : ANF.tempId -> ANF.cExpr -> bool
