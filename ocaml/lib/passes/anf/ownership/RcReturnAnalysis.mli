(* ReturnAnalysis.fs - Analyze aliases and ownership escaping through returns and aggregates. *)
module TempMap = InliningCommon.TempMap
module TempSet = ANFEffects.TempSet
type returnAnnotatedExpr = RReturn of ANF.atom * TempSet.t | RLet of ANF.tempId * ANF.cExpr * returnAnnotatedExpr * TempSet.t | RIf of ANF.atom * returnAnnotatedExpr * returnAnnotatedExpr * TempSet.t | RJoin of ANF.typedParam * returnAnnotatedExpr * returnAnnotatedExpr * TempSet.t | RJump of ANF.tempId * ANF.atom * TempSet.t
val returnedSet : returnAnnotatedExpr -> TempSet.t
val collectAliasChain : ANF.tempId TempMap.t -> ANF.tempId -> TempSet.t
val tryOwnershipPreservingAliasSource : ANF.cExpr -> ANF.tempId option
val analyzeReturns : TempSet.t TempMap.t -> ANF.tempId TempMap.t -> ANF.aExpr -> returnAnnotatedExpr
val transfersIntoReturnedAggregate : ANF.tempId -> returnAnnotatedExpr -> bool
val transfersIntoRawSlot : ANF.tempId -> returnAnnotatedExpr -> bool
val isBorrowingExpr : ANF.cExpr -> bool
