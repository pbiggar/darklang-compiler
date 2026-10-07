(* RcInsertExpression.mli - Elaborate ANF expression ownership using return and alias facts. *)
val inferBindingType : RcTypeFacts.typeContext -> ANF.tempId -> ANF.cExpr -> RcReturnAnalysis.returnAnnotatedExpr -> AST.semanticType
val insertRCWithAnalysis : RcReturnAnalysis.TempSet.t RcTypeFacts.TempMap.t -> RcCleanup.returnDec list -> RcTypeFacts.typeContext -> AST.functionId option -> RcReturnAnalysis.returnAnnotatedExpr -> ANF.varGen -> RcCleanup.returnDec list -> RcCleanup.returnDec list -> (ANF.tempId * AST.semanticType * MemoryModel.rcShape) list -> AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val insertRCInternal : RcTypeFacts.typeContext -> ANF.aExpr -> ANF.varGen -> AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val insertRC : RcTypeFacts.typeContext -> ANF.aExpr -> ANF.varGen -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
