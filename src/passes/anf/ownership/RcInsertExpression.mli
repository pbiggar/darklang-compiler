(* RcInsertExpression.mli - Elaborate ANF expression ownership using return and alias facts. *)
val insertRCWithAnalysis :
  RcReturnAnalysis.TempSet.t RcTypeFacts.TempMap.t ->
  RcCleanup.returnDec list ->
  RcTypeFacts.typeContext ->
  AST.functionId option ->
  RcReturnAnalysis.returnAnnotatedExpr ->
  ANF.varGen ->
  RcCleanup.returnDec list ->
  RcCleanup.returnDec list ->
  (ANF.tempId * AST.semanticType * MemoryModel.rcShape) list ->
  AST.semanticType RcTypeFacts.TempMap.t ->
  ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
