(* RcBindingTypeInference.mli - Type recovery shared by pre-RC lowering. *)
val inferBindingType :
  RcTypeFacts.typeContext ->
  ANF.tempId ->
  ANF.cExpr ->
  RcReturnAnalysis.returnAnnotatedExpr ->
  AST.semanticType

