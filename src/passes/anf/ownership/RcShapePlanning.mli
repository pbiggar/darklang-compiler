(* RcShapePlanning.mli - Select canonical representation shapes for ANF ownership decisions. *)
val rcShapeForType :
  RcTypeFacts.typeContext -> AST.semanticType -> MemoryModel.rcShape

val rcMetadataForTypeAndShape :
  RcTypeFacts.typeContext ->
  AST.semanticType ->
  MemoryModel.rcShape ->
  MemoryModel.rcMetadata

val shapeNeedsManagedAliasRootPreservation :
  RcTypeFacts.typeContext -> AST.semanticType -> bool

val bindingNeedsShapeAutomaticDec :
  RcTypeFacts.typeContext ->
  ANF.cExpr ->
  AST.semanticType ->
  MemoryModel.rcShape ->
  bool

val cexprProducesNonRcSentinel : ANF.cExpr -> bool
