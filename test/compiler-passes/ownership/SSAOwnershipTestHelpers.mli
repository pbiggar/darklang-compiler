(* SSAOwnershipTestHelpers.mli - Production SSA ownership transformation and queries. *)
open Dark_compiler

val transform :
  RcTypeFacts.typeContext -> ANF.functionDef -> (SSAANF.functionDef, string) result

val hasOperation : (ANF.cExpr -> bool) -> SSAANF.functionDef -> bool
val hasRefCountIncForTemp : ANF.tempId -> SSAANF.functionDef -> bool
val hasRefCountDecForTemp : ANF.tempId -> SSAANF.functionDef -> bool

val tryRefCountDecSourceTypeForTemp :
  ANF.tempId -> SSAANF.functionDef -> AST.semanticType option
