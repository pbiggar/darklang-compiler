(* RefCountInsertion.mli - Pre-SSA ownership proofs and legacy structured RC test support. *)
val verifyOwnershipContracts :
  RcTypeFacts.typeContext ->
  OwnedIR.callSignature FunctionIdMap.t ->
  ANF.program ->
  (unit, string) result

val ownedDictionaryFrontierParams :
  ANF.functionDef -> RcReturnAnalysis.TempSet.t

val insertRCInFunction :
  RcTypeFacts.typeContext ->
  ANF.functionDef ->
  ANF.varGen ->
  ANF.functionDef * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t

val verifyJoinInterfaces :
  RcTypeFacts.typeContext -> ANF.program -> (unit, string) result
