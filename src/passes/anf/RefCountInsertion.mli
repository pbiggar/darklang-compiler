(* RefCountInsertion.mli - Orchestrate function RC elaboration and verify complete type and join interfaces. *)
val verifyOwnershipContracts : RcTypeFacts.typeContext -> OwnedIR.callSignature FunctionIdMap.t -> ANF.program -> (unit, string) result
val ownedDictionaryFrontierParams : ANF.functionDef -> RcReturnAnalysis.TempSet.t
val insertRCInFunction : RcTypeFacts.typeContext -> ANF.functionDef -> ANF.varGen -> ANF.functionDef * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val collectMissingTempIdsInExpr : ANF.typeMap -> ANF.aExpr -> ANF.tempId list -> ANF.tempId list
val collectMissingTempIdsInFunction : ANF.typeMap -> ANF.functionDef -> ANF.tempId list -> ANF.tempId list
val verifyTypeMapCompleteness : ANF.program -> ANF.typeMap -> ANF.tempId list
val verifyJoinInterfaces : RcTypeFacts.typeContext -> ANF.program -> (unit, string) result
val insertRCInProgram : AST_to_ANF.conversionResult -> (ANF.program * ANF.typeMap, string) result
val insertRCInProgramWithTrace : (string -> float -> unit) option -> AST_to_ANF.conversionResult -> (ANF.program * ANF.typeMap, string) result
