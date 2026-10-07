(* TailCallDetection.mli - Tail Call Detection Pass *)
module TempMap = ANFConstants.TempMap
module TempSet = ANFEffects.TempSet
val isRefCountDec : ANF.cExpr -> bool
val isReturnOf : ANF.tempId -> ANF.aExpr -> bool
val convertToTailCall : ANF.cExpr -> ANF.cExpr
val canonicalTempId : ANF.tempId TempMap.t -> ANF.tempId -> ANF.tempId
val extendAliasRoots : ANF.tempId TempMap.t -> ANF.tempId -> ANF.cExpr -> ANF.tempId TempMap.t
val extendBorrowRoots : ANF.tempId TempMap.t -> TempSet.t TempMap.t -> ANF.tempId -> ANF.cExpr -> TempSet.t TempMap.t
val leadingRetainedParams : TempSet.t -> ANF.aExpr -> TempSet.t
val isCallExpr : ANF.cExpr -> bool
val detectTailCalls : AST.functionId -> (AST.functionId -> bool) -> ANF.typedParam list -> TempSet.t -> TempSet.t -> bool -> ANF.tempId TempMap.t -> TempSet.t TempMap.t -> TempSet.t -> ANF.aExpr -> ANF.aExpr
val isEligibleFunctionName : string -> bool
val detectTailCallsInFunction : ANF.functionDef -> ANF.functionDef
val detectTailCallsInProgram : ANF.program -> ANF.program
val detectTailCallsInProgramWithRecursion : AST.loweredRecursiveMember FunctionIdMap.t -> ANF.program -> ANF.program
