(* Cleanup.fs - Plan retain/release placement and preserve cleanup across tail calls. *)
type returnDec = ANF.tempId * AST.semanticType * MemoryModel.rcShape * MemoryModel.rcKind option * MemoryModel.rcMetadata option * bool
type internalOwnedParamKind = ReturnedAccumulator | NonEscapingLoopState
type ownedParamDec = {paramIndex : int; releaseOnTerminalReturn : bool; dec : returnDec}
type recordReuseCleanup = {descriptor : ANF.recordDescriptor; source : ANF.tempId; fields : (int * AST.semanticType * MemoryModel.rcShape) list}
type letFrame = {tempId : ANF.tempId; cExpr : ANF.cExpr; allocationIncTargets : (ANF.tempId * AST.semanticType * MemoryModel.rcShape) list; recordReuseCleanup : recordReuseCleanup option; transferableOwnership : returnDec option; returnInc : (AST.semanticType * MemoryModel.rcShape) option; branchDec : returnDec option}
val createReturnDec : RcTypeFacts.typeContext -> ANF.tempId -> AST.semanticType -> MemoryModel.rcShape -> MemoryModel.rcKind option -> returnDec
val retainExprForShape : RcTypeFacts.typeContext -> ANF.tempId -> AST.semanticType -> MemoryModel.rcShape -> ANF.cExpr
val releaseExprForShape : ANF.tempId -> AST.semanticType -> MemoryModel.rcShape -> MemoryModel.rcKind option -> MemoryModel.rcMetadata option -> bool -> ANF.cExpr
val functionParamReturnTransfersOwnedAccumulator : RcTypeFacts.typeContext -> AST.functionId -> int -> AST.semanticType -> bool
val internalOwnedTailParamKind : ANF.functionDef -> int -> ANF.typedParam -> internalOwnedParamKind option
val internalOwnedTailParamHasSafeReplacements : ANF.functionDef -> int -> RcReturnAnalysis.TempSet.t -> bool
val insertParamIncsAtReturn : RcTypeFacts.typeContext -> (ANF.tempId * AST.semanticType * MemoryModel.rcShape) list -> RcReturnAnalysis.TempSet.t -> ANF.aExpr -> ANF.varGen -> AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val insertReturnDecs : returnDec list -> ANF.aExpr -> ANF.varGen -> AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val applyLetFrame : RcTypeFacts.typeContext -> letFrame -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val applyLetFrames : RcTypeFacts.typeContext -> letFrame list -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val isTempUsedAsSelfTailCallArg : RcTypeFacts.typeContext -> AST.functionId -> ANF.tempId -> RcReturnAnalysis.returnAnnotatedExpr -> bool
val moveDecsBeforeNonSelfTailCalls : AST.functionId -> ANF.aExpr -> ANF.aExpr
val insertOwnedAccumulatorDecsBeforeSelfTailCalls : RcTypeFacts.typeContext -> AST.functionId -> ownedParamDec list -> ANF.aExpr -> ANF.varGen -> AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
val requiredFunctionCleanups : RcTypeFacts.typeContext -> AST.functionId -> ANF.aExpr -> bool * bool
val insertClosureMapSourceRetainsBeforeHelperCalls : RcTypeFacts.typeContext -> AST.functionId -> ANF.aExpr -> ANF.varGen -> AST.semanticType RcTypeFacts.TempMap.t -> ANF.aExpr * ANF.varGen * AST.semanticType RcTypeFacts.TempMap.t
