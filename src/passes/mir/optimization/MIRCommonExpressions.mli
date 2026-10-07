(* MIRCommonExpressions.mli - Reuse and path-complete scalar expressions under effect constraints. *)
type exprKey =
 | BinExpr of MIR.binOp * MIR.operand * MIR.operand * AST.semanticType
 | UnaryExpr of MIR.unaryOp * MIR.operand
 | ScalarHeapLoadExpr of MIR.vReg * int * AST.semanticType
 | DirectCallExpr of AST.functionId * MIR.operand list * AST.semanticType
val isCommutative : MIR.binOp -> bool
val normalizeOperands : MIR.binOp -> MIR.operand -> MIR.operand -> MIR.operand * MIR.operand
val makeBinExprKey : MIR.binOp -> MIR.operand -> MIR.operand -> AST.semanticType -> exprKey
val makeUnaryExprKey : MIR.unaryOp -> MIR.operand -> exprKey
val makeScalarHeapLoadExprKey : MIR.vReg -> int -> AST.semanticType -> exprKey
val applyCSEWithEffectFreeCallsAndTopology : MIRLoopTopology.dominatorTopology option -> SpecializationIdentity.FunctionSet.t -> MIR.cfg -> MIR.cfg * bool * MIRLoopTopology.dominatorTopology
val applyCSEWithEffectFreeCalls : SpecializationIdentity.FunctionSet.t -> MIR.cfg -> MIR.cfg * bool
val applyCSE : MIR.cfg -> MIR.cfg * bool
