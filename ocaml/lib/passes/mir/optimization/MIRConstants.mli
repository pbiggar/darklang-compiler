(* Constants.fs - Fold typed scalar operations with native-width arithmetic semantics. *)
val truncateToType : int64 -> AST.semanticType -> int64
val truncateOperandToType : MIR.operand -> AST.semanticType -> MIR.operand
val euclideanMod : int64 -> int64 -> int64
val isReflexiveEqualityType : AST.semanticType -> bool
val isTotallyOrderedIntegerType : AST.semanticType -> bool
val isUnsignedIntegerType : AST.semanticType -> bool
val tryFoldBinOp : MIR.binOp -> MIR.operand -> MIR.operand -> AST.semanticType -> MIR.operand option
