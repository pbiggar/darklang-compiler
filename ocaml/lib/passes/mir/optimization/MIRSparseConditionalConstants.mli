(* SparseConditionalConstants.fs - Joint SSA constant and CFG-edge analysis. *)
val applySparseConditionalConstantPropagationWithCallResults : (AST.functionId -> MIR.operand option) -> MIR.cfg -> MIR.cfg * bool
val applySparseConditionalSimplification : MIR.cfg -> MIR.cfg * bool
val applySparseConditionalConstantPropagation : MIR.cfg -> MIR.cfg * bool
