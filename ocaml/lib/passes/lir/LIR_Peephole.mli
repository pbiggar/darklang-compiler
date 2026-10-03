(* Faithful low-level instruction, branch and loop optimizations. *)
module RegMap : Map.S with type key = LIR.reg
val sameReg : LIR.reg -> LIR.reg -> bool
val sameFReg : LIR.fReg -> LIR.fReg -> bool
val getSuccessors : LIR.terminator -> LIR.label list
val buildPredecessors : LIR.cfg -> LIR.label list LIR.LabelMap.t
val buildSuccessors : LIR.cfg -> LIR.label list LIR.LabelMap.t
val isCallInstr : LIR.instr -> bool
val isPureLoopInstr : LIR.instr -> bool
val optimizeInstr : LIR.instr -> LIR.instr option
val retargetSeparatedDeadFAdds : LIR.instr list -> LIR.instr list
val optimizeInstrs : LIR.instr list -> LIR.instr list
val removeSelfMovesFromInstrs : LIR.instr list -> LIR.instr list
val removeSelfMovesFromFunction : LIR.functionDef -> LIR.functionDef
val sinkSeparatedAllocatedFAdds : LIR.instr list -> LIR.instr list
val removeRedundantFloatingCopyBackMoves : LIR.instr list -> LIR.instr list
val sinkImmediateCounterUpdate : LIR.instr list -> LIR.instr list option
val removePostAllocationMovesFromFunction : LIR.functionDef -> LIR.functionDef
val optimizeAllocatedCounterUpdates : LIR.functionDef -> LIR.functionDef
val isRegUsedInInstrs : LIR.reg -> LIR.instr list -> bool
type mulConstantPattern = PowerOfTwoPlusOne | PowerOfTwoMinusOne
val tryMulConstantPattern : int64 -> (int * mulConstantPattern) option
val tryMulByConstant : LIR.instr list -> LIR.instr list
val tryFuseMulAdd : LIR.instr list -> LIR.instr list
val tryFuseMulSub : LIR.instr list -> LIR.instr list
val tryFuseFloatMultiplyAdd : LIR.instr list -> LIR.instr list * bool
val formSelectDiamonds : LIR.cfg -> LIR.cfg * bool
val tryFuseCondBranch : int RegMap.t -> LIR.instr list -> LIR.terminator -> (LIR.instr list * LIR.terminator) option
val tryFuseBooleanNotBranch : int RegMap.t -> LIR.instr list -> LIR.terminator -> (LIR.instr list * LIR.terminator) option
val isPowerOf2 : int64 -> bool
val bitPosition : int64 -> int
val tryFuseAndBitBranch : LIR.instr list -> LIR.terminator -> (LIR.instr list * LIR.terminator) option
val tryFuseCmpZeroBranch : LIR.instr list -> LIR.terminator -> (LIR.instr list * LIR.terminator) option
val applyAndBitBranchFusion : LIR.instr list -> LIR.terminator -> LIR.instr list * LIR.terminator
val optimizeBlock : LIR.basicBlock -> LIR.basicBlock * bool
val optimizeCFG : LIR.cfg -> LIR.cfg
val optimizeFunction : LIR.functionDef -> LIR.functionDef
val optimizeFunctionFor : Platform.arch -> LIR.functionDef -> LIR.functionDef
val optimizeProgram : LIR.program -> LIR.program
