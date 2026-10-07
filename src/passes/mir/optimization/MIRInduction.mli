(* MIRInduction.mli - Reduce affine induction expressions in verified loop shapes. *)
val nextRegisterId : MIR.cfg -> int
val resolveLatchCopy : MIR.instr list -> MIR.vReg -> MIR.vReg
val isIncrementByOne : AST.semanticType -> MIR.vReg -> MIR.vReg -> MIR.instr -> bool
val applyAffineInductionStrengthReductionWithTopology : MIRLoopTopology.loopTopology -> MIR.cfg -> MIR.cfg * bool
val applyAffineInductionStrengthReduction : MIR.cfg -> MIR.cfg * bool
