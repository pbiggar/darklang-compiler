(* MIRLoopInvariantMotion.mli - Build preheaders and hoist proven loop-invariant operations. *)
val isHoistableInstr : MIR.instr -> bool
val applyLoopInvariantCodeMotionWithEffectFreeCalls : SpecializationIdentity.FunctionSet.t -> MIRLoopTopology.loopTopology -> MIR.cfg -> MIR.cfg * bool * MIRLoopTopology.loopTopology
val applyLoopInvariantCodeMotion : MIR.cfg -> MIR.cfg * bool
