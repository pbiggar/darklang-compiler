(* ControlFlow.fs - Simplify MIR branches, joins, and unreachable blocks. *)
val mergeLinearBlocks : MIR.cfg -> MIR.cfg * bool
val simplifyEmptyBlocks : MIR.cfg -> MIR.cfg * bool
val simplifyRetPhiJoins : MIR.cfg -> MIR.cfg * bool
