(* Unrolling.fs - Unroll bounded scalar counted-loop shapes. *)
val isScalarValueType : AST.semanticType -> bool
val applyCountedLoopUnrollingWithTopology : MIRLoopTopology.loopTopology -> MIR.cfg -> MIR.cfg * bool
val applyCountedLoopUnrolling : MIR.cfg -> MIR.cfg * bool
