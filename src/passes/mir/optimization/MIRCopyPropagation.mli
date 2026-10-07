(* MIRCopyPropagation.mli - Resolve and propagate MIR copy equivalences. *)
type copyMap = MIR.operand MIR.VRegMap.t
val buildCopyMap : MIR.cfg -> copyMap
val resolveCopy : copyMap -> MIR.operand -> MIR.operand
val resolveCopyMap : copyMap -> copyMap
val propagateCopyOperand : copyMap -> MIR.operand -> MIR.operand
val propagateCopyInstr : copyMap -> MIR.instr -> MIR.instr
val propagateCopyTerminator : copyMap -> MIR.terminator -> MIR.terminator
