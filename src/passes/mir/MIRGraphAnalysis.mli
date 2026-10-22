(* MIRGraphAnalysis.mli - Analyze MIR control-flow edges, dominance and register uses. *)
type predecessors = MIR.label list MIR.LabelMap.t
type dominators = MIR.label MIR.LabelMap.t

val getSuccessors : MIR.basicBlock -> MIR.label list
val buildPredecessors : MIR.cfg -> predecessors
val computeDominators : MIR.cfg -> predecessors -> dominators
val buildDomTree : dominators -> MIR.label list MIR.LabelMap.t
val getBlockDefs : MIR.basicBlock -> MIR.VRegSet.t
val getBlockUses : MIR.basicBlock -> MIR.VRegSet.t
