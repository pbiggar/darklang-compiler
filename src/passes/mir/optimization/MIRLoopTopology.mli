(* MIRLoopTopology.mli - Compute dominators and natural-loop interfaces. *)
[@@@warning "-30"]
val getSuccessors : MIR.basicBlock -> MIR.label list
val buildSuccessors : MIR.cfg -> MIR.label list MIR.LabelMap.t
val cfgHasReachableCycle : MIR.cfg -> bool
val dominates : MIR.label -> SSA_Construction.dominators -> MIR.label -> MIR.label -> bool
type loopTopology = {loops : MIR.LabelSet.t MIR.LabelMap.t; predecessors : SSA_Construction.predecessors}
type dominatorTopology = {predecessors : SSA_Construction.predecessors; immediateDominators : SSA_Construction.dominators}
val buildDominatorTopology : MIR.cfg -> dominatorTopology
val tryBuildLoopTopologyWithDominators : MIR.cfg -> dominatorTopology -> loopTopology option
val tryBuildLoopTopology : MIR.cfg -> loopTopology option
val findNaturalLoops : MIR.cfg -> MIR.LabelSet.t MIR.LabelMap.t
