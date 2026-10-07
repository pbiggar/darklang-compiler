(* CallGraphSchedule.mli - Order exact MIR function nodes before their callers. *)
type component = {nodeIndices : int list; sccs : MIR.functionDef list list; functions : MIR.functionDef list}
val directCallees : MIR.functionDef -> SpecializationIdentity.FunctionSet.t
val calleeFirst : MIR.functionDef list -> component list
