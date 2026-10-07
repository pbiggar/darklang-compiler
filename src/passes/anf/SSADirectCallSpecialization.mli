(* SSADirectCallSpecialization.mli - Specialize direct calls on typed SSA blocks. *)
type specialization = {
  functions : SSAANF.functionDef list;
  cloneOrigins : AST.functionId FunctionIdMap.t;
}

val removeUnusedRematerializedValues :
  string FunctionIdMap.t -> SSAANF.functionDef -> SSAANF.functionDef

val specializeProgramWithFunctionNames :
  string FunctionIdMap.t -> SSAANF.functionDef list -> specialization

val reachableFrom :
  DirectCallFacts.FunctionSet.t ->
  SSAANF.functionDef list ->
  SSAANF.functionDef list
