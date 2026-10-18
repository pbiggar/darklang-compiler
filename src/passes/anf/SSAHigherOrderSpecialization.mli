(* SSAHigherOrderSpecialization.mli - Specialize known callable arguments on typed SSA blocks. *)
type specialization = {
  functions : SSAANF.functionDef list;
  cloneOrigins : AST.functionId FunctionIdMap.t;
}

type purpose =
  | Optimization
  | AsyncInterfaces of SpecializationIdentity.FunctionSet.t

val specializeProgramWithExternalFunctionsAndNames :
  ?purpose:purpose ->
  AST.functionId StringOrder.Map.t ->
  int64 ->
  SSAANF.functionDef list ->
  SSAANF.functionDef list ->
  specialization
