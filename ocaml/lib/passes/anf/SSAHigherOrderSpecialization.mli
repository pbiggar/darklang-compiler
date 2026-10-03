(* SSAHigherOrderSpecialization.fs - Specialize known callable arguments on typed SSA blocks. *)
type specialization = {functions : SSAANF.functionDef list; cloneOrigins : AST.functionId FunctionIdMap.t}
val specializeProgramWithExternalFunctionsAndNames : AST.functionId StringOrder.Map.t -> int64 -> SSAANF.functionDef list -> SSAANF.functionDef list -> specialization
