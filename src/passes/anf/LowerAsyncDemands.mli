(* LowerAsyncDemands.mli - Lower pending source values and coordinated native call interfaces. *)
type lowered = { functions : ANF.functionDef list; nextFunctionOrdinal : int64 }

val lower :
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  TypeRegistries.functionIdRegistry ->
  int64 ->
  ANF.functionDef StringOrder.Map.t ->
  ANF.functionDef list ->
  (lowered, string) result
