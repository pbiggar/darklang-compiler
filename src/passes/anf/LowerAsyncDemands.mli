(* LowerAsyncDemands.mli - Lower pending source values and coordinated native call interfaces. *)
type lowered = { functions : ANF.functionDef list; nextFunctionOrdinal : int64 }

type failure =
  | Diagnostic of string
  | MissingRuntimeSpecializations of SpecializationIdentity.SpecSet.t

val lower :
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  TypeRegistries.functionIdRegistry ->
  int64 ->
  ANF.functionDef StringOrder.Map.t ->
  ANF.functionDef list ->
  (lowered, failure) result
