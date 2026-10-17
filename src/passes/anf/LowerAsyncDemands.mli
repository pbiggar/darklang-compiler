(* LowerAsyncDemands.mli - Keep private source values pending until concrete inspection. *)
val lower :
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  TypeRegistries.functionIdRegistry ->
  ANF.functionDef list ->
  (ANF.functionDef list, string) result
