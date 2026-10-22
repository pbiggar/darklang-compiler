(* AsyncCallableSpecialization.mli - Expose known source callbacks before transport inference. *)
type prepared = {
  functions : ANF.functionDef list;
  externalDefinitions : ANF.functionDef StringOrder.Map.t;
  nextFunctionOrdinal : int64;
}

val prepare :
  AST.functionId StringOrder.Map.t ->
  int64 ->
  (unit -> RcTypeFacts.typeContext) ->
  ANF.functionDef StringOrder.Map.t ->
  ANF.functionDef list ->
  (prepared, string) result
