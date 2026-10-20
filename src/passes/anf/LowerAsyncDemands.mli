(* LowerAsyncDemands.mli - Lower pending source values and coordinated native call interfaces. *)
type lowered = {
  functions : ANF.functionDef list;
  nextFunctionOrdinal : int64;
  sumShapes : MemoryModel.rcSumShapeRegistry;
}

type failure =
  | Diagnostic of string
  | MissingRuntimeSpecializations of
      SpecializationIdentity.SpecSet.t * MemoryModel.rcSumShapeRegistry
  | MissingSourceSpecializations of
      SpecializationIdentity.SpecSet.t * MemoryModel.rcSumShapeRegistry

val lower :
  SpecializationIdentity.specKey StringOrder.Map.t ->
  TypeRegistries.functionRegistry ->
  TypeRegistries.functionNameRegistry ->
  TypeRegistries.functionIdRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  int64 ->
  ANF.functionDef StringOrder.Map.t ->
  ANF.functionDef list ->
  (lowered, failure) result
