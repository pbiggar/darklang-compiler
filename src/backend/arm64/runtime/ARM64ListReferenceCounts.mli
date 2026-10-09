type listRefCountDecHelperSpec = {
  label : string;
  releaseLeafListPayload : bool;
  releaseLeafDictPayload : bool;
  releaseLeafClosurePayload : bool;
}

val generateListRefCountIncHelper : unit -> Symbolic.instr list
val listRefCountDecHelperSpecs : listRefCountDecHelperSpec list

val generateNeededListRefCountDecHelpers :
  ARM64CodeGenTypes.codeGenContext ->
  StringOrder.Set.t ->
  (int * MemoryModel.rcReleasePlan) StringOrder.Map.t ->
  Symbolic.instr list
