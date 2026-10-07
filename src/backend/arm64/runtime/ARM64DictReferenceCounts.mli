val generateDictRefCountIncHelper : unit -> Symbolic.instr list

val generateDictRefCountDecHelper :
  string ->
  MemoryModel.rcReleasePlan ->
  bool ->
  bool ->
  string option ->
  bool ->
  bool ->
  (int * MemoryModel.rcReleasePlan) option ->
  bool ->
  bool ->
  bool ->
  ARM64CodeGenTypes.codeGenContext ->
  Symbolic.instr list

val generatePlannedDictRefCountDecHelper :
  string ->
  MemoryModel.rcReleasePlan ->
  ARM64CodeGenTypes.codeGenContext ->
  Symbolic.instr list
