val generateDictRefCountIncHelper : unit -> X86_64.instr list

val generateDictRefCountDecHelper :
  string ->
  MemoryModel.rcReleasePlan ->
  MemoryModel.rcOperation option ->
  bool ->
  string option ->
  bool ->
  bool ->
  (int * MemoryModel.rcReleasePlan) option ->
  bool ->
  LIR.recordRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  X86_64.instr list

val generatePlannedDictRefCountDecHelper :
  string ->
  MemoryModel.rcReleasePlan ->
  bool ->
  LIR.recordRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  X86_64.instr list
