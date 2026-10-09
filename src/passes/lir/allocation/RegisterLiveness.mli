val getSuccessors : LIR.terminator -> LIR.label list

val computeGenKill :
  AllocationModel.vRegDomain ->
  LIR.basicBlock ->
  AllocationModel.bitSet * AllocationModel.bitSet

val computeFloatGenKill :
  AllocationModel.vRegDomain ->
  LIR.basicBlock ->
  AllocationModel.bitSet * AllocationModel.bitSet

val computeCombinedLivenessBitsFromFacts :
  AllocationModel.blockIndex ->
  AllocationModel.classifiedBlock array ->
  int list ->
  int list ->
  AllocationModel.vRegDomain
  * AllocationModel.blockLiveness array
  * AllocationModel.vRegDomain
  * AllocationModel.blockLiveness array

val computeLivenessBitsFromFacts :
  AllocationModel.blockIndex ->
  AllocationModel.classifiedBlock array ->
  int list ->
  AllocationModel.vRegDomain * AllocationModel.blockLiveness array

val computeFloatLivenessBitsFromFacts :
  AllocationModel.blockIndex ->
  AllocationModel.classifiedBlock array ->
  int list ->
  AllocationModel.vRegDomain * AllocationModel.blockLiveness array

val computeLivenessBits :
  LIR.cfg ->
  AllocationModel.vRegDomain
  * AllocationModel.blockIndex
  * AllocationModel.blockLiveness array

val computeFloatLivenessBits :
  LIR.cfg ->
  AllocationModel.vRegDomain
  * AllocationModel.blockIndex
  * AllocationModel.blockLiveness array

val computeSaveRegsPreparation :
  AllocationModel.vRegDomain ->
  AllocationModel.vRegDomain ->
  LIR.basicBlock ->
  AllocationModel.instrRegisterFacts array ->
  AllocationModel.bitSet ->
  AllocationModel.bitSet ->
  (AllocationModel.bitSet * AllocationModel.bitSet) list

val isEmptySaveRegs : LIR.instr -> bool
