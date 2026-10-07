val callerSavedRegs : LIR.physReg list
val calleeSavedRegsFor : Platform.arch -> LIR.physReg list
val isNonTailCall : LIR.instr -> bool
val hasNonTailCalls : LIR.basicBlock array -> bool

val getAllocatableRegs :
  Platform.arch -> LIR.basicBlock array -> LIR.physReg list
