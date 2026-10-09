val generateCalleeSavedSaves : LIR.physReg list -> Symbolic.instr list * int
val generateCalleeSavedRestores : LIR.physReg list -> Symbolic.instr list
val calleeSavedStackSpace : LIR.physReg list -> int
val floatCalleeSavedStackSpace : LIR.physFPReg list -> int

val generateFloatCalleeSavedSaves :
  LIR.physFPReg list -> int -> Symbolic.instr list

val generateFloatCalleeSavedRestores :
  LIR.physFPReg list -> int -> Symbolic.instr list

val generatePrologue :
  LIR.physReg list -> LIR.physFPReg list -> int -> Symbolic.instr list

val generateEpilogue :
  LIR.physReg list -> LIR.physFPReg list -> int -> Symbolic.instr list
