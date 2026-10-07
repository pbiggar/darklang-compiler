val dataLabel : string -> Symbolic.labelRef
val stringDataLabel : string -> Symbolic.labelRef
val floatDataLabel : float -> Symbolic.labelRef
val codeLabel : string -> Symbolic.labelRef
val runtimeInstrs : ARM64.instr list -> Symbolic.instr list
val utf8Len : string -> int
val loadStringLiteralPointer : Symbolic.reg -> string -> Symbolic.instr list
val generateRuntimeErrorHelper : ARM64.targetConfig -> Symbolic.instr list
val preparedHeapOverflowTrapBody : ARM64.targetConfig -> Symbolic.instr list

val generateHeapOverflowTrapBlock :
  Symbolic.instr list -> string -> Symbolic.instr list

val withHeapBoundsCheck :
  string -> Symbolic.instr list -> Symbolic.instr list -> Symbolic.instr list

val checkedBumpAllocReg :
  string -> Symbolic.reg -> Symbolic.reg -> Symbolic.instr list
