val lirPhysRegToARM64Reg : LIR.physReg -> Symbolic.reg
val lirPhysFPRegToARM64FReg : LIR.physFPReg -> Symbolic.fReg
val lirFRegToARM64FReg : LIR.fReg -> (Symbolic.fReg, string) result
val lirRegToARM64Reg : LIR.reg -> (Symbolic.reg, string) result
val virtualToFVirtual : LIR.reg -> LIR.fReg
val loadImmediate : Symbolic.reg -> int64 -> Symbolic.instr list
val loadStackSlot : Symbolic.reg -> int -> (Symbolic.instr list, string) result

val loadCliOperand :
  Symbolic.reg -> LIR.operand -> (Symbolic.instr list, string) result

val storeStackSlot : Symbolic.reg -> int -> (Symbolic.instr list, string) result
