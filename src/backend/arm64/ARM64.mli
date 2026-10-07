type reg =
  | X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | SP

type fReg =
  | D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15
  | D16
  | D17
  | D18
  | D19
  | D20
  | D21
  | D22
  | D23
  | D24
  | D25
  | D26
  | D27
  | D28
  | D29
  | D30
  | D31

type condition = EQ | NE | LT | GT | LE | GE | LO | HI | LS | HS

type extend =
  | ExtendUXTB
  | ExtendUXTH
  | ExtendUXTW
  | ExtendSXTB
  | ExtendSXTH
  | ExtendSXTW

type instr =
  | MOVZ of reg * int * int
  | MOVN of reg * int * int
  | MOVK of reg * int * int
  | ADD_imm of reg * reg * int
  | ADD_reg of reg * reg * reg
  | ADD_shifted of reg * reg * reg * int
  | ADD_extended of reg * reg * reg * extend
  | SUB_imm of reg * reg * int
  | SUB_imm12 of reg * reg * int
  | SUB_reg of reg * reg * reg
  | SUB_shifted of reg * reg * reg * int
  | SUB_extended of reg * reg * reg * extend
  | SUBS_imm of reg * reg * int
  | MUL of reg * reg * reg
  | SDIV of reg * reg * reg
  | UDIV of reg * reg * reg
  | MSUB of reg * reg * reg * reg
  | MADD of reg * reg * reg * reg
  | CMP_imm of reg * int
  | CMP_reg of reg * reg
  | CSET of reg * condition
  | CSEL of reg * reg * reg * condition
  | AND_reg of reg * reg * reg
  | BIC_reg of reg * reg * reg
  | AND_imm of reg * reg * int64
  | ORR_reg of reg * reg * reg
  | EOR_reg of reg * reg * reg
  | LSL_reg of reg * reg * reg
  | LSR_reg of reg * reg * reg
  | ASR_reg of reg * reg * reg
  | LSL_imm of reg * reg * int
  | LSR_imm of reg * reg * int
  | ASR_imm of reg * reg * int
  | MVN of reg * reg
  | MOV_reg of reg * reg
  | STRB of reg * reg * int
  | LDRB of reg * reg * reg
  | LDRB_imm of reg * reg * int
  | STRB_reg of reg * reg
  | STP of reg * reg * reg * int
  | STP_pre of reg * reg * reg * int
  | LDP of reg * reg * reg * int
  | LDP_post of reg * reg * reg * int
  | STR of reg * reg * int
  | LDR of reg * reg * int
  | STUR of reg * reg * int
  | LDUR of reg * reg * int
  | BL of string
  | BLR of reg
  | BR of reg
  | CBZ of reg * string
  | CBNZ of reg * string
  | B_label of string
  | B_cond_label of condition * string
  | CBZ_offset of reg * int
  | CBNZ_offset of reg * int
  | TBZ of reg * int * int
  | TBNZ of reg * int * int
  | TBZ_label of reg * int * string
  | TBNZ_label of reg * int * string
  | B of int
  | B_cond of condition * int
  | NEG of reg * reg
  | RET
  | SVC of int
  | Label of string
  | ADRP of reg * string
  | ADD_label of reg * reg * string
  | ADR of reg * string
  | LDR_fp of fReg * reg * int
  | STR_fp of fReg * reg * int
  | STP_fp of fReg * fReg * reg * int
  | LDP_fp of fReg * fReg * reg * int
  | FADD of fReg * fReg * fReg
  | FSUB of fReg * fReg * fReg
  | FMUL of fReg * fReg * fReg
  | FMADD of fReg * fReg * fReg * fReg
  | FDIV of fReg * fReg * fReg
  | FNEG of fReg * fReg
  | FABS of fReg * fReg
  | FSQRT of fReg * fReg
  | FCMP of fReg * fReg
  | FMOV_reg of fReg * fReg
  | FMOV_imm of fReg * float
  | FMOV_to_gp of reg * fReg
  | FMOV_from_gp of fReg * reg
  | SCVTF of fReg * reg
  | FCVTZS of reg * fReg
  | SXTB of reg * reg
  | SXTH of reg * reg
  | SXTW of reg * reg
  | UXTB of reg * reg
  | UXTH of reg * reg
  | UXTW of reg * reg

type machineCode = int32

type syscallConfig = {
  numbers : Platform.syscallNumbers;
  svcImmediate : int;
  syscallRegister : reg;
}

val tryEncodeFmovFloatImmediate : float -> int32 option
val syscallConfigFor : Platform.os -> syscallConfig

type targetConfig

val targetConfigFor : Platform.arm64Target -> targetConfig
val targetOS : targetConfig -> Platform.os
val targetSyscalls : targetConfig -> syscallConfig
