type reg = 
 | RAX
 | RBX
 | RCX
 | RDX
 | RSI
 | RDI
 | RBP
 | RSP
 | R8
 | R9
 | R10
 | R11
 | R12
 | R13
 | R14
 | R15
type fReg = 
 | XMM0
 | XMM1
 | XMM2
 | XMM3
 | XMM4
 | XMM5
 | XMM6
 | XMM7
 | XMM8
 | XMM9
 | XMM10
 | XMM11
 | XMM12
 | XMM13
 | XMM14
 | XMM15
type condition = 
 | EQ
 | NE
 | LT
 | GT
 | LE
 | GE
 | B
 | A
 | BE
 | AE
 | P
 | NP
type size = 
 | Byte
 | Word
 | DWord
 | QWord
type instr = 
 | MOV_imm of reg * int64
 | MOV_imm32 of reg * int32
 | MOV_reg of reg * reg
 | MOV_load of reg * reg * int32
 | MOV_store of reg * int32 * reg
 | MOV_reg32 of reg * reg
 | MOVZX_byte of reg * reg
 | MOVZX_word of reg * reg
 | MOVSX_byte of reg * reg
 | MOVSX_word of reg * reg
 | MOVSXD of reg * reg
 | LEA of reg * reg * int32
 | LEA_index of reg * reg * reg * int * int32
 | LEA_rip of reg * string
 | PUSH of reg
 | POP of reg
 | ADD_imm of reg * int32
 | ADD_reg of reg * reg
 | ADD_load of reg * reg * int32
 | SUB_imm of reg * int32
 | SUB_reg of reg * reg
 | SUB_load of reg * reg * int32
 | IMUL_reg of reg * reg
 | IMUL_imm of reg * reg * int32
 | IDIV of reg
 | DIV of reg
 | NEG of reg
 | NOT of reg
 | CQO
 | XOR_reg of reg * reg
 | CMP_imm of reg * int32
 | CMP_reg of reg * reg
 | TEST_reg of reg * reg
 | SETcc of condition * reg
 | CMOVcc of condition * reg * reg
 | AND_imm of reg * int32
 | AND_reg of reg * reg
 | OR_reg of reg * reg
 | SHL_imm of reg * int
 | SHR_imm of reg * int
 | SAR_imm of reg * int
 | SHL_cl of reg
 | SHR_cl of reg
 | SAR_cl of reg
 | MOV_store_byte of reg * int32 * reg
 | MOV_load_byte of reg * reg * int32
 | CALL of string
 | CALL_reg of reg
 | JMP of string
 | JMP_reg of reg
 | Jcc of condition * string
 | RET
 | SYSCALL
 | Label of string
 | MOVSD_load of fReg * reg * int32
 | MOVSD_store of reg * int32 * fReg
 | MOVSD_reg of fReg * fReg
 | ADDSD of fReg * fReg
 | SUBSD of fReg * fReg
 | MULSD of fReg * fReg
 | DIVSD of fReg * fReg
 | XORPD of fReg * fReg
 | SQRTSD of fReg * fReg
 | UCOMISD of fReg * fReg
 | CVTSI2SD of fReg * reg
 | CVTTSD2SI of reg * fReg
 | MOVQ_to_gp of reg * fReg
 | MOVQ_from_gp of fReg * reg
type machineCode = bytes
val stringLiteralLabelPrefix : string
val stringLiteralLabel : string -> string
val tryStringLiteralValue : string -> string option
