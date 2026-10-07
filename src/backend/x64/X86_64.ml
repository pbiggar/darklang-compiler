(*
   X86_64.ml - x86-64 Instruction Types
   Defines x86-64 instruction and register types.
   x86-64 is a CISC architecture with variable-length instructions (1-15 bytes).
   These types represent x86-64 assembly instructions that will be encoded
   to machine code by the x86_64 encoding pass.
   Register conventions (System V AMD64 ABI - Linux):
   - RDI, RSI, RDX, RCX, R8, R9: Integer argument registers
   - XMM0-XMM7: Floating-point argument registers
   - RAX: Return value
   - RBX, RBP, R12-R15: Callee-saved
   - RSP: Stack pointer
   Syscall conventions (Linux):
   - RAX: Syscall number
   - RDI, RSI, RDX, R10, R8, R9: Syscall arguments
   - Invoked via SYSCALL instruction
   x86-64 general-purpose registers (64-bit)
*)
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

(*
   x86-64 SSE/AVX floating-point registers (128-bit, used as 64-bit double)
*)
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

(*
   Comparison conditions (for SETcc/Jcc)
   Equal (ZF=1)
   Not equal (ZF=0)
   Less than (signed: SF!=OF)
   Greater than (signed: ZF=0 and SF=OF)
   Less than or equal (signed: ZF=1 or SF!=OF)
   Greater than or equal (signed: SF=OF)
   Unsigned/float conditions (for use after UCOMISD):
   Below (CF=1) — float less than
   Above (CF=0 and ZF=0) — float greater than
   Below or equal (CF=1 or ZF=1) — float less or equal
   Above or equal (CF=0) — float greater or equal
   Parity set (PF=1) — unordered (NaN)
   Parity not set (PF=0) — ordered (not NaN)
*)
type condition = EQ | NE | LT | GT | LE | GE | B | A | BE | AE | P | NP

(*
   Operand size for instructions that need explicit sizing
   8-bit
   16-bit
   32-bit
   64-bit
*)
type size = Byte | Word | DWord | QWord

(*
   x86-64 instruction types
   Data movement
   MOV reg, imm64 (movabs for 64-bit)
   MOV reg, imm32 (sign-extended to 64-bit)
   MOV reg, reg
   MOV reg, [base + offset]
   MOV [base + offset], reg
   MOV r32, r32 (zero-extends to 64-bit)
   MOVZX reg, reg8 (zero-extend byte)
   MOVZX reg, reg16 (zero-extend word)
   MOVSX reg, reg8 (sign-extend byte)
   MOVSX reg, reg16 (sign-extend word)
   MOVSXD reg64, reg32 (sign-extend dword)
   LEA reg, [base + offset]
   LEA reg, [RIP + label]
   Stack operations
   Arithmetic
   Signed multiply: dest = dest * src
   Signed multiply: dest = src * imm
   Signed divide: RDX:RAX / src → RAX=quot, RDX=rem
   Unsigned divide: RDX:RAX / src → RAX=quot, RDX=rem
   Negate: dest = -dest
   Bitwise NOT: dest = ~dest
   Sign-extend RAX into RDX:RAX (before IDIV)
   XOR for zeroing or bitwise xor
   Comparison and conditional
   TEST reg, reg (AND without storing, sets flags)
   Set byte to 0/1 based on condition
   Conditional register move
   Bitwise
   Shift left by immediate
   Logical shift right by immediate
   Arithmetic shift right by immediate
   Shift left by CL register
   Logical shift right by CL register
   Arithmetic shift right by CL register
   Byte-level memory
   MOV [base + offset], src8
   MOVZX dest, byte [base + offset]
   Control flow
   CALL rel32
   CALL reg (indirect)
   JMP rel32
   JMP reg (indirect, for tail calls)
   Conditional jump to label
   Linux syscall
   Pseudo-instruction: marks a label position
   Floating-point (SSE2)
   Load double from [base + offset]
   Store double to [base + offset]
   Move between XMM registers
   Add double
   Subtract double
   Multiply double
   Divide double
   XOR packed double (for negation/zeroing)
   Square root double
   Compare doubles (sets flags)
   Convert int64 to double
   Convert double to int64 (truncate)
   Move 64 bits from XMM to GP
   Move 64 bits from GP to XMM
*)
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

(*
   Machine code (variable-length byte sequence for one instruction)
*)
type machineCode = bytes

(*
   Literal values are carried in symbolic RIP-relative labels until ELF layout.
   The payload is kept verbatim: these labels are internal map keys, not names
   passed through an assembler.
*)
let stringLiteralLabelPrefix = "__dark_string_literal_data:"
let stringLiteralLabel value = stringLiteralLabelPrefix ^ value

let tryStringLiteralValue label =
  if String.starts_with ~prefix:stringLiteralLabelPrefix label then
    Some
      (String.sub label
         (String.length stringLiteralLabelPrefix)
         (String.length label - String.length stringLiteralLabelPrefix))
  else None
