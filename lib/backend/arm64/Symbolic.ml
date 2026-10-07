(*
   Symbolic.fs - Symbolic ARM64 Instruction Types
   Defines ARM64 instructions with explicit data label references so that
   string/float literals can stay symbolic until final emission.
   Reuse ARM64 register and condition types
   Re-export register values for convenience in codegen
*)
type reg = ARM64.reg = 
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
type fReg = ARM64.fReg = 
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
type condition = ARM64.condition = 
 | EQ
 | NE
 | LT
 | GT
 | LE
 | GE
 | LO
 | HI
 | LS
 | HS
(*
   Data references for late pool resolution
*)
type dataRef = 
 | StringLiteral of string
 | FloatLiteral of float
 | Named of string
(*
   Label reference (code vs data)
*)
type labelRef = 
 | CodeLabel of string
 | DataLabel of dataRef
(*
   ARM64 instruction types (symbolic label refs for data)
*)
type instr = 
 | MOVZ of reg * int * int
 | MOVN of reg * int * int
 | MOVK of reg * int * int
 | ADD_imm of reg * reg * int
 | ADD_reg of reg * reg * reg
 | ADD_shifted of reg * reg * reg * int
 | ADD_extended of reg * reg * reg * ARM64.extend
 | SUB_imm of reg * reg * int
 | SUB_imm12 of reg * reg * int
 | SUB_reg of reg * reg * reg
 | SUB_shifted of reg * reg * reg * int
 | SUB_extended of reg * reg * reg * ARM64.extend
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
 | ADRP of reg * labelRef
 | ADD_label of reg * reg * labelRef
 | ADR of reg * labelRef
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
 | FMOV_zero of fReg
 | FMOV_to_gp of reg * fReg
 | FMOV_from_gp of fReg * reg
 | CNT_8B of fReg * fReg
 | ADDV_8B of fReg * fReg
 | UMOV_byte of reg * fReg
 | SCVTF of fReg * reg
 | FCVTZS of reg * fReg
 | SXTB of reg * reg
 | SXTH of reg * reg
 | SXTW of reg * reg
 | UXTB of reg * reg
 | UXTH of reg * reg
 | UXTW of reg * reg
let coverageDataLabelName="_coverage_data"
let leakCounterLabelName="_leak_count"
let isNamedDataLabel name=name=coverageDataLabelName || name=leakCounterLabelName
(*
   Classify labels from concrete ARM64 instructions
*)
let classifyLabel name=if isNamedDataLabel name then DataLabel (Named name) else CodeLabel name
(*
   Convert concrete ARM64 instruction to symbolic (for runtime helpers)
*)
let ofARM64 = function
    | ARM64.MOVZ (dest, imm, shift) -> MOVZ (dest, imm, shift)
    | ARM64.MOVN (dest, imm, shift) -> MOVN (dest, imm, shift)
    | ARM64.MOVK (dest, imm, shift) -> MOVK (dest, imm, shift)
    | ARM64.ADD_imm (dest, src, imm) -> ADD_imm (dest, src, imm)
    | ARM64.ADD_reg (dest, src1, src2) -> ADD_reg (dest, src1, src2)
    | ARM64.ADD_shifted (dest, src1, src2, shift) -> ADD_shifted (dest, src1, src2, shift)
    | ARM64.ADD_extended (dest, src1, src2, extend) -> ADD_extended (dest, src1, src2, extend)
    | ARM64.SUB_imm (dest, src, imm) -> SUB_imm (dest, src, imm)
    | ARM64.SUB_imm12 (dest, src, imm) -> SUB_imm12 (dest, src, imm)
    | ARM64.SUB_reg (dest, src1, src2) -> SUB_reg (dest, src1, src2)
    | ARM64.SUB_shifted (dest, src1, src2, shift) -> SUB_shifted (dest, src1, src2, shift)
    | ARM64.SUB_extended (dest, src1, src2, extend) -> SUB_extended (dest, src1, src2, extend)
    | ARM64.SUBS_imm (dest, src, imm) -> SUBS_imm (dest, src, imm)
    | ARM64.MUL (dest, src1, src2) -> MUL (dest, src1, src2)
    | ARM64.SDIV (dest, src1, src2) -> SDIV (dest, src1, src2)
    | ARM64.UDIV (dest, src1, src2) -> UDIV (dest, src1, src2)
    | ARM64.MSUB (dest, src1, src2, src3) -> MSUB (dest, src1, src2, src3)
    | ARM64.MADD (dest, src1, src2, src3) -> MADD (dest, src1, src2, src3)
    | ARM64.CMP_imm (src, imm) -> CMP_imm (src, imm)
    | ARM64.CMP_reg (src1, src2) -> CMP_reg (src1, src2)
    | ARM64.CSET (dest, cond) -> CSET (dest, cond)
    | ARM64.CSEL (dest, whenTrue, whenFalse, cond) -> CSEL (dest, whenTrue, whenFalse, cond)
    | ARM64.AND_reg (dest, src1, src2) -> AND_reg (dest, src1, src2)
    | ARM64.BIC_reg (dest, src1, src2) -> BIC_reg (dest, src1, src2)
    | ARM64.AND_imm (dest, src, imm) -> AND_imm (dest, src, imm)
    | ARM64.ORR_reg (dest, src1, src2) -> ORR_reg (dest, src1, src2)
    | ARM64.EOR_reg (dest, src1, src2) -> EOR_reg (dest, src1, src2)
    | ARM64.LSL_reg (dest, src, shift) -> LSL_reg (dest, src, shift)
    | ARM64.LSR_reg (dest, src, shift) -> LSR_reg (dest, src, shift)
    | ARM64.ASR_reg (dest, src, shift) -> ASR_reg (dest, src, shift)
    | ARM64.LSL_imm (dest, src, shift) -> LSL_imm (dest, src, shift)
    | ARM64.LSR_imm (dest, src, shift) -> LSR_imm (dest, src, shift)
    | ARM64.ASR_imm (dest, src, shift) -> ASR_imm (dest, src, shift)
    | ARM64.MVN (dest, src) -> MVN (dest, src)
    | ARM64.MOV_reg (dest, src) -> MOV_reg (dest, src)
    | ARM64.STRB (src, addr, offset) -> STRB (src, addr, offset)
    | ARM64.LDRB (dest, baseAddr, index) -> LDRB (dest, baseAddr, index)
    | ARM64.LDRB_imm (dest, baseAddr, offset) -> LDRB_imm (dest, baseAddr, offset)
    | ARM64.STRB_reg (src, addr) -> STRB_reg (src, addr)
    | ARM64.STP (reg1, reg2, addr, offset) -> STP (reg1, reg2, addr, offset)
    | ARM64.STP_pre (reg1, reg2, addr, offset) -> STP_pre (reg1, reg2, addr, offset)
    | ARM64.LDP (reg1, reg2, addr, offset) -> LDP (reg1, reg2, addr, offset)
    | ARM64.LDP_post (reg1, reg2, addr, offset) -> LDP_post (reg1, reg2, addr, offset)
    | ARM64.STR (src, addr, offset) -> STR (src, addr, offset)
    | ARM64.LDR (dest, addr, offset) -> LDR (dest, addr, offset)
    | ARM64.STUR (src, addr, offset) -> STUR (src, addr, offset)
    | ARM64.LDUR (dest, addr, offset) -> LDUR (dest, addr, offset)
    | ARM64.BL label -> BL label
    | ARM64.BLR reg -> BLR reg
    | ARM64.BR reg -> BR reg
    | ARM64.CBZ (reg, label) -> CBZ (reg, label)
    | ARM64.CBNZ (reg, label) -> CBNZ (reg, label)
    | ARM64.B_label label -> B_label label
    | ARM64.B_cond_label (cond, label) -> B_cond_label (cond, label)
    | ARM64.CBZ_offset (reg, offset) -> CBZ_offset (reg, offset)
    | ARM64.CBNZ_offset (reg, offset) -> CBNZ_offset (reg, offset)
    | ARM64.TBZ (reg, bit, offset) -> TBZ (reg, bit, offset)
    | ARM64.TBNZ (reg, bit, offset) -> TBNZ (reg, bit, offset)
    | ARM64.TBZ_label (reg, bit, label) -> TBZ_label (reg, bit, label)
    | ARM64.TBNZ_label (reg, bit, label) -> TBNZ_label (reg, bit, label)
    | ARM64.B offset -> B offset
    | ARM64.B_cond (cond, offset) -> B_cond (cond, offset)
    | ARM64.NEG (dest, src) -> NEG (dest, src)
    | ARM64.RET -> RET
    | ARM64.SVC imm -> SVC imm
    | ARM64.Label name -> Label name
    | ARM64.ADRP (dest, label) -> ADRP (dest, classifyLabel label)
    | ARM64.ADD_label (dest, src, label) -> ADD_label (dest, src, classifyLabel label)
    | ARM64.ADR (dest, label) -> ADR (dest, classifyLabel label)
    | ARM64.LDR_fp (dest, addr, offset) -> LDR_fp (dest, addr, offset)
    | ARM64.STR_fp (src, addr, offset) -> STR_fp (src, addr, offset)
    | ARM64.STP_fp (freg1, freg2, addr, offset) -> STP_fp (freg1, freg2, addr, offset)
    | ARM64.LDP_fp (freg1, freg2, addr, offset) -> LDP_fp (freg1, freg2, addr, offset)
    | ARM64.FADD (dest, src1, src2) -> FADD (dest, src1, src2)
    | ARM64.FSUB (dest, src1, src2) -> FSUB (dest, src1, src2)
    | ARM64.FMUL (dest, src1, src2) -> FMUL (dest, src1, src2)
    | ARM64.FMADD (dest, src1, src2, addend) -> FMADD (dest, src1, src2, addend)
    | ARM64.FDIV (dest, src1, src2) -> FDIV (dest, src1, src2)
    | ARM64.FNEG (dest, src) -> FNEG (dest, src)
    | ARM64.FABS (dest, src) -> FABS (dest, src)
    | ARM64.FSQRT (dest, src) -> FSQRT (dest, src)
    | ARM64.FCMP (src1, src2) -> FCMP (src1, src2)
    | ARM64.FMOV_reg (dest, src) -> FMOV_reg (dest, src)
    | ARM64.FMOV_imm (dest, value) -> FMOV_imm (dest, value)
    | ARM64.FMOV_to_gp (dest, src) -> FMOV_to_gp (dest, src)
    | ARM64.FMOV_from_gp (dest, src) -> FMOV_from_gp (dest, src)
    | ARM64.SCVTF (dest, src) -> SCVTF (dest, src)
    | ARM64.FCVTZS (dest, src) -> FCVTZS (dest, src)
    | ARM64.SXTB (dest, src) -> SXTB (dest, src)
    | ARM64.SXTH (dest, src) -> SXTH (dest, src)
    | ARM64.SXTW (dest, src) -> SXTW (dest, src)
    | ARM64.UXTB (dest, src) -> UXTB (dest, src)
    | ARM64.UXTH (dest, src) -> UXTH (dest, src)
    | ARM64.UXTW (dest, src) -> UXTW (dest, src)

(*
   Convert concrete ARM64 instruction list to symbolic
*)
let ofARM64List instrs=List.map ofARM64 instrs
