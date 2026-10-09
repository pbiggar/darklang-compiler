(*
   ARM64_Encoding.ml - ARM64 Machine Code Encoding (Pass 7)
   Encodes ARM64 instructions to 32-bit machine code per ARMv8 specification.
   Encoding algorithm:
   - Two-pass encoding for label-based branches:
   Pass 1: Compute label positions (byte offsets)
   Pass 2: Encode with computed branch offsets
   - Encodes registers as 5-bit fields
   - Packs immediates into instruction-specific bit positions
   - Combines opcode bits, operand fields into 32-bit words
   - Each instruction has unique bit layout defined by ARMv8
   Example:
   MOVZ X0, #42, LSL #0  →  0xD2800540
   ADD X1, X0, #5        →  0x91001401
   RET                   →  0xD65F03C0
*)
[@@@warning "-4"]

let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b = Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let uint32 = Int32.of_int
let ( lsl ) value bits = Int32.shift_left value (Stdlib.( land ) bits 31)

let ( lsr ) value bits =
  Int32.shift_right_logical value (Stdlib.( land ) bits 31)

let ( lor ) = Int32.logor
let ( land ) = Int32.logand

(*
   Encode general-purpose register to 5-bit value
*)
let encodeReg = function
  | ARM64.X0 -> 0l
  | ARM64.X1 -> 1l
  | ARM64.X2 -> 2l
  | ARM64.X3 -> 3l
  | ARM64.X4 -> 4l
  | ARM64.X5 -> 5l
  | ARM64.X6 -> 6l
  | ARM64.X7 -> 7l
  | ARM64.X8 -> 8l
  | ARM64.X9 -> 9l
  | ARM64.X10 -> 10l
  | ARM64.X11 -> 11l
  | ARM64.X12 -> 12l
  | ARM64.X13 -> 13l
  | ARM64.X14 -> 14l
  | ARM64.X15 -> 15l
  | ARM64.X16 -> 16l
  | ARM64.X17 -> 17l
  | ARM64.X18 -> 18l
  | ARM64.X19 -> 19l
  | ARM64.X20 -> 20l
  | ARM64.X21 -> 21l
  | ARM64.X22 -> 22l
  | ARM64.X23 -> 23l
  | ARM64.X24 -> 24l
  | ARM64.X25 -> 25l
  | ARM64.X26 -> 26l
  | ARM64.X27 -> 27l
  | ARM64.X28 -> 28l
  | ARM64.X29 -> 29l
  | ARM64.X30 -> 30l
  | ARM64.SP -> 31l

(*
   Encode floating-point register to 5-bit value
*)
let encodeFReg = function
  | ARM64.D0 -> 0l
  | ARM64.D1 -> 1l
  | ARM64.D2 -> 2l
  | ARM64.D3 -> 3l
  | ARM64.D4 -> 4l
  | ARM64.D5 -> 5l
  | ARM64.D6 -> 6l
  | ARM64.D7 -> 7l
  | ARM64.D8 -> 8l
  | ARM64.D9 -> 9l
  | ARM64.D10 -> 10l
  | ARM64.D11 -> 11l
  | ARM64.D12 -> 12l
  | ARM64.D13 -> 13l
  | ARM64.D14 -> 14l
  | ARM64.D15 -> 15l
  | ARM64.D16 -> 16l
  | ARM64.D17 -> 17l
  | ARM64.D18 -> 18l
  | ARM64.D19 -> 19l
  | ARM64.D20 -> 20l
  | ARM64.D21 -> 21l
  | ARM64.D22 -> 22l
  | ARM64.D23 -> 23l
  | ARM64.D24 -> 24l
  | ARM64.D25 -> 25l
  | ARM64.D26 -> 26l
  | ARM64.D27 -> 27l
  | ARM64.D28 -> 28l
  | ARM64.D29 -> 29l
  | ARM64.D30 -> 30l
  | ARM64.D31 -> 31l

let regName reg =
  if reg = ARM64.SP then "SP" else "X" ^ Int32.to_string (encodeReg reg)

let conditionName = function
  | ARM64.EQ -> "EQ"
  | ARM64.NE -> "NE"
  | ARM64.LT -> "LT"
  | ARM64.GT -> "GT"
  | ARM64.LE -> "LE"
  | ARM64.GE -> "GE"
  | ARM64.LO -> "LO"
  | ARM64.HI -> "HI"
  | ARM64.LS -> "LS"
  | ARM64.HS -> "HS"

let dataRefValue = function
  | Symbolic.StringLiteral value ->
      StructuralFormat.Union ("StringLiteral", [ StructuralFormat.Text value ])
  | Symbolic.FloatLiteral value ->
      StructuralFormat.Union
        ( "FloatLiteral",
          [ StructuralFormat.Scalar (FloatFormat.structural value) ] )
  | Symbolic.Named value ->
      StructuralFormat.Union ("Named", [ StructuralFormat.Text value ])

let labelRefName value =
  StructuralFormat.format
    (match value with
    | Symbolic.CodeLabel name ->
        StructuralFormat.Union ("CodeLabel", [ StructuralFormat.Text name ])
    | Symbolic.DataLabel data ->
        StructuralFormat.Union ("DataLabel", [ dataRefValue data ]))

let encodeUnsignedScaled12Offset instructionName offset =
  if offset > 32760 || offset mod 8 <> 0 then
    Crash.crash
      (Printf.sprintf
         "%s: unsigned scaled offset must be 0..32760 and 8-byte aligned, got \
          %d"
         instructionName offset);
  uint32 (offset / 8) lsl 10

let encodeInt16UnsignedScaled12Offset instructionName offset =
  if offset < 0 then
    Crash.crash
      (Printf.sprintf
         "%s: unsigned scaled offset must be 0..32760 and 8-byte aligned, got \
          %d"
         instructionName offset);
  encodeUnsignedScaled12Offset instructionName offset

let encodeInt16SignedScaled7Offset instructionName offset =
  if offset < -512 || offset > 504 || offset mod 8 <> 0 then
    Crash.crash
      (Printf.sprintf
         "%s: signed pair offset must be -512..504 and 8-byte aligned, got %d"
         instructionName offset);
  (uint32 (offset / 8) land 0x7fl) lsl 15

let encodeUnsigned12Immediate instructionName imm =
  if imm > 4095 then
    Crash.crash
      (Printf.sprintf
         "%s: unsigned immediate must fit imm12 field (0..4095), got %d"
         instructionName imm);
  uint32 imm lsl 10

let encodeMoveWideShift instructionName = function
  | 0 -> 0l lsl 21
  | 16 -> 1l lsl 21
  | 32 -> 2l lsl 21
  | 48 -> 3l lsl 21
  | shift ->
      Crash.crash
        (Printf.sprintf "%s: shift must be one of 0, 16, 32, or 48, got %d"
           instructionName shift)

(*
   Encode a 64-bit AArch64 logical immediate whose bits form one circular run
   of ones. N=1 selects a 64-bit element; immr rotates the low run into place.
*)
let tryEncode64BitLogicalImmediate imm =
  let onesCount = Int64.popcount imm in
  if onesCount = 0 || onesCount = 64 then None
  else
    let lowOnes = Int64.pred (Int64.shift_left 1L onesCount) in
    let rotateRight value amount =
      if amount = 0 then value
      else
        Int64.logor
          (Int64.shift_right_logical value amount)
          (Int64.shift_left value (64 - amount))
    in
    List.find_map
      (fun rotation ->
        if rotateRight lowOnes rotation = imm then
          Some (uint32 rotation, uint32 (onesCount - 1))
        else None)
      (List.init 64 Fun.id)

(*
   Encode one symbolic ARM64 instruction to one 32-bit machine-code word.
   Symbolic branches and labels are resolved by encodeSymbolicWithLabels.
   MOVZ encoding: sf=1 opc=10 100101 hw imm16 Rd
   Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
   MOVN encoding: sf=1 opc=00 100101 hw imm16 Rd
   Sets Rd = NOT(imm16 << shift), useful for negative constants
   MOVN has opc=00
   MOVK encoding: sf=1 opc=11 100101 hw imm16 Rd
   ADD immediate: sf=1 0 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
   No shift
   ADD register: sf=1 0 0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
   ADD shifted register: sf=1 0 0 01011 shift=00(LSL) 0 Rm(5) imm6(6) Rn(5) Rd(5)
   dest = src1 + (src2 << shiftAmt)
   shift amount in bits 15-10
   SUB immediate: sf=1 op=1 S=0 10001 shift(2) imm12(12) Rn(5) Rd(5)
   64-bit operation
   SUB (vs ADD which has op=0)
   Don't set flags
   Fixed opcode bits
   SUB immediate with shift=12: actual value = imm * 4096
   sf=1 op=1 S=0 10001 shift=01(12-bit shift) imm12(12) Rn(5) Rd(5)
   shift=1 means LSL #12
   SUB register: sf=1 op=1 S=0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
   Bits: sf(31) op(30) S(29) 01011(28-24) shift(23-22) 0(21) Rm(20-16) imm6(15-10) Rn(9-5) Rd(4-0)
   64-bit
   Subtract (not add)
   Don't set flags (use SUB not SUBS)
   SUB shifted register: sf=1 op=1 S=0 01011 shift=00(LSL) 0 Rm(5) imm6(6) Rn(5) Rd(5)
   dest = src1 - (src2 << shiftAmt)
   SUBS immediate: like SUB but sets condition flags
   sf=1 op=1 S=1 10001 shift=00 imm12(12) Rn(5) Rd(5)
   Set flags (this is SUBS, not SUB)
   No shift on immediate
   MADD: sf=1 0 0 11011 000 Rm(5) 0 Ra=11111 Rn(5) Rd(5)
   Using Ra=XZR(31) for pure multiply
   XZR
   SDIV: sf=1 0 0 11010110 Rm(5) 000011 Rn(5) Rd(5)
   UDIV: sf=1 0 0 11010110 Rm(5) 000010 Rn(5) Rd(5)
   MSUB: sf=1 0 0 11011 000 Rm(5) 1 Ra(5) Rn(5) Rd(5)
   Computes: Rd = Ra - (Rn * Rm)
   Distinguishes MSUB from MADD
   MADD: sf=1 0 0 11011 000 Rm(5) 0 Ra(5) Rn(5) Rd(5)
   Computes: Rd = Ra + (Rn * Rm)
   flagBit = 0 for MADD (vs 1 for MSUB)
   Special case: MOV Xd, SP cannot use ORR (register 31 = XZR in ORR context)
   Use ADD Xd, SP, #0 instead
   Zero immediate
   SP
   MOV is ORR with XZR: sf=1 01 01010 00 0 Rm(5) 000000 Rn=11111 Rd(5)
   Bit 31: sf=1, Bit 30: 0, Bit 29: 1 (ORR not AND), Bits 28-21: 01010000
   Critical: bit 29 distinguishes ORR (1) from AND (0)
   STRB immediate unsigned offset: 00 111 001 00 imm12 Rn Rt
   Size=00 (byte), opc=00, bit24=1 for unsigned offset mode
   Byte operation
   Fixed bits for STRB unsigned offset (bit 24 = 1)
   LDRB (register offset): 00 111 000 01 1 Rm option S 10 Rn Rt
   Size=00 (byte), V=0, opc=01 (load unsigned), register offset mode
   option=011 (LSL), S=0 (no shift)
   Byte operation (bits 31-30 = 00)
   111 0 00 01 1 at bits 29-21
   LSL extend
   LDRB (unsigned offset): 00 111 001 01 imm12 Rn Rt
   Size=00 (byte), V=0, opc=01 (load unsigned), unsigned offset mode
   Fixed bits for LDRB unsigned offset (opc=01)
   STRB (register): store byte to address in register
   Use immediate offset 0: 00 111 001 00 000000000000 Rn Rt
   Fixed bits for STRB unsigned offset
   offset = 0
   Label-based branches - resolved via two-pass encoding (see encodeWithLabels)
   These are only valid after label resolution.
   Resolved in encodeWithLabels with computed label offsets
   Pseudo-instruction: marks a label position (no machine code generated)
   Offset-based branches (for handcrafted runtime code with known offsets)
   CBZ: sf 011010 0 imm19 Rt
   Compare and Branch on Zero
   64-bit register
   CBZ (vs CBNZ which has 1)
   Offset is in instructions (4-byte units), sign-extended, stored as imm19
   CBNZ: sf 011010 1 imm19 Rt
   Compare and Branch on Non-Zero
   CBNZ (vs CBZ which has 0)
   TBZ: b5 011011 0 b40 imm14 Rt
   Test bit and Branch if Zero
   b5 = bit[5], b40 = bit[4:0]
   TBZ (vs TBNZ which has 1)
   TBNZ: b5 011011 1 b40 imm14 Rt
   Test bit and Branch if Not Zero
   TBNZ (vs TBZ which has 0)
   B: 000101 imm26
   Unconditional branch
   Offset is in instructions (4-byte units), sign-extended
   B.cond: 01010100 imm19 0 cond
   Conditional branch based on condition flags
   Equal (Z set)
   Not equal (Z clear)
   Less than (signed)
   Greater than (signed)
   Less than or equal (signed)
   Greater than or equal (signed)
   Lower than (unsigned)
   Higher than (unsigned)
   Lower than or same (unsigned)
   Higher than or same (unsigned)
   CMP immediate is SUBS XZR, Rn, #imm (SUB with set flags, dest=XZR)
   Encoding: sf=1 op=1 S=1 10001 shift(2) imm12(12) Rn(5) Rd=11111
   SUB (vs ADD)
   Set flags (critical for CMP)
   XZR (discard result, only flags matter)
   CMP register is SUBS XZR, Rn, Rm (SUB with set flags, dest=XZR)
   Encoding: sf=1 op=1 S=1 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd=11111
   SUB
   XZR (discard result)
   CSET Rd, cond is CSINC Rd, XZR, XZR, invert(cond)
   Encoding: sf=1 op=0 S=0 11010100 Rm=11111 cond(4) 01 Rn=11111 Rd(5)
   Invert condition for CSINC
   Inverted from NE
   Inverted from EQ
   Inverted from GE
   Inverted from LE
   Inverted from GT
   Inverted from LT
   Inverted from HS
   Inverted from LS
   Inverted from HI
   Inverted from LO
   CSINC vs CSEL
   AND register: sf=1 opc=00 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
   AND (vs ORR which has opc=01)
   BIC register is AND (shifted register) with the N bit set to invert Rm.
   AND immediate: sf=1 opc=00 100100 N(1) immr(6) imms(6) Rn(5) Rd(5)
   AND
   N=1 for 64-bit element size
   ORR register: sf=1 opc=01 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
   ORR
   EOR register: sf=1 opc=10 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
   EOR (vs AND=00, ORR=01)
   LSLV (variable shift left): sf=1 0 0 11010110 Rm(5) 001000 Rn(5) Rd(5)
   LSLV opcode
   LSRV (variable shift right): sf=1 0 0 11010110 Rm(5) 001001 Rn(5) Rd(5)
   LSRV opcode
   LSL Rd, Rn, #shift is alias for UBFM Rd, Rn, #(64-shift), #(63-shift)
   UBFM: sf=1 opc=10 100110 N=1 immr(6) imms(6) Rn(5) Rd(5)
   UBFM
   N=1 for 64-bit
   LSR Rd, Rn, #shift is alias for UBFM Rd, Rn, #shift, #63
   imms = 63 for LSR
   ASR is the SBFM alias with immr=shift and imms=63.
   MVN is ORN Rd, XZR, Rm (OR NOT with Rn=XZR)
   Encoding: sf=1 opc=01 01010 shift=00 1 Rm(5) imm6=000000 Rn=11111 Rd(5)
   ORR-family
   NOT bit (distinguishes ORN from ORR)
   NEG: SUB dest, XZR, src
   Encoding: sf=1 op=1 S=0 01011 shift=00 0 Rm(src) imm6=000000 Rn=11111(XZR) Rd(dest)
   Subtract
   Stack operations
   STP (Store Pair) - signed offset addressing
   Encoding: opc(2) 101 V(1) mode(2) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
   For 64-bit registers: opc=10 (bits 31-30)
   V=0 (integer), mode=10 (signed offset), L=0 (store)
   Bits 29-22 = 1010 010 0 = 0b10100100
   STP: 101 0 010 0 (mode=signed offset, L=0)
   LDP (Load Pair) - signed offset addressing
   V=0 (integer), mode=10 (signed offset), L=1 (load)
   Bits 29-22 = 1010 010 1 = 0b10100101
   LDP: 101 0 010 1 (mode=signed offset, L=1)
   STP (Store Pair) - pre-indexed addressing: addr += offset, then store
   Encoding: opc(2) 101 V(1) mode(3) L(1) imm7(7) Rt2(5) Rn(5) Rt(5)
   V=0 (integer), mode=011 (pre-indexed), L=0 (store)
   Bits 29-22 = 1010 011 0 = 0b10100110
   STP pre-indexed: 101 0 011 0
   LDP (Load Pair) - post-indexed addressing: load, then addr += offset
   V=0 (integer), mode=01 (post-indexed), L=1 (load)
   Bits 29-22 = 1010 001 1 = 0b10100011
   LDP post-indexed: 101 0 001 1
   STR (Store Register) - unsigned offset addressing
   Encoding: size=11 111 001 00 imm12 Rn Rt
   For 64-bit: size=11 (bits 31-30)
   imm12 is unsigned offset in units of 8 bytes (bits 21-10)
   64-bit (11)
   STR unsigned offset mode
   LDR (Load Register) - unsigned offset addressing
   Encoding: size=11 111 001 01 imm12 Rn Rt
   Same as STR but bit 22 = 1 for load
   LDR unsigned offset mode (bit 22=1)
   STUR (Store Register Unscaled) - signed offset addressing
   Encoding: size=11 111 000 00 0 imm9 00 Rn Rt
   Fixed bits 29-21: 111000000 (unscaled store)
   imm9 is signed offset in bytes (bits 20-12)
   STUR: bits 29-21 = 111000000
   Extract 9-bit signed offset (sign-extend to 32-bit, then mask)
   LDUR (Load Register Unscaled) - signed offset addressing
   Encoding: size=11 111 000 01 0 imm9 00 Rn Rt
   Same as STUR but bit 22 = 1 for load
   Fixed bits 29-21: 111000010 (unscaled load, bit 22=1)
   LDUR: bits 29-21 = 111000010 (bit 22=1)
   BL is handled in encodeWithLabels (label-based branch)
   Resolved via two-pass encoding like other label-based branches
   BLR: Branch with Link to Register
   Encoding: 1101011 0 0 01 11111 0000 0 0 Rn 00000
   0xD63F0000 | (Rn << 5)
   BR: Branch to Register (no link, for tail calls)
   Encoding: 1101011 0 0 00 11111 0000 0 0 Rn 00000
   0xD61F0000 | (Rn << 5)
   RET: 1101011 0 0 10 11111 0000 0 0 Rn=11110 00000
   Default RET uses X30 (link register)
   SVC: 11010100 000 imm16 000 01
   Bits: 11010100000(31-21) imm16(20-5) 00001(4-0)
   Floating-point instructions
   LDR (SIMD&FP) - unsigned offset addressing for double
   Encoding: size=11 111 101 01 imm12 Rn Rt
   size=11 for 64-bit (double), V=1 (FP), opc=01 (load)
   64-bit (double)
   LDR FP unsigned offset mode
   STR (SIMD&FP) - unsigned offset addressing for double
   Encoding: size=11 111 101 00 imm12 Rn Rt
   size=11 for 64-bit (double), V=1 (FP), opc=00 (store)
   STR FP unsigned offset mode
   STP (SIMD&FP) - signed offset addressing for double
   opc=01 for 64-bit (D registers), V=1 (FP), mode=010 (signed offset), L=0 (store)
   Bits 29-22 = 101 1 010 0 = 0b10110100
   01 for 64-bit FP
   STP FP: 101 1 010 0
   LDP (SIMD&FP) - signed offset addressing for double
   opc=01 for 64-bit (D registers), V=1 (FP), mode=010 (signed offset), L=1 (load)
   Bits 29-22 = 101 1 010 1 = 0b10110101
   LDP FP: 101 1 010 1
   FADD (scalar, double): 0001 1110 01 1 Rm 0010 10 Rn Rd
   ftype=01 (double), opcode=0010 (add)
   FADD
   FSUB (scalar, double): 0001 1110 01 1 Rm 0011 10 Rn Rd
   FSUB
   FMUL (scalar, double): 0001 1110 01 1 Rm 0000 10 Rn Rd
   FMUL
   FMADD Dd, Dn, Dm, Da: Dd = Da + Dn * Dm.
   FDIV (scalar, double): 0001 1110 01 1 Rm 0001 10 Rn Rd
   FDIV
   FNEG (scalar, double): 0x1E614000 for D0, D0
   Encoding: 0001 1110 0110 0001 0100 0000 Rn Rd
   bit 16 set for FNEG
   FNEG opcode (16)
   FABS (scalar, double): 0x1E60C000 for D0, D0
   Encoding: 0001 1110 0110 0000 1100 0000 Rn Rd
   bit 16 clear for FABS
   FABS opcode (48)
   FSQRT (scalar, double): 0x1E61C000 for D0, D0
   Encoding: 0001 1110 0110 0001 1100 0000 Rn Rd
   bit 16 set for FSQRT
   FSQRT opcode (48)
   FCMP (scalar, double): 0001 1110 01 1 Rm 00 1000 Rn 00 000
   Encoding: 0001 1110 01 1 Rm 00 1000 Rn 00 opc=000
   FCMP
   Compare (not with zero)
   FMOV (register, double): 0001 1110 01 1 00000 010000 Rn Rd
   Unused
   FMOV
   FMOV (scalar immediate, double): base opcode plus modified-immediate byte.
   FMOV Dd, XZR copies the zero register's bits into a double register.
   Register 31 denotes XZR in this instruction encoding.
   FMOV (scalar to GP, double): 1001 1110 01 1 00110 000000 Vn Rd
   sf=1, ftype=01 (double), rmode=00, opcode=110
   Move 64-bit FP register to GP register (bit-for-bit)
   FMOV to general
   FMOV (general to scalar, double): 1001 1110 01 1 00111 000000 Rn Vd
   sf=1, ftype=01 (double), rmode=00, opcode=111
   Move GP register to 64-bit FP register (bit-for-bit)
   FMOV from general
   CNT Vd.8B, Vn.8B. The SIMD register number occupies the same
   encoding field as its scalar D-register view.
   ADDV Bd, Vn.8B horizontally sums the eight byte lanes.
   UMOV Wd, Vn.B[0] zero-extends the selected byte and, by writing Wd,
   clears the upper half of the corresponding X register.
   SCVTF (scalar, integer to FP, double): 1001 1110 01 1 00010 000000 Rn Rd
   sf=1 (64-bit int), ftype=01 (double), rmode=00, opcode=010
   SCVTF
   FCVTZS (scalar, FP to integer, double): 1001 1110 01 1 11000 000000 Rn Rd
   sf=1 (64-bit int), ftype=01 (double), rmode=11 (toward zero), opcode=000
   FCVTZS
   Sign/zero extension instructions (for integer overflow truncation)
   These are encoded using SBFM/UBFM (Signed/Unsigned Bitfield Move)
   SXTB: SBFM Xd, Xn, #0, #7 (sign-extend byte to 64-bit)
   Encoding: sf=1 opc=00 100110 N=1 immr=0 imms=7 Rn Rd
   SBFM (signed)
   rotate by 0
   extract bits 0-7 (byte)
   SXTH: SBFM Xd, Xn, #0, #15 (sign-extend halfword to 64-bit)
   SBFM
   extract bits 0-15 (halfword)
   SXTW: SBFM Xd, Xn, #0, #31 (sign-extend word to 64-bit)
   extract bits 0-31 (word)
   UXTB: UBFM Xd, Xn, #0, #7 (zero-extend byte to 64-bit)
   Encoding: sf=1 opc=10 100110 N=1 immr=0 imms=7 Rn Rd
   UBFM (unsigned)
   UXTH: UBFM Xd, Xn, #0, #15 (zero-extend halfword to 64-bit)
   UXTW: UBFM Xd, Xn, #0, #31 (zero-extend word to 64-bit)
*)
let encodeSymbolicWord instr =
  match instr with
  | Symbolic.MOVZ (dest, imm, shift) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let opcode = 0b100101l lsl 23 in
      let hw = encodeMoveWideShift "MOVZ" shift in
      let imm16 = uint32 imm lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor opcode lor hw lor imm16 lor rd
  | Symbolic.MOVN (dest, imm, shift) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let opcode = 0b100101l lsl 23 in
      let hw = encodeMoveWideShift "MOVN" shift in
      let imm16 = uint32 imm lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor opcode lor hw lor imm16 lor rd
  | Symbolic.MOVK (dest, imm, shift) ->
      let sf = 1l lsl 31 in
      let opc = 3l lsl 29 in
      let opcode = 0b100101l lsl 23 in
      let hw = encodeMoveWideShift "MOVK" shift in
      let imm16 = uint32 imm lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor opcode lor hw lor imm16 lor rd
  | Symbolic.ADD_imm (dest, src, imm) ->
      let sf = 1l lsl 31 in
      let op = 0b10001l lsl 24 in
      let shift = 0l lsl 22 in
      let imm12 = encodeUnsigned12Immediate "ADD_imm" imm in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor shift lor imm12 lor rn lor rd
  | Symbolic.ADD_reg (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let op = 0b01011l lsl 24 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor rn lor rd
  | Symbolic.ADD_shifted (dest, src1, src2, shiftAmt) ->
      let sf = 1l lsl 31 in
      let op = 0b01011l lsl 24 in
      let rm = encodeReg src2 lsl 16 in
      let imm6 = uint32 shiftAmt lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor imm6 lor rn lor rd
  | Symbolic.ADD_extended (dest, src1, src2, extend) ->
      let option =
        match extend with
        | ARM64.ExtendUXTB -> 0l
        | ARM64.ExtendUXTH -> 1l
        | ARM64.ExtendUXTW -> 2l
        | ARM64.ExtendSXTB -> 4l
        | ARM64.ExtendSXTH -> 5l
        | ARM64.ExtendSXTW -> 6l
      in
      0x8B200000l
      lor (encodeReg src2 lsl 16)
      lor (option lsl 13)
      lor (encodeReg src1 lsl 5)
      lor encodeReg dest
  | Symbolic.SUB_imm (dest, src, imm) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 0l lsl 29 in
      let opcode = 0b10001l lsl 24 in
      let shift = 0l lsl 22 in
      let imm12 = encodeUnsigned12Immediate "SUB_imm" imm in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor shift lor imm12 lor rn lor rd
  | Symbolic.SUB_imm12 (dest, src, imm) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 0l lsl 29 in
      let opcode = 0b10001l lsl 24 in
      let shift = 1l lsl 22 in
      let imm12 = encodeUnsigned12Immediate "SUB_imm12" imm in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor shift lor imm12 lor rn lor rd
  | Symbolic.SUB_reg (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 0l lsl 29 in
      let opcode = 0b01011l lsl 24 in
      let shift = 0l lsl 22 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor shift lor rm lor rn lor rd
  | Symbolic.SUB_shifted (dest, src1, src2, shiftAmt) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 0l lsl 29 in
      let opcode = 0b01011l lsl 24 in
      let rm = encodeReg src2 lsl 16 in
      let imm6 = uint32 shiftAmt lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor rm lor imm6 lor rn lor rd
  | Symbolic.SUB_extended (dest, src1, src2, extend) ->
      let option =
        match extend with
        | ARM64.ExtendUXTB -> 0l
        | ARM64.ExtendUXTH -> 1l
        | ARM64.ExtendUXTW -> 2l
        | ARM64.ExtendSXTB -> 4l
        | ARM64.ExtendSXTH -> 5l
        | ARM64.ExtendSXTW -> 6l
      in
      0xCB200000l
      lor (encodeReg src2 lsl 16)
      lor (option lsl 13)
      lor (encodeReg src1 lsl 5)
      lor encodeReg dest
  | Symbolic.SUBS_imm (dest, src, imm) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 1l lsl 29 in
      let opcode = 0b10001l lsl 24 in
      let shift = 0l lsl 22 in
      let imm12 = encodeUnsigned12Immediate "SUBS_imm" imm in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor shift lor imm12 lor rn lor rd
  | Symbolic.MUL (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let op = 0b11011000l lsl 21 in
      let rm = encodeReg src2 lsl 16 in
      let ra = 31l lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor ra lor rn lor rd
  | Symbolic.SDIV (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let op = 0b11010110l lsl 21 in
      let rm = encodeReg src2 lsl 16 in
      let fixedBits = 0b000011l lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor fixedBits lor rn lor rd
  | Symbolic.UDIV (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let op = 0b11010110l lsl 21 in
      let rm = encodeReg src2 lsl 16 in
      let fixedBits = 0b000010l lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor fixedBits lor rn lor rd
  | Symbolic.MSUB (dest, src1, src2, src3) ->
      let sf = 1l lsl 31 in
      let op = 0b11011000l lsl 21 in
      let rm = encodeReg src2 lsl 16 in
      let flagBit = 1l lsl 15 in
      let ra = encodeReg src3 lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor flagBit lor ra lor rn lor rd
  | Symbolic.MADD (dest, src1, src2, src3) ->
      let sf = 1l lsl 31 in
      let op = 0b11011000l lsl 21 in
      let rm = encodeReg src2 lsl 16 in
      let ra = encodeReg src3 lsl 10 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor ra lor rn lor rd
  | Symbolic.MOV_reg (dest, src) ->
      if src = ARM64.SP then
        let sf = 1l lsl 31 in
        let op = 0b10001l lsl 24 in
        let shift = 0l lsl 22 in
        let imm12 = 0l lsl 10 in
        let rn = 31l lsl 5 in
        let rd = encodeReg dest in
        sf lor op lor shift lor imm12 lor rn lor rd
      else
        let sf = 1l lsl 31 in
        let opc = 1l lsl 29 in
        let op = 0b01010000l lsl 21 in
        let rm = encodeReg src lsl 16 in
        let rn = 31l lsl 5 in
        let rd = encodeReg dest in
        sf lor opc lor op lor rm lor rn lor rd
  | Symbolic.STRB (src, addr, offset) ->
      let size = 0l lsl 30 in
      let vOpc = 0b11100100l lsl 22 in
      let imm12 = (uint32 offset land 0xFFFl) lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg src in
      size lor vOpc lor imm12 lor rn lor rt
  | Symbolic.LDRB (dest, baseReg, indexReg) ->
      let size = 0l lsl 30 in
      let bits29to21 = 0b111000011l lsl 21 in
      let rm = encodeReg indexReg lsl 16 in
      let option = 0b011l lsl 13 in
      let s = 0l lsl 12 in
      let fixed2 = 0b10l lsl 10 in
      let rn = encodeReg baseReg lsl 5 in
      let rt = encodeReg dest in
      size lor bits29to21 lor rm lor option lor s lor fixed2 lor rn lor rt
  | Symbolic.LDRB_imm (dest, baseReg, offset) ->
      let size = 0l lsl 30 in
      let vOpc = 0b11100101l lsl 22 in
      let imm12 = (uint32 offset land 0xFFFl) lsl 10 in
      let rn = encodeReg baseReg lsl 5 in
      let rt = encodeReg dest in
      size lor vOpc lor imm12 lor rn lor rt
  | Symbolic.STRB_reg (src, addr) ->
      let size = 0l lsl 30 in
      let vOpc = 0b11100100l lsl 22 in
      let imm12 = 0l lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg src in
      size lor vOpc lor imm12 lor rn lor rt
  | Symbolic.CBZ (reg, label) ->
      Crash.crash
        (Printf.sprintf "CBZ label must be resolved before encoding: %s, %s"
           (regName reg) label)
  | Symbolic.CBNZ (reg, label) ->
      Crash.crash
        (Printf.sprintf "CBNZ label must be resolved before encoding: %s, %s"
           (regName reg) label)
  | Symbolic.B_label label ->
      Crash.crash
        (Printf.sprintf "B label must be resolved before encoding: %s" label)
  | Symbolic.B_cond_label (cond, label) ->
      Crash.crash
        (Printf.sprintf
           "conditional B label must be resolved before encoding: %s, %s"
           (conditionName cond) label)
  | Symbolic.Label label ->
      Crash.crash
        (Printf.sprintf "label does not encode to a machine-code word: %s" label)
  | Symbolic.ADRP (dest, label) ->
      Crash.crash
        (Printf.sprintf "ADRP label must be resolved before encoding: %s, %s"
           (regName dest) (labelRefName label))
  | Symbolic.ADR (dest, label) ->
      Crash.crash
        (Printf.sprintf "ADR label must be resolved before encoding: %s, %s"
           (regName dest) (labelRefName label))
  | Symbolic.ADD_label (dest, src, label) ->
      Crash.crash
        (Printf.sprintf "ADD label must be resolved before encoding: %s, %s, %s"
           (regName dest) (regName src) (labelRefName label))
  | Symbolic.CBZ_offset (reg, offset) ->
      let sf = 1l lsl 31 in
      let op = 0b011010l lsl 25 in
      let flag = 0l lsl 24 in
      let imm19 = (uint32 offset land 0x7FFFFl) lsl 5 in
      let rt = encodeReg reg in
      sf lor op lor flag lor imm19 lor rt
  | Symbolic.CBNZ_offset (reg, offset) ->
      let sf = 1l lsl 31 in
      let op = 0b011010l lsl 25 in
      let flag = 1l lsl 24 in
      let imm19 = (uint32 offset land 0x7FFFFl) lsl 5 in
      let rt = encodeReg reg in
      sf lor op lor flag lor imm19 lor rt
  | Symbolic.TBZ (reg, bit, offset) ->
      let b5 = (uint32 bit lsr 5) lsl 31 in
      let op = 0b011011l lsl 25 in
      let flag = 0l lsl 24 in
      let b40 = (uint32 bit land 0x1Fl) lsl 19 in
      let imm14 = (uint32 offset land 0x3FFFl) lsl 5 in
      let rt = encodeReg reg in
      b5 lor op lor flag lor b40 lor imm14 lor rt
  | Symbolic.TBNZ (reg, bit, offset) ->
      let b5 = (uint32 bit lsr 5) lsl 31 in
      let op = 0b011011l lsl 25 in
      let flag = 1l lsl 24 in
      let b40 = (uint32 bit land 0x1Fl) lsl 19 in
      let imm14 = (uint32 offset land 0x3FFFl) lsl 5 in
      let rt = encodeReg reg in
      b5 lor op lor flag lor b40 lor imm14 lor rt
  | Symbolic.TBZ_label _ | Symbolic.TBNZ_label _ ->
      Crash.crash "test-bit branch label must be resolved before encoding"
  | Symbolic.B offset ->
      let op = 0b000101l lsl 26 in
      let imm26 = uint32 offset land 0x3FFFFFFl in
      op lor imm26
  | Symbolic.B_cond (cond, offset) ->
      let op = 0b01010100l lsl 24 in
      let imm19 = (uint32 offset land 0x7FFFFl) lsl 5 in
      let condBits =
        match cond with
        | ARM64.EQ -> 0b0000l
        | ARM64.NE -> 0b0001l
        | ARM64.LT -> 0b1011l
        | ARM64.GT -> 0b1100l
        | ARM64.LE -> 0b1101l
        | ARM64.GE -> 0b1010l
        | ARM64.LO -> 0b0011l
        | ARM64.HI -> 0b1000l
        | ARM64.LS -> 0b1001l
        | ARM64.HS -> 0b0010l
      in
      op lor imm19 lor condBits
  | Symbolic.CMP_imm (src, imm) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 1l lsl 29 in
      let opcode = 0b10001l lsl 24 in
      let shift = 0l lsl 22 in
      let imm12 = encodeUnsigned12Immediate "CMP_imm" imm in
      let rn = encodeReg src lsl 5 in
      let rd = 31l in
      sf lor op lor s lor opcode lor shift lor imm12 lor rn lor rd
  | Symbolic.CMP_reg (src1, src2) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 1l lsl 29 in
      let opcode = 0b01011l lsl 24 in
      let shift = 0l lsl 22 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = 31l in
      sf lor op lor s lor opcode lor shift lor rm lor rn lor rd
  | Symbolic.CSET (dest, cond) ->
      let sf = 1l lsl 31 in
      let op = 0l lsl 30 in
      let s = 0l lsl 29 in
      let opcode = 0b11010100l lsl 21 in
      let rm = 31l lsl 16 in
      let condCode =
        match cond with
        | ARM64.EQ -> 0b0001l
        | ARM64.NE -> 0b0000l
        | ARM64.LT -> 0b1010l
        | ARM64.GT -> 0b1101l
        | ARM64.LE -> 0b1100l
        | ARM64.GE -> 0b1011l
        | ARM64.LO -> 0b0010l
        | ARM64.HI -> 0b1001l
        | ARM64.LS -> 0b1000l
        | ARM64.HS -> 0b0011l
      in
      let condBits = condCode lsl 12 in
      let fixedBits = 0b01l lsl 10 in
      let rn = 31l lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor rm lor condBits lor fixedBits lor rn lor rd
  | Symbolic.CSEL (dest, whenTrue, whenFalse, cond) ->
      let condition =
        match cond with
        | ARM64.EQ -> 0b0000l
        | ARM64.NE -> 0b0001l
        | ARM64.HS -> 0b0010l
        | ARM64.LO -> 0b0011l
        | ARM64.HI -> 0b1000l
        | ARM64.LS -> 0b1001l
        | ARM64.GE -> 0b1010l
        | ARM64.LT -> 0b1011l
        | ARM64.GT -> 0b1100l
        | ARM64.LE -> 0b1101l
      in
      0x9A800000l
      lor (encodeReg whenFalse lsl 16)
      lor (condition lsl 12)
      lor (encodeReg whenTrue lsl 5)
      lor encodeReg dest
  | Symbolic.AND_reg (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let op = 0b01010l lsl 24 in
      let shift = 0l lsl 22 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor shift lor rm lor rn lor rd
  | Symbolic.BIC_reg (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let op = 0b01010l lsl 24 in
      let shift = 0l lsl 22 in
      let invertRm = 1l lsl 21 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor shift lor invertRm lor rm lor rn lor rd
  | Symbolic.AND_imm (dest, src, imm) ->
      let immrValue, immsValue =
        match tryEncode64BitLogicalImmediate imm with
        | Some fields -> fields
        | None ->
            Crash.crash
              (Printf.sprintf
                 "AND immediate is not an encodable 64-bit single-run mask: \
                  0x%016LX"
                 imm)
      in
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let op = 0b100100l lsl 23 in
      let n = 1l lsl 22 in
      let immr = immrValue lsl 16 in
      let imms = immsValue lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor n lor immr lor imms lor rn lor rd
  | Symbolic.ORR_reg (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let opc = 1l lsl 29 in
      let op = 0b01010l lsl 24 in
      let shift = 0l lsl 22 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor shift lor rm lor rn lor rd
  | Symbolic.EOR_reg (dest, src1, src2) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let op = 0b01010l lsl 24 in
      let shift = 0l lsl 22 in
      let rm = encodeReg src2 lsl 16 in
      let rn = encodeReg src1 lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor shift lor rm lor rn lor rd
  | Symbolic.LSL_reg (dest, src, shift) ->
      let sf = 1l lsl 31 in
      let op = 0b11010110l lsl 21 in
      let rm = encodeReg shift lsl 16 in
      let fixedBits = 0b001000l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor fixedBits lor rn lor rd
  | Symbolic.LSR_reg (dest, src, shift) ->
      let sf = 1l lsl 31 in
      let op = 0b11010110l lsl 21 in
      let rm = encodeReg shift lsl 16 in
      let fixedBits = 0b001001l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor fixedBits lor rn lor rd
  | Symbolic.ASR_reg (dest, src, shift) ->
      let sf = 1l lsl 31 in
      let op = 0b11010110l lsl 21 in
      let rm = encodeReg shift lsl 16 in
      let fixedBits = 0b001010l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor rm lor fixedBits lor rn lor rd
  | Symbolic.LSL_imm (dest, src, shift) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let op = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = uint32 (Stdlib.( land ) (sub 64 shift) 63) lsl 16 in
      let imms = uint32 (Stdlib.( land ) (sub 63 shift) 63) lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor n lor immr lor imms lor rn lor rd
  | Symbolic.LSR_imm (dest, src, shift) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let op = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = uint32 (Stdlib.( land ) shift 63) lsl 16 in
      let imms = 63l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor n lor immr lor imms lor rn lor rd
  | Symbolic.ASR_imm (dest, src, shift) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let op = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = uint32 (Stdlib.( land ) shift 63) lsl 16 in
      let imms = 63l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor n lor immr lor imms lor rn lor rd
  | Symbolic.MVN (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 1l lsl 29 in
      let op = 0b01010l lsl 24 in
      let shift = 0l lsl 22 in
      let n = 1l lsl 21 in
      let rm = encodeReg src lsl 16 in
      let rn = 31l lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor op lor shift lor n lor rm lor rn lor rd
  | Symbolic.NEG (dest, src) ->
      let sf = 1l lsl 31 in
      let op = 1l lsl 30 in
      let s = 0l lsl 29 in
      let opcode = 0b01011l lsl 24 in
      let shift = 0l lsl 22 in
      let rm = encodeReg src lsl 16 in
      let rn = 31l lsl 5 in
      let rd = encodeReg dest in
      sf lor op lor s lor opcode lor shift lor rm lor rn lor rd
  | Symbolic.STP (reg1, reg2, addr, offset) ->
      let opc = 2l lsl 30 in
      let fixedBits = 0b10100100l lsl 22 in
      let imm7 = encodeInt16SignedScaled7Offset "STP" offset in
      let rt2 = encodeReg reg2 lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg reg1 in
      opc lor fixedBits lor imm7 lor rt2 lor rn lor rt
  | Symbolic.LDP (reg1, reg2, addr, offset) ->
      let opc = 2l lsl 30 in
      let fixedBits = 0b10100101l lsl 22 in
      let imm7 = encodeInt16SignedScaled7Offset "LDP" offset in
      let rt2 = encodeReg reg2 lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg reg1 in
      opc lor fixedBits lor imm7 lor rt2 lor rn lor rt
  | Symbolic.STP_pre (reg1, reg2, addr, offset) ->
      let opc = 2l lsl 30 in
      let fixedBits = 0b10100110l lsl 22 in
      let imm7 = encodeInt16SignedScaled7Offset "STP_pre" offset in
      let rt2 = encodeReg reg2 lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg reg1 in
      opc lor fixedBits lor imm7 lor rt2 lor rn lor rt
  | Symbolic.LDP_post (reg1, reg2, addr, offset) ->
      let opc = 2l lsl 30 in
      let fixedBits = 0b10100011l lsl 22 in
      let imm7 = encodeInt16SignedScaled7Offset "LDP_post" offset in
      let rt2 = encodeReg reg2 lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg reg1 in
      opc lor fixedBits lor imm7 lor rt2 lor rn lor rt
  | Symbolic.STR (src, addr, offset) ->
      let size = 3l lsl 30 in
      let fixedBits = 0b11100100l lsl 22 in
      let imm12 = encodeInt16UnsignedScaled12Offset "STR" offset in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg src in
      size lor fixedBits lor imm12 lor rn lor rt
  | Symbolic.LDR (dest, addr, offset) ->
      let size = 3l lsl 30 in
      let fixedBits = 0b11100101l lsl 22 in
      let imm12 = encodeInt16UnsignedScaled12Offset "LDR" offset in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg dest in
      size lor fixedBits lor imm12 lor rn lor rt
  | Symbolic.STUR (src, addr, offset) ->
      let size = 3l lsl 30 in
      let fixedBits = 0b111000000l lsl 21 in
      let imm9 = (uint32 offset land 0x1FFl) lsl 12 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg src in
      size lor fixedBits lor imm9 lor rn lor rt
  | Symbolic.LDUR (dest, addr, offset) ->
      let size = 3l lsl 30 in
      let fixedBits = 0b111000010l lsl 21 in
      let imm9 = (uint32 offset land 0x1FFl) lsl 12 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeReg dest in
      size lor fixedBits lor imm9 lor rn lor rt
  | Symbolic.BL label ->
      Crash.crash
        (Printf.sprintf "BL label must be resolved before encoding: %s" label)
  | Symbolic.BLR reg ->
      let rn = encodeReg reg in
      0xD63F0000l lor (rn lsl 5)
  | Symbolic.BR reg ->
      let rn = encodeReg reg in
      0xD61F0000l lor (rn lsl 5)
  | Symbolic.RET -> 0xD65F03C0l
  | Symbolic.SVC imm ->
      let imm16 = uint32 imm in
      0xD4000001l lor (imm16 lsl 5)
  | Symbolic.LDR_fp (dest, addr, offset) ->
      let size = 3l lsl 30 in
      let fixedBits = 0b11110101l lsl 22 in
      let imm12 = encodeInt16UnsignedScaled12Offset "LDR_fp" offset in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeFReg dest in
      size lor fixedBits lor imm12 lor rn lor rt
  | Symbolic.STR_fp (src, addr, offset) ->
      let size = 3l lsl 30 in
      let fixedBits = 0b11110100l lsl 22 in
      let imm12 = encodeInt16UnsignedScaled12Offset "STR_fp" offset in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeFReg src in
      size lor fixedBits lor imm12 lor rn lor rt
  | Symbolic.STP_fp (freg1, freg2, addr, offset) ->
      let opc = 1l lsl 30 in
      let fixedBits = 0b10110100l lsl 22 in
      let imm7 = encodeInt16SignedScaled7Offset "STP_fp" offset in
      let rt2 = encodeFReg freg2 lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeFReg freg1 in
      opc lor fixedBits lor imm7 lor rt2 lor rn lor rt
  | Symbolic.LDP_fp (freg1, freg2, addr, offset) ->
      let opc = 1l lsl 30 in
      let fixedBits = 0b10110101l lsl 22 in
      let imm7 = encodeInt16SignedScaled7Offset "LDP_fp" offset in
      let rt2 = encodeFReg freg2 lsl 10 in
      let rn = encodeReg addr lsl 5 in
      let rt = encodeFReg freg1 in
      opc lor fixedBits lor imm7 lor rt2 lor rn lor rt
  | Symbolic.FADD (dest, src1, src2) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = encodeFReg src2 lsl 16 in
      let opcode = 0b001010l lsl 10 in
      let rn = encodeFReg src1 lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FSUB (dest, src1, src2) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = encodeFReg src2 lsl 16 in
      let opcode = 0b001110l lsl 10 in
      let rn = encodeFReg src1 lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FMUL (dest, src1, src2) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = encodeFReg src2 lsl 16 in
      let opcode = 0b000010l lsl 10 in
      let rn = encodeFReg src1 lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FMADD (dest, src1, src2, addend) ->
      0x1F400000l
      lor (encodeFReg src2 lsl 16)
      lor (encodeFReg addend lsl 10)
      lor (encodeFReg src1 lsl 5)
      lor encodeFReg dest
  | Symbolic.FDIV (dest, src1, src2) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = encodeFReg src2 lsl 16 in
      let opcode = 0b000110l lsl 10 in
      let rn = encodeFReg src1 lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FNEG (dest, src) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = 1l lsl 16 in
      let opcode = 0b010000l lsl 10 in
      let rn = encodeFReg src lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FABS (dest, src) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = 0l lsl 16 in
      let opcode = 0b110000l lsl 10 in
      let rn = encodeFReg src lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FSQRT (dest, src) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = 1l lsl 16 in
      let opcode = 0b110000l lsl 10 in
      let rn = encodeFReg src lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FCMP (src1, src2) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = encodeFReg src2 lsl 16 in
      let opcode = 0b001000l lsl 10 in
      let rn = encodeFReg src1 lsl 5 in
      let opc = 0b00000l in
      fixedBits lor rm lor opcode lor rn lor opc
  | Symbolic.FMOV_reg (dest, src) ->
      let fixedBits = 0b00011110011l lsl 21 in
      let rm = 0l lsl 16 in
      let opcode = 0b010000l lsl 10 in
      let rn = encodeFReg src lsl 5 in
      let rd = encodeFReg dest in
      fixedBits lor rm lor opcode lor rn lor rd
  | Symbolic.FMOV_imm (dest, value) -> (
      match ARM64.tryEncodeFmovFloatImmediate value with
      | Some imm8 -> 0x1E601000l lor (imm8 lsl 13) lor encodeFReg dest
      | None ->
          Crash.crash
            (Printf.sprintf "FMOV immediate does not support %s"
               (FloatFormat.roundTrip value)))
  | Symbolic.FMOV_zero dest -> 0x9E6703E0l lor encodeFReg dest
  | Symbolic.FMOV_to_gp (dest, src) ->
      let sf = 1l lsl 31 in
      let fixedBits = 0b0011110011l lsl 21 in
      let opcode1 = 0b00110l lsl 16 in
      let opcode2 = 0b000000l lsl 10 in
      let rn = encodeFReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor fixedBits lor opcode1 lor opcode2 lor rn lor rd
  | Symbolic.FMOV_from_gp (dest, src) ->
      let sf = 1l lsl 31 in
      let fixedBits = 0b0011110011l lsl 21 in
      let opcode1 = 0b00111l lsl 16 in
      let opcode2 = 0b000000l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeFReg dest in
      sf lor fixedBits lor opcode1 lor opcode2 lor rn lor rd
  | Symbolic.CNT_8B (dest, src) ->
      0x0E205800l lor (encodeFReg src lsl 5) lor encodeFReg dest
  | Symbolic.ADDV_8B (dest, src) ->
      0x0E31B800l lor (encodeFReg src lsl 5) lor encodeFReg dest
  | Symbolic.UMOV_byte (dest, src) ->
      0x0E013C00l lor (encodeFReg src lsl 5) lor encodeReg dest
  | Symbolic.SCVTF (dest, src) ->
      let sf = 1l lsl 31 in
      let fixedBits = 0b0011110011l lsl 21 in
      let opcode1 = 0b00010l lsl 16 in
      let opcode2 = 0b000000l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeFReg dest in
      sf lor fixedBits lor opcode1 lor opcode2 lor rn lor rd
  | Symbolic.FCVTZS (dest, src) ->
      let sf = 1l lsl 31 in
      let fixedBits = 0b0011110011l lsl 21 in
      let opcode1 = 0b11000l lsl 16 in
      let opcode2 = 0b000000l lsl 10 in
      let rn = encodeFReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor fixedBits lor opcode1 lor opcode2 lor rn lor rd
  | Symbolic.SXTB (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let fixedBits = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = 0l lsl 16 in
      let imms = 7l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor fixedBits lor n lor immr lor imms lor rn lor rd
  | Symbolic.SXTH (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let fixedBits = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = 0l lsl 16 in
      let imms = 15l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor fixedBits lor n lor immr lor imms lor rn lor rd
  | Symbolic.SXTW (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 0l lsl 29 in
      let fixedBits = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = 0l lsl 16 in
      let imms = 31l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor fixedBits lor n lor immr lor imms lor rn lor rd
  | Symbolic.UXTB (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let fixedBits = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = 0l lsl 16 in
      let imms = 7l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor fixedBits lor n lor immr lor imms lor rn lor rd
  | Symbolic.UXTH (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let fixedBits = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = 0l lsl 16 in
      let imms = 15l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor fixedBits lor n lor immr lor imms lor rn lor rd
  | Symbolic.UXTW (dest, src) ->
      let sf = 1l lsl 31 in
      let opc = 2l lsl 29 in
      let fixedBits = 0b100110l lsl 23 in
      let n = 1l lsl 22 in
      let immr = 0l lsl 16 in
      let imms = 31l lsl 10 in
      let rn = encodeReg src lsl 5 in
      let rd = encodeReg dest in
      sf lor opc lor fixedBits lor n lor immr lor imms lor rn lor rd

(*
   Fixed words and chunk-local relocations are encoded once. Remaining
   relocation slots contain zero until the final program layout is known.
*)
type preparedChunk = {
  machineCodeTemplate : int32 array;
  relocations : (int * Symbolic.instr) array;
  codeLabels : (string * int) array;
  poolLabelRefs : Symbolic.labelRef array;
}

(*
   Two-Pass Encoding for Label Resolution
   Pass 1: Compute byte offset for each concrete label.
*)
let computeLabelPositions instructions =
  let rec loop instrs offset labels =
    match instrs with
    | [] -> labels
    | ARM64.Label name :: rest ->
        loop rest offset (StringOrder.Map.add name offset labels)
    | _ :: rest -> loop rest (add offset 4) labels
  in
  loop instructions 0 StringOrder.Map.empty

(*
   Pass 1: Compute byte offset for each symbolic label.
*)
let computeSymbolicLabelPositions instructions =
  let rec loop instrs offset labels =
    match instrs with
    | [] -> labels
    | Symbolic.Label name :: rest ->
        loop rest offset (StringOrder.Map.add name offset labels)
    | _ :: rest -> loop rest (add offset 4) labels
  in
  loop instructions 0 StringOrder.Map.empty

(*
   Compute symbolic code size and label positions in one instruction-list pass.
*)
let computeSymbolicLayout instructions =
  let rec loop instrs offset labels =
    match instrs with
    | [] -> (offset, labels)
    | Symbolic.Label name :: rest ->
        loop rest offset (StringOrder.Map.add name offset labels)
    | _ :: rest -> loop rest (add offset 4) labels
  in
  loop instructions 0 StringOrder.Map.empty

(*
   Compute the size of code in bytes from symbolic instructions
*)
let getSymbolicCodeSize instructions =
  List.fold_left
    (fun size -> function Symbolic.Label _ -> size | _ -> add size 4)
    0 instructions

type dataOffsets =
  | LiteralOffsets of int StringOrder.Map.t * int LiteralPool.FloatBitsMap.t
  | LabelOffsets of int StringOrder.Map.t * int StringOrder.Map.t

let tryFindDataOffset labelRef offsets namedOffsets =
  match labelRef with
  | Symbolic.DataLabel (Symbolic.StringLiteral value) -> (
      match offsets with
      | LiteralOffsets (stringOffsets, _) ->
          StringOrder.Map.find_opt value stringOffsets
      | LabelOffsets _ -> None)
  | Symbolic.DataLabel (Symbolic.FloatLiteral value) -> (
      match offsets with
      | LiteralOffsets (_, floatOffsets) ->
          LiteralPool.FloatBitsMap.find_opt
            (Int64.bits_of_float value)
            floatOffsets
      | LabelOffsets _ -> None)
  | Symbolic.DataLabel (Symbolic.Named name) ->
      StringOrder.Map.find_opt name namedOffsets
  | Symbolic.CodeLabel name -> (
      match offsets with
      | LiteralOffsets _ -> StringOrder.Map.find_opt name namedOffsets
      | LabelOffsets (strings, floats) ->
          if String.starts_with ~prefix:"str_" name then
            StringOrder.Map.find_opt name strings
          else if String.starts_with ~prefix:"_float" name then
            StringOrder.Map.find_opt name floats
          else StringOrder.Map.find_opt name namedOffsets)

let labelRefDescription = function
  | Symbolic.CodeLabel name | Symbolic.DataLabel (Symbolic.Named name) -> name
  | Symbolic.DataLabel (Symbolic.StringLiteral value) -> value
  | Symbolic.DataLabel (Symbolic.FloatLiteral value) ->
      FloatFormat.roundTrip value

(*
   Encode an instruction with label resolution
   currentOffset: byte offset of current instruction
   Label-based branches - resolve to offsets
   Compute relative offset in instructions (divide by 4)
   Encode as CBZ with immediate offset
   CBZ
   Encode as CBNZ with immediate offset
   CBNZ
   Encode as TBZ with immediate offset
   TBZ
   Encode as TBNZ with immediate offset
   TBNZ
   Encode as B with immediate offset
   B.cond: 01010100 imm19 0 cond
   BL encoding: 1 00101 imm26
   Bit 31 = 1 (distinguishes BL from B which has bit 31 = 0)
   BL opcode (bit 31=1, bits 30-26=00101)
   ADRP: form PC-relative address to 4KB page
   Encoding: 1 immlo(2) 10000 immhi(19) Rd(5)
   The label should point to data in .rodata section
   Compute page-relative offset (4KB pages)
   ADRP uses the page containing PC, so we compute:
   page_offset = ((target & ~0xFFF) - (pc & ~0xFFF)) >> 12
   Encode
   ADRP (bit 31=1)
   ADR: form PC-relative address
   Encoding: 0 immlo(2) 10000 immhi(19) Rd(5)
   immlo is bits 0-1, immhi is bits 2-20 of the 21-bit signed offset
   Compute byte offset from current PC to label
   ADR has a ±1MB range (21-bit signed immediate)
   ADR (bit 31=0, vs ADRP which has bit 31=1)
   ADD with label offset (page offset portion)
   Used with ADRP to get full address
   This adds the lower 12 bits of the address (page offset)
   Get the 12-bit page offset
   Encode as ADD immediate
   64-bit
   ADD immediate opcode
   No shift
   All other instructions: use single-pass encoding
*)
let encodeSymbolicWithLabels instr currentOffset tryFindCodeLabel dataOffsets
    dataLabels =
  match instr with
  | Symbolic.CBZ (reg, label) -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let sf = 1l lsl 31 in
          let op = 0b011010l lsl 25 in
          let flag = 0l lsl 24 in
          let imm19 = (uint32 instrOffset land 0x7FFFFl) lsl 5 in
          let rt = encodeReg reg in
          sf lor op lor flag lor imm19 lor rt
      | None ->
          Crash.crash
            (Printf.sprintf "CBZ: Label '%s' not found in labelMap" label))
  | Symbolic.CBNZ (reg, label) -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let sf = 1l lsl 31 in
          let op = 0b011010l lsl 25 in
          let flag = 1l lsl 24 in
          let imm19 = (uint32 instrOffset land 0x7FFFFl) lsl 5 in
          let rt = encodeReg reg in
          sf lor op lor flag lor imm19 lor rt
      | None ->
          Crash.crash
            (Printf.sprintf "CBNZ: Label '%s' not found in labelMap" label))
  | Symbolic.TBZ_label (reg, bit, label) -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let b5 = (uint32 bit lsr 5) lsl 31 in
          let op = 0b011011l lsl 25 in
          let flag = 0l lsl 24 in
          let b40 = (uint32 bit land 0x1Fl) lsl 19 in
          let imm14 = (uint32 instrOffset land 0x3FFFl) lsl 5 in
          let rt = encodeReg reg in
          b5 lor op lor flag lor b40 lor imm14 lor rt
      | None ->
          Crash.crash
            (Printf.sprintf "TBZ: Label '%s' not found in labelMap" label))
  | Symbolic.TBNZ_label (reg, bit, label) -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let b5 = (uint32 bit lsr 5) lsl 31 in
          let op = 0b011011l lsl 25 in
          let flag = 1l lsl 24 in
          let b40 = (uint32 bit land 0x1Fl) lsl 19 in
          let imm14 = (uint32 instrOffset land 0x3FFFl) lsl 5 in
          let rt = encodeReg reg in
          b5 lor op lor flag lor b40 lor imm14 lor rt
      | None ->
          Crash.crash
            (Printf.sprintf "TBNZ: Label '%s' not found in labelMap" label))
  | Symbolic.B_label label -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let op = 0b000101l lsl 26 in
          let imm26 = uint32 instrOffset land 0x3FFFFFFl in
          op lor imm26
      | None ->
          Crash.crash
            (Printf.sprintf "B: Label '%s' not found in labelMap" label))
  | Symbolic.B_cond_label (cond, label) -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let op = 0b01010100l lsl 24 in
          let imm19 = (uint32 instrOffset land 0x7FFFFl) lsl 5 in
          let condBits =
            match cond with
            | ARM64.EQ -> 0b0000l
            | ARM64.NE -> 0b0001l
            | ARM64.LT -> 0b1011l
            | ARM64.GT -> 0b1100l
            | ARM64.LE -> 0b1101l
            | ARM64.GE -> 0b1010l
            | ARM64.LO -> 0b0011l
            | ARM64.HI -> 0b1000l
            | ARM64.LS -> 0b1001l
            | ARM64.HS -> 0b0010l
          in
          op lor imm19 lor condBits
      | None ->
          Crash.crash
            (Printf.sprintf "B.cond: Label '%s' not found in labelMap" label))
  | Symbolic.BL label -> (
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let instrOffset = byteOffset / 4 in
          let op = 0b100101l lsl 26 in
          let imm26 = uint32 instrOffset land 0x3FFFFFFl in
          op lor imm26
      | None ->
          Crash.crash
            (Printf.sprintf "BL: Label '%s' not found in labelMap" label))
  | Symbolic.Label _ -> Crash.crash "labels do not encode to machine-code words"
  | Symbolic.ADRP (dest, labelRef) -> (
      match tryFindDataOffset labelRef dataOffsets dataLabels with
      | Some targetOffset ->
          let pcPage = Stdlib.( land ) currentOffset (lnot 0xFFF) in
          let targetPage = Stdlib.( land ) targetOffset (lnot 0xFFF) in
          let pageOffset = sub targetPage pcPage / 4096 in
          let rd = encodeReg dest in
          let immlo = (uint32 pageOffset land 0b11l) lsl 29 in
          let immhi = ((uint32 pageOffset lsr 2) land 0x7FFFFl) lsl 5 in
          let op = 1l lsl 31 in
          let opcode = 0b10000l lsl 24 in
          op lor immlo lor opcode lor immhi lor rd
      | None ->
          Crash.crash
            (Printf.sprintf "ADRP: Label '%s' not found in labelMap"
               (labelRefDescription labelRef)))
  | Symbolic.ADR (dest, labelRef) -> (
      let label = labelRefDescription labelRef in
      match tryFindCodeLabel label with
      | Some targetOffset ->
          let byteOffset = sub targetOffset currentOffset in
          let rd = encodeReg dest in
          let immlo = (uint32 byteOffset land 0b11l) lsl 29 in
          let immhi = ((uint32 byteOffset lsr 2) land 0x7FFFFl) lsl 5 in
          let op = 0l lsl 31 in
          let opcode = 0b10000l lsl 24 in
          op lor immlo lor opcode lor immhi lor rd
      | None ->
          Crash.crash
            (Printf.sprintf "ADR: Label '%s' not found in labelMap" label))
  | Symbolic.ADD_label (dest, src, labelRef) -> (
      match tryFindDataOffset labelRef dataOffsets dataLabels with
      | Some targetOffset ->
          let pageOffset = Stdlib.( land ) targetOffset 0xFFF in
          let sf = 1l lsl 31 in
          let op = 0b00100010l lsl 23 in
          let shift = 0l lsl 22 in
          let imm12 = (uint32 pageOffset land 0xFFFl) lsl 10 in
          let rn = encodeReg src lsl 5 in
          let rd = encodeReg dest in
          sf lor op lor shift lor imm12 lor rn lor rd
      | None ->
          Crash.crash
            (Printf.sprintf "ADD_label: Label '%s' not found in labelMap"
               (labelRefDescription labelRef)))
  | _ -> encodeSymbolicWord instr

(*
   Compatibility entry point for concrete-instruction encoder tests.
*)
let encodeWord instr = encodeSymbolicWord (Symbolic.ofARM64 instr)

(*
   Compatibility entry point for direct encoder tests. Production emission
   encodes its existing symbolic instructions without converting them.
*)
let encode instr =
  let symbolicInstr = Symbolic.ofARM64 instr in
  match symbolicInstr with
  | Symbolic.CBZ _ | Symbolic.CBNZ _ | Symbolic.B_label _
  | Symbolic.B_cond_label _ | Symbolic.Label _ | Symbolic.ADRP _
  | Symbolic.ADR _ | Symbolic.ADD_label _ | Symbolic.TBZ_label _
  | Symbolic.TBNZ_label _ | Symbolic.BL _ ->
      []
  | _ -> [ encodeSymbolicWord symbolicInstr ]

(*
   Compatibility entry point for concrete instruction streams. Production
   emission calls encodeSymbolicWithLabels directly.
*)
let encodeWithLabels instr currentOffset codeLabels stringLabels floatLabels
    dataLabels =
  encodeSymbolicWithLabels (Symbolic.ofARM64 instr) currentOffset
    (fun label -> StringOrder.Map.find_opt label codeLabels)
    (LabelOffsets (stringLabels, floatLabels))
    dataLabels

let tryLocalCodeTarget = function
  | Symbolic.CBZ (_, label)
  | Symbolic.CBNZ (_, label)
  | Symbolic.B_label label
  | Symbolic.B_cond_label (_, label)
  | Symbolic.TBZ_label (_, _, label)
  | Symbolic.TBNZ_label (_, _, label)
  | Symbolic.BL label ->
      Some label
  | Symbolic.ADR (_, Symbolic.CodeLabel label) -> Some label
  | _ -> None

let arrayOfQueue values = Array.of_seq (Queue.to_seq values)

let poolRefCollector () =
  let poolLabelRefs = Queue.create () in
  let stringLiterals = Hashtbl.create 16 in
  let floatLiterals = Hashtbl.create 16 in
  let recordPoolLabelRef labelRef =
    match labelRef with
    | Symbolic.DataLabel (Symbolic.StringLiteral value) ->
        if not (Hashtbl.mem stringLiterals value) then (
          Hashtbl.add stringLiterals value ();
          Queue.add labelRef poolLabelRefs)
    | Symbolic.DataLabel (Symbolic.FloatLiteral value) ->
        let bits = Int64.bits_of_float value in
        if not (Hashtbl.mem floatLiterals bits) then (
          Hashtbl.add floatLiterals bits ();
          Queue.add labelRef poolLabelRefs)
    | Symbolic.CodeLabel _ | Symbolic.DataLabel (Symbolic.Named _) -> ()
  in
  (poolLabelRefs, recordPoolLabelRef)

let resolveLocalRelocations machineCodeTemplate codeLabelArray relocations =
  let localCodeLabels = Hashtbl.create 16 in
  Array.iter
    (fun (name, offset) -> Hashtbl.replace localCodeLabels name offset)
    codeLabelArray;
  let tryFindLocalCodeLabel label = Hashtbl.find_opt localCodeLabels label in
  let unresolvedRelocations = Queue.create () in
  Queue.iter
    (fun (wordIndex, instr) ->
      match tryLocalCodeTarget instr with
      | Some label when Hashtbl.mem localCodeLabels label ->
          machineCodeTemplate.(wordIndex) <-
            encodeSymbolicWithLabels instr (mul wordIndex 4)
              tryFindLocalCodeLabel
              (LiteralOffsets
                 (StringOrder.Map.empty, LiteralPool.FloatBitsMap.empty))
              StringOrder.Map.empty
      | _ -> Queue.add (wordIndex, instr) unresolvedRelocations)
    relocations;
  arrayOfQueue unresolvedRelocations

let prepareSymbolicChunk instructions =
  let words = Queue.create () in
  let relocations = Queue.create () in
  let codeLabels = Queue.create () in
  let poolLabelRefs, recordPoolLabelRef = poolRefCollector () in
  let addRelocation instr =
    Queue.add (Queue.length words, instr) relocations;
    Queue.add 0l words
  in
  List.iter
    (fun instr ->
      match instr with
      | Symbolic.Label name ->
          Queue.add (name, mul (Queue.length words) 4) codeLabels
      | Symbolic.ADRP (_, labelRef)
      | Symbolic.ADD_label (_, _, labelRef)
      | Symbolic.ADR (_, labelRef) ->
          recordPoolLabelRef labelRef;
          addRelocation instr
      | Symbolic.CBZ _ | Symbolic.CBNZ _ | Symbolic.B_label _
      | Symbolic.B_cond_label _ | Symbolic.TBZ_label _ | Symbolic.TBNZ_label _
      | Symbolic.BL _ ->
          addRelocation instr
      | _ -> Queue.add (encodeSymbolicWord instr) words)
    instructions;
  let machineCodeTemplate = arrayOfQueue words in
  let codeLabelArray = arrayOfQueue codeLabels in
  let unresolvedRelocations =
    resolveLocalRelocations machineCodeTemplate codeLabelArray relocations
  in
  {
    machineCodeTemplate;
    relocations = unresolvedRelocations;
    codeLabels = codeLabelArray;
    poolLabelRefs = arrayOfQueue poolLabelRefs;
  }

(*
   Compose prepared function chunks into one position-independent group.
   Fixed words are copied from their cached templates, while calls and branches
   whose targets are inside the group are resolved once for every later use of
   that exact group shape.
*)
let combinePreparedChunks = function
  | [] ->
      {
        machineCodeTemplate = [||];
        relocations = [||];
        codeLabels = [||];
        poolLabelRefs = [||];
      }
  | [ chunk ] -> chunk
  | chunks ->
      let wordCount =
        List.fold_left
          (fun total chunk ->
            add total (Array.length chunk.machineCodeTemplate))
          0 chunks
      in
      let machineCodeTemplate = Array.make wordCount 0l in
      let codeLabels = Queue.create () in
      let relocations = Queue.create () in
      let poolLabelRefs, recordPoolLabelRef = poolRefCollector () in
      let _ =
        List.fold_left
          (fun outputIndex chunk ->
            Array.blit chunk.machineCodeTemplate 0 machineCodeTemplate
              outputIndex
              (Array.length chunk.machineCodeTemplate);
            Array.iter
              (fun (name, relativeOffset) ->
                Queue.add
                  (name, add (mul outputIndex 4) relativeOffset)
                  codeLabels)
              chunk.codeLabels;
            Array.iter
              (fun (relativeIndex, instr) ->
                Queue.add (add outputIndex relativeIndex, instr) relocations)
              chunk.relocations;
            Array.iter recordPoolLabelRef chunk.poolLabelRefs;
            add outputIndex (Array.length chunk.machineCodeTemplate))
          0 chunks
      in
      let codeLabelArray = arrayOfQueue codeLabels in
      let unresolvedRelocations =
        resolveLocalRelocations machineCodeTemplate codeLabelArray relocations
      in
      {
        machineCodeTemplate;
        relocations = unresolvedRelocations;
        codeLabels = codeLabelArray;
        poolLabelRefs = arrayOfQueue poolLabelRefs;
      }

(*
   Compute the size of concrete code in bytes.
*)
let getCodeSize instructions =
  List.fold_left
    (fun total -> function ARM64.Label _ -> total | _ -> add total 4)
    0 instructions

(*
   Compute float literal positions given code file offset, code size, and float pool.
   Keys use exact IEEE-754 bits so positive and negative zero remain distinct.
   Floats are stored as 8-byte IEEE 754 doubles, aligned to 8 bytes
   Floats start after headers + code, 8-byte aligned
   Pool arrays retain first-use layout order.
   Each double is 8 bytes
*)
let computeFloatLiteralOffsets codeFileOffset codeSize
    (floatPool : LiteralPool.floatPool) =
  if Array.length floatPool.LiteralPool.floats = 0 then
    LiteralPool.FloatBitsMap.empty
  else
    let startOffset = add codeFileOffset codeSize in
    let alignedStart = Stdlib.( land ) (add startOffset 7) (lnot 7) in
    snd
      (Array.fold_left
         (fun (offset, offsetMap) floatValue ->
           let bits = Int64.bits_of_float floatValue in
           let newMap = LiteralPool.FloatBitsMap.add bits offset offsetMap in
           (add offset 8, newMap))
         (alignedStart, LiteralPool.FloatBitsMap.empty)
         floatPool.LiteralPool.floats)

(*
   Compute string literal positions given code file offset, code size, and string pool.
   codeFileOffset: where code starts in the file/segment
   Compute the size of the float pool in bytes
   Each double is 8 bytes
*)
let getFloatPoolSize (floatPool : LiteralPool.floatPool) =
  mul (Array.length floatPool.LiteralPool.floats) 8

(*
   Strings start after headers + code + floats
   Float pool is 8-byte aligned, so account for alignment
   Each string has format: [refcount:8][length:8][data:N][padding:P]
   Pool arrays retain first-use layout order.
*)
let computeStringLiteralOffsets codeFileOffset codeSize floatPoolSize
    (stringPool : LiteralPool.stringPool) =
  if Array.length stringPool.LiteralPool.strings = 0 then StringOrder.Map.empty
  else
    let floatStart =
      Stdlib.( land ) (add (add codeFileOffset codeSize) 7) (lnot 7)
    in
    let startOffset = add floatStart floatPoolSize in
    snd
      (Array.fold_left
         (fun (offset, offsetMap) (str, len) ->
           let newMap = StringOrder.Map.add str offset offsetMap in
           let alignedLen = mul (add len 7 / 8) 8 in
           (add (add (add offset 8) alignedLen) 8, newMap))
         (startOffset, StringOrder.Map.empty)
         stringPool.LiteralPool.strings)

(*
   Compute the size of the string pool in bytes
   Each string has format: [refcount:8][length:8][data:N][padding:P]
*)
let getStringPoolSize (stringPool : LiteralPool.stringPool) =
  Array.fold_left
    (fun size (_, len) ->
      let alignedLen = mul (add len 7 / 8) 8 in
      add (add (add size 8) alignedLen) 8)
    0 stringPool.LiteralPool.strings

(*
   Compute the platform-specific code file offset for encoding
   ELF: header (64) + 1 program header (56) = 120
   Mach-O: header (32) + load commands + padding
   Must match Binary_Generation_MachO.ml calculation
*)
let computeCodeFileOffset os (stringPool : LiteralPool.stringPool)
    (floatPool : LiteralPool.floatPool) enableLeakCheck =
  match os with
  | Platform.Linux -> 64 + 56
  | Platform.MacOS ->
      let headerSize = 32 in
      let pageZeroCommandSize = 72 in
      let hasData =
        Array.length stringPool.LiteralPool.strings <> 0
        || Array.length floatPool.LiteralPool.floats <> 0
        || enableLeakCheck
      in
      let numTextSections = if hasData then 2 else 1 in
      let textSegmentCommandSize = 72 + (80 * numTextSections) in
      let linkeditSegmentCommandSize = 72 in
      let dylinkerCommandSize = 32 in
      let dylibCommandSize = 56 in
      let symtabCommandSize = 24 in
      let dysymtabCommandSize = 80 in
      let uuidCommandSize = 24 in
      let buildVersionCommandSize = 24 in
      let mainCommandSize = 24 in
      let commandsSize =
        pageZeroCommandSize + textSegmentCommandSize
        + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize
        + symtabCommandSize + dysymtabCommandSize + uuidCommandSize
        + buildVersionCommandSize + mainCommandSize
      in
      Stdlib.( land ) (headerSize + commandsSize + 200 + 7) (lnot 7)

(*
   Compute leak counter label position
   ELF counters occupy a separate page from executable code; Mach-O retains
   its existing word-aligned placement. Binary emission uses the same rule.
*)
let computeLeakCounterLabel os codeFileOffset codeSize floatPoolSize
    stringPoolSize =
  let floatStart =
    Stdlib.( land ) (add (add codeFileOffset codeSize) 7) (lnot 7)
  in
  let stringStart = add floatStart floatPoolSize in
  let dataEnd = add stringStart stringPoolSize in
  let leakStart =
    match os with
    | Platform.Linux -> RuntimeDataLayout.elfCounterOffset dataEnd
    | Platform.MacOS -> Stdlib.( land ) (add dataEnd 7) (lnot 7)
  in
  StringOrder.Map.singleton Symbolic.leakCounterLabelName leakStart

(*
   Encode symbolic instructions with string and float pool support
   Resolves label refs on the fly to avoid allocating a concrete instruction list
   Step 1: Compose cached relative chunk layouts into one program layout.
   This index is transient and receives every code label in every emitted
   executable. A mutable ordinal dictionary avoids allocating a new tree
   path for each insertion and gives relocations constant-time lookup.
   Step 2: Compute float label positions (after headers + code, 8-byte aligned)
   Step 3: Compute string label positions (after headers + code + floats)
   Step 5: Encode with label resolution (current offset includes file offset)
*)
let encodePreparedChunksWithPools chunks stringPool floatPool os enableLeakCheck
    =
  let codeFileOffset =
    computeCodeFileOffset os stringPool floatPool enableLeakCheck
  in
  let codeLabelMap = Hashtbl.create 16 in
  let codeSize = ref 0 in
  List.iter
    (fun chunk ->
      Array.iter
        (fun (name, relativeOffset) ->
          Hashtbl.replace codeLabelMap name
            (add (add codeFileOffset !codeSize) relativeOffset))
        chunk.codeLabels;
      codeSize := add !codeSize (mul (Array.length chunk.machineCodeTemplate) 4))
    chunks;
  let floatOffsets =
    computeFloatLiteralOffsets codeFileOffset !codeSize floatPool
  in
  let floatPoolSize = getFloatPoolSize floatPool in
  let stringOffsets =
    computeStringLiteralOffsets codeFileOffset !codeSize floatPoolSize
      stringPool
  in
  let stringPoolSize = getStringPoolSize stringPool in
  let leakLabels =
    if enableLeakCheck then
      computeLeakCounterLabel os codeFileOffset !codeSize floatPoolSize
        stringPoolSize
    else StringOrder.Map.empty
  in
  let dataLabels = leakLabels in
  let dataOffsets = LiteralOffsets (stringOffsets, floatOffsets) in
  let tryFindCodeLabel label = Hashtbl.find_opt codeLabelMap label in
  let encoded = Array.make (!codeSize / 4) 0l in
  let rec encodeChunks remaining outputIndex =
    match remaining with
    | [] -> encoded
    | chunk :: rest ->
        Array.blit chunk.machineCodeTemplate 0 encoded outputIndex
          (Array.length chunk.machineCodeTemplate);
        Array.iter
          (fun (relativeIndex, instr) ->
            let absoluteIndex = add outputIndex relativeIndex in
            encoded.(absoluteIndex) <-
              encodeSymbolicWithLabels instr
                (add codeFileOffset (mul absoluteIndex 4))
                tryFindCodeLabel dataOffsets dataLabels)
          chunk.relocations;
        encodeChunks rest
          (add outputIndex (Array.length chunk.machineCodeTemplate))
  in
  encodeChunks chunks 0

(*
   Encode one symbolic stream through the same prepared-chunk path used by
   production emission.
*)
let encodeSymbolicWithPools instructions stringPool floatPool os enableLeakCheck
    =
  encodePreparedChunksWithPools
    [ prepareSymbolicChunk instructions ]
    stringPool floatPool os enableLeakCheck

(*
   Compatibility entry point for concrete instruction streams. The compiler's
   production path keeps instructions symbolic through encoding.
*)
let encodeAllWithPools instructions stringPool floatPool os enableLeakCheck =
  encodeSymbolicWithPools
    (List.map Symbolic.ofARM64 instructions)
    stringPool floatPool os enableLeakCheck
