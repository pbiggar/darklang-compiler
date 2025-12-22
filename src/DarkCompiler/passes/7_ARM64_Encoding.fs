// 7_ARM64_Encoding.fs - ARM64 Machine Code Encoding (Pass 7)
//
// Encodes ARM64 instructions to 32-bit machine code per ARMv8 specification.
//
// Encoding algorithm:
// - Encodes registers as 5-bit fields
// - Packs immediates into instruction-specific bit positions
// - Combines opcode bits, operand fields into 32-bit words
// - Each instruction has unique bit layout defined by ARMv8
//
// Example:
//   MOVZ X0, #42, LSL #0  →  0xD2800540
//   ADD X1, X0, #5        →  0x91001401
//   RET                   →  0xD65F03C0

module ARM64_Encoding

/// Encode register to 5-bit value
let encodeReg (reg: ARM64.Reg) : uint32 =
    match reg with
    | ARM64.X0 -> 0u | ARM64.X1 -> 1u | ARM64.X2 -> 2u | ARM64.X3 -> 3u
    | ARM64.X4 -> 4u | ARM64.X5 -> 5u | ARM64.X6 -> 6u | ARM64.X7 -> 7u
    | ARM64.X8 -> 8u | ARM64.X9 -> 9u | ARM64.X10 -> 10u | ARM64.X11 -> 11u
    | ARM64.X12 -> 12u | ARM64.X13 -> 13u | ARM64.X14 -> 14u | ARM64.X15 -> 15u
    | ARM64.X16 -> 16u
    | ARM64.X29 -> 29u | ARM64.X30 -> 30u | ARM64.SP -> 31u

/// Encode ARM64 instruction to 32-bit machine code
let encode (instr: ARM64.Instr) : ARM64.MachineCode list =
    match instr with
    | ARM64.MOVZ (dest, imm, shift) ->
        // MOVZ encoding: sf=1 opc=10 100101 hw imm16 Rd
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 2u <<< 29
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | ARM64.MOVK (dest, imm, shift) ->
        // MOVK encoding: sf=1 opc=11 100101 hw imm16 Rd
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 3u <<< 29
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | ARM64.ADD_imm (dest, src, imm) ->
        // ADD immediate: sf=1 0 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b10001u <<< 24
        let shift = 0u <<< 22  // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.ADD_reg (dest, src1, src2) ->
        // ADD register: sf=1 0 0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b01011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | ARM64.SUB_imm (dest, src, imm) ->
        // SUB immediate: sf=1 1 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b110001u <<< 23
        let shift = 0u <<< 22
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.SUB_reg (dest, src1, src2) ->
        // SUB register: sf=1 1 0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b101011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | ARM64.MUL (dest, src1, src2) ->
        // MADD: sf=1 0 0 11011 000 Rm(5) 0 Ra=11111 Rn(5) Rd(5)
        // Using Ra=XZR(31) for pure multiply
        let sf = 1u <<< 31
        let op = 0b11011000u <<< 21
        let rm = (encodeReg src2) <<< 16
        let ra = 31u <<< 10  // XZR
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| ra ||| rn ||| rd]

    | ARM64.SDIV (dest, src1, src2) ->
        // SDIV: sf=1 0 0 11010110 Rm(5) 000010 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b11010110u <<< 21
        let rm = (encodeReg src2) <<< 16
        let fixedBits = 0b000010u <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.MOV_reg (dest, src) ->
        // MOV is ORR with XZR: sf=1 01 01010 00 0 Rm(5) 000000 Rn=11111 Rd(5)
        let sf = 1u <<< 31
        let op = 0b01010100u <<< 21
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | ARM64.RET ->
        // RET: 1101011 0 0 10 11111 0000 0 0 Rn=11110 00000
        // Default RET uses X30 (link register)
        [0xD65F03C0u]

    | ARM64.SVC imm ->
        // SVC: 11010100 000 imm16 000 01
        // Bits: 11010100000(31-21) imm16(20-5) 00001(4-0)
        let imm16 = uint32 imm
        [0xD4000001u ||| (imm16 <<< 5)]
