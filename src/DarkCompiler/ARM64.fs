module ARM64

/// ARM64 registers (subset for now)
type Reg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15
    | X29 | X30 | SP

/// ARM64 instruction encoding
type Instr =
    | MOVZ of dest:Reg * imm:uint16 * shift:int  // Move with zero
    | MOVK of dest:Reg * imm:uint16 * shift:int  // Move with keep
    | ADD_imm of dest:Reg * src:Reg * imm:uint16
    | ADD_reg of dest:Reg * src1:Reg * src2:Reg
    | SUB_imm of dest:Reg * src:Reg * imm:uint16
    | SUB_reg of dest:Reg * src1:Reg * src2:Reg
    | MUL of dest:Reg * src1:Reg * src2:Reg
    | SDIV of dest:Reg * src1:Reg * src2:Reg
    | MOV_reg of dest:Reg * src:Reg
    | RET

/// Machine code (32-bit instruction)
type MachineCode = uint32

/// Encode register to 5-bit value
let encodeReg (reg: Reg) : uint32 =
    match reg with
    | X0 -> 0u | X1 -> 1u | X2 -> 2u | X3 -> 3u
    | X4 -> 4u | X5 -> 5u | X6 -> 6u | X7 -> 7u
    | X8 -> 8u | X9 -> 9u | X10 -> 10u | X11 -> 11u
    | X12 -> 12u | X13 -> 13u | X14 -> 14u | X15 -> 15u
    | X29 -> 29u | X30 -> 30u | SP -> 31u

/// Encode ARM64 instruction to 32-bit machine code
let encode (instr: Instr) : MachineCode list =
    match instr with
    | MOVZ (dest, imm, shift) ->
        // MOVZ encoding: sf=1 opc=10 100101 hw imm16 Rd
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 2u <<< 29
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | MOVK (dest, imm, shift) ->
        // MOVK encoding: sf=1 opc=11 100101 hw imm16 Rd
        // Bits: sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
        let sf = 1u <<< 31
        let opc = 3u <<< 29
        let opcode = 0b100101u <<< 23
        let hw = (uint32 shift / 16u) <<< 21
        let imm16 = (uint32 imm) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| opcode ||| hw ||| imm16 ||| rd]

    | ADD_imm (dest, src, imm) ->
        // ADD immediate: sf=1 0 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b10001u <<< 24
        let shift = 0u <<< 22  // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]

    | ADD_reg (dest, src1, src2) ->
        // ADD register: sf=1 0 0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b01011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | SUB_imm (dest, src, imm) ->
        // SUB immediate: sf=1 1 0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b110001u <<< 23
        let shift = 0u <<< 22
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| shift ||| imm12 ||| rn ||| rd]

    | SUB_reg (dest, src1, src2) ->
        // SUB register: sf=1 1 0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b101011u <<< 24
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | MUL (dest, src1, src2) ->
        // MADD: sf=1 0 0 11011 000 Rm(5) 0 Ra=11111 Rn(5) Rd(5)
        // Using Ra=XZR(31) for pure multiply
        let sf = 1u <<< 31
        let op = 0b11011000u <<< 21
        let rm = (encodeReg src2) <<< 16
        let ra = 31u <<< 10  // XZR
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| ra ||| rn ||| rd]

    | SDIV (dest, src1, src2) ->
        // SDIV: sf=1 0 0 11010110 Rm(5) 000010 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b11010110u <<< 21
        let rm = (encodeReg src2) <<< 16
        let fixedBits = 0b000010u <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | MOV_reg (dest, src) ->
        // MOV is ORR with XZR: sf=1 01 01010 00 0 Rm(5) 000000 Rn=11111 Rd(5)
        let sf = 1u <<< 31
        let op = 0b01010100u <<< 21
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| rn ||| rd]

    | RET ->
        // RET: 1101011 0 0 10 11111 0000 0 0 Rn=11110 00000
        // Default RET uses X30 (link register)
        [0xD65F03C0u]
