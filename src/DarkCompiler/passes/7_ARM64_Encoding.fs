// 7_ARM64_Encoding.fs - ARM64 Machine Code Encoding (Pass 7)
//
// Encodes ARM64 instructions to 32-bit machine code per ARMv8 specification.
//
// Encoding algorithm:
// - Two-pass encoding for label-based branches:
//   Pass 1: Compute label positions (byte offsets)
//   Pass 2: Encode with computed branch offsets
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
        // SUB immediate: sf=1 op=1 S=0 10001 shift(2) imm12(12) Rn(5) Rd(5)
        let sf = 1u <<< 31          // 64-bit operation
        let op = 1u <<< 30          // SUB (vs ADD which has op=0)
        let s = 0u <<< 29           // Don't set flags
        let opcode = 0b10001u <<< 24 // Fixed opcode bits
        let shift = 0u <<< 22       // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.SUB_reg (dest, src1, src2) ->
        // SUB register: sf=1 op=1 S=0 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        // Bits: sf(31) op(30) S(29) 01011(28-24) shift(23-22) 0(21) Rm(20-16) imm6(15-10) Rn(9-5) Rd(4-0)
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // Subtract (not add)
        let s = 0u <<< 29   // Don't set flags (use SUB not SUBS)
        let opcode = 0b01011u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| rm ||| rn ||| rd]

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
        // SDIV: sf=1 0 0 11010110 Rm(5) 000011 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b11010110u <<< 21
        let rm = (encodeReg src2) <<< 16
        let fixedBits = 0b000011u <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.UDIV (dest, src1, src2) ->
        // UDIV: sf=1 0 0 11010110 Rm(5) 000010 Rn(5) Rd(5)
        let sf = 1u <<< 31
        let op = 0b11010110u <<< 21
        let rm = (encodeReg src2) <<< 16
        let fixedBits = 0b000010u <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| fixedBits ||| rn ||| rd]

    | ARM64.MSUB (dest, src1, src2, src3) ->
        // MSUB: sf=1 0 0 11011 000 Rm(5) 1 Ra(5) Rn(5) Rd(5)
        // Computes: Rd = Ra - (Rn * Rm)
        let sf = 1u <<< 31
        let op = 0b11011000u <<< 21
        let rm = (encodeReg src2) <<< 16
        let flagBit = 1u <<< 15  // Distinguishes MSUB from MADD
        let ra = (encodeReg src3) <<< 10
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| op ||| rm ||| flagBit ||| ra ||| rn ||| rd]

    | ARM64.MOV_reg (dest, src) ->
        // MOV is ORR with XZR: sf=1 01 01010 00 0 Rm(5) 000000 Rn=11111 Rd(5)
        // Bit 31: sf=1, Bit 30: 0, Bit 29: 1 (ORR not AND), Bits 28-21: 01010000
        let sf = 1u <<< 31
        let opc = 1u <<< 29  // Critical: bit 29 distinguishes ORR (1) from AND (0)
        let op = 0b01010000u <<< 21
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| rm ||| rn ||| rd]

    | ARM64.STRB (src, addr, offset) ->
        // STRB immediate unsigned offset: 00 111 001 00 imm12 Rn Rt
        // Size=00 (byte), opc=00, bit24=1 for unsigned offset mode
        let size = 0u <<< 30  // Byte operation
        let vOpc = 0b11100100u <<< 22  // Fixed bits for STRB unsigned offset (bit 24 = 1)
        let imm12 = (uint32 offset &&& 0xFFFu) <<< 10
        let rn = (encodeReg addr) <<< 5
        let rt = encodeReg src
        [size ||| vOpc ||| imm12 ||| rn ||| rt]

    // Label-based branches - resolved via two-pass encoding (see encodeWithLabels)
    // These return empty here because they are only used during label resolution
    | ARM64.CBZ (reg, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.CBNZ (reg, label) ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.B_label label ->
        // Resolved in encodeWithLabels with computed label offsets
        []

    | ARM64.Label label ->
        // Pseudo-instruction: marks a label position (no machine code generated)
        []

    // Offset-based branches (for handcrafted runtime code with known offsets)
    | ARM64.CBZ_offset (reg, offset) ->
        // CBZ: sf 011010 0 imm19 Rt
        // Compare and Branch on Zero
        let sf = 1u <<< 31  // 64-bit register
        let op = 0b011010u <<< 25
        let flag = 0u <<< 24  // CBZ (vs CBNZ which has 1)
        // Offset is in instructions (4-byte units), sign-extended, stored as imm19
        let imm19 = ((uint32 offset) &&& 0x7FFFFu) <<< 5
        let rt = encodeReg reg
        [sf ||| op ||| flag ||| imm19 ||| rt]

    | ARM64.CBNZ_offset (reg, offset) ->
        // CBNZ: sf 011010 1 imm19 Rt
        // Compare and Branch on Non-Zero
        let sf = 1u <<< 31  // 64-bit register
        let op = 0b011010u <<< 25
        let flag = 1u <<< 24  // CBNZ (vs CBZ which has 0)
        // Offset is in instructions (4-byte units), sign-extended, stored as imm19
        let imm19 = ((uint32 offset) &&& 0x7FFFFu) <<< 5
        let rt = encodeReg reg
        [sf ||| op ||| flag ||| imm19 ||| rt]

    | ARM64.TBNZ (reg, bit, offset) ->
        // TBNZ: b5 011011 1 b40 imm14 Rt
        // Test bit and Branch if Not Zero
        // b5 = bit[5], b40 = bit[4:0]
        let b5 = (uint32 bit >>> 5) <<< 31
        let op = 0b011011u <<< 25
        let flag = 1u <<< 24  // TBNZ (vs TBZ which has 0)
        let b40 = (uint32 bit &&& 0x1Fu) <<< 19
        let imm14 = ((uint32 offset) &&& 0x3FFFu) <<< 5
        let rt = encodeReg reg
        [b5 ||| op ||| flag ||| b40 ||| imm14 ||| rt]

    | ARM64.B offset ->
        // B: 000101 imm26
        // Unconditional branch
        // Offset is in instructions (4-byte units), sign-extended
        let op = 0b000101u <<< 26
        let imm26 = (uint32 offset) &&& 0x3FFFFFFu
        [op ||| imm26]

    | ARM64.CMP_imm (src, imm) ->
        // CMP immediate is SUBS XZR, Rn, #imm (SUB with set flags, dest=XZR)
        // Encoding: sf=1 op=1 S=1 10001 shift(2) imm12(12) Rn(5) Rd=11111
        let sf = 1u <<< 31          // 64-bit operation
        let op = 1u <<< 30          // SUB (vs ADD)
        let s = 1u <<< 29           // Set flags (critical for CMP)
        let opcode = 0b10001u <<< 24
        let shift = 0u <<< 22       // No shift
        let imm12 = (uint32 imm) <<< 10
        let rn = (encodeReg src) <<< 5
        let rd = 31u                // XZR (discard result, only flags matter)
        [sf ||| op ||| s ||| opcode ||| shift ||| imm12 ||| rn ||| rd]

    | ARM64.CMP_reg (src1, src2) ->
        // CMP register is SUBS XZR, Rn, Rm (SUB with set flags, dest=XZR)
        // Encoding: sf=1 op=1 S=1 01011 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd=11111
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // SUB
        let s = 1u <<< 29   // Set flags (critical for CMP)
        let opcode = 0b01011u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = 31u        // XZR (discard result)
        [sf ||| op ||| s ||| opcode ||| shift ||| rm ||| rn ||| rd]

    | ARM64.CSET (dest, cond) ->
        // CSET Rd, cond is CSINC Rd, XZR, XZR, invert(cond)
        // Encoding: sf=1 op=0 S=0 11010100 Rm=11111 cond(4) 01 Rn=11111 Rd(5)
        let sf = 1u <<< 31
        let op = 0u <<< 30
        let s = 0u <<< 29
        let opcode = 0b11010100u <<< 21
        let rm = 31u <<< 16  // XZR
        // Invert condition for CSINC
        let condCode =
            match cond with
            | ARM64.EQ -> 0b0001u  // Inverted from NE
            | ARM64.NE -> 0b0000u  // Inverted from EQ
            | ARM64.LT -> 0b1010u  // Inverted from GE
            | ARM64.GT -> 0b1101u  // Inverted from LE
            | ARM64.LE -> 0b1100u  // Inverted from GT
            | ARM64.GE -> 0b1011u  // Inverted from LT
        let condBits = condCode <<< 12
        let fixedBits = 0b01u <<< 10  // CSINC vs CSEL
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| rm ||| condBits ||| fixedBits ||| rn ||| rd]

    | ARM64.AND_reg (dest, src1, src2) ->
        // AND register: sf=1 opc=00 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 0u <<< 29  // AND (vs ORR which has opc=01)
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| rm ||| rn ||| rd]

    | ARM64.ORR_reg (dest, src1, src2) ->
        // ORR register: sf=1 opc=01 01010 shift=00 0 Rm(5) imm6=000000 Rn(5) Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 1u <<< 29  // ORR
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src2) <<< 16
        let rn = (encodeReg src1) <<< 5
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| rm ||| rn ||| rd]

    | ARM64.MVN (dest, src) ->
        // MVN is ORN Rd, XZR, Rm (OR NOT with Rn=XZR)
        // Encoding: sf=1 opc=01 01010 shift=00 1 Rm(5) imm6=000000 Rn=11111 Rd(5)
        let sf = 1u <<< 31  // 64-bit
        let opc = 1u <<< 29  // ORR-family
        let op = 0b01010u <<< 24
        let shift = 0u <<< 22  // No shift
        let n = 1u <<< 21  // NOT bit (distinguishes ORN from ORR)
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| opc ||| op ||| shift ||| n ||| rm ||| rn ||| rd]

    | ARM64.NEG (dest, src) ->
        // NEG: SUB dest, XZR, src
        // Encoding: sf=1 op=1 S=0 01011 shift=00 0 Rm(src) imm6=000000 Rn=11111(XZR) Rd(dest)
        let sf = 1u <<< 31  // 64-bit
        let op = 1u <<< 30  // Subtract
        let s = 0u <<< 29   // Don't set flags
        let opcode = 0b01011u <<< 24
        let shift = 0u <<< 22  // No shift
        let rm = (encodeReg src) <<< 16
        let rn = 31u <<< 5  // XZR
        let rd = encodeReg dest
        [sf ||| op ||| s ||| opcode ||| shift ||| rm ||| rn ||| rd]

    | ARM64.RET ->
        // RET: 1101011 0 0 10 11111 0000 0 0 Rn=11110 00000
        // Default RET uses X30 (link register)
        [0xD65F03C0u]

    | ARM64.SVC imm ->
        // SVC: 11010100 000 imm16 000 01
        // Bits: 11010100000(31-21) imm16(20-5) 00001(4-0)
        let imm16 = uint32 imm
        [0xD4000001u ||| (imm16 <<< 5)]

/// Two-Pass Encoding for Label Resolution

/// Pass 1: Compute byte offset for each label
/// Returns map from label name to byte offset
/// Note: Labels mark positions but don't generate code, so we track actual code offset
let computeLabelPositions (instructions: ARM64.Instr list) : Map<string, int> =
    let rec loop instrs offset labelMap =
        match instrs with
        | [] -> labelMap
        | instr :: rest ->
            match instr with
            | ARM64.Label name ->
                // Record this label's position, don't increment offset (pseudo-instruction)
                loop rest offset (Map.add name offset labelMap)
            | ARM64.CBZ _ | ARM64.CBNZ _ | ARM64.B_label _ ->
                // These will be resolved in pass 2, each is 4 bytes
                loop rest (offset + 4) labelMap
            | _ ->
                // Encode instruction to get actual size
                let machineCode = encode instr
                let size = List.length machineCode * 4
                loop rest (offset + size) labelMap
    loop instructions 0 Map.empty

/// Encode an instruction with label resolution
/// currentOffset: byte offset of current instruction
/// labelMap: map of label names to byte offsets
let encodeWithLabels (instr: ARM64.Instr) (currentOffset: int) (labelMap: Map<string, int>) : ARM64.MachineCode list =
    match instr with
    // Label-based branches - resolve to offsets
    | ARM64.CBZ (reg, label) ->
        match Map.tryFind label labelMap with
        | Some targetOffset ->
            // Compute relative offset in instructions (divide by 4)
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as CBZ with immediate offset
            let sf = 1u <<< 31
            let op = 0b011010u <<< 25
            let flag = 0u <<< 24  // CBZ
            let imm19 = ((uint32 instrOffset) &&& 0x7FFFFu) <<< 5
            let rt = encodeReg reg
            [sf ||| op ||| flag ||| imm19 ||| rt]
        | None ->
            // Label not found - return empty (error will be caught later)
            []

    | ARM64.CBNZ (reg, label) ->
        match Map.tryFind label labelMap with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as CBNZ with immediate offset
            let sf = 1u <<< 31
            let op = 0b011010u <<< 25
            let flag = 1u <<< 24  // CBNZ
            let imm19 = ((uint32 instrOffset) &&& 0x7FFFFu) <<< 5
            let rt = encodeReg reg
            [sf ||| op ||| flag ||| imm19 ||| rt]
        | None ->
            []

    | ARM64.B_label label ->
        match Map.tryFind label labelMap with
        | Some targetOffset ->
            let byteOffset = targetOffset - currentOffset
            let instrOffset = byteOffset / 4
            // Encode as B with immediate offset
            let op = 0b000101u <<< 26
            let imm26 = (uint32 instrOffset) &&& 0x3FFFFFFu
            [op ||| imm26]
        | None ->
            []

    | ARM64.Label _ ->
        // Pseudo-instruction: no machine code
        []

    // All other instructions: use single-pass encoding
    | _ ->
        encode instr

/// Main encoding entry point with two-pass label resolution
let encodeAll (instructions: ARM64.Instr list) : ARM64.MachineCode list =
    // Pass 1: Compute label positions
    let labelMap = computeLabelPositions instructions

    // Pass 2: Encode with label resolution
    let rec encodeLoop instrs offset acc =
        match instrs with
        | [] -> List.rev acc
        | instr :: rest ->
            let machineCode = encodeWithLabels instr offset labelMap
            let newOffset = offset + (List.length machineCode * 4)
            encodeLoop rest newOffset (List.rev machineCode @ acc)

    encodeLoop instructions 0 []
