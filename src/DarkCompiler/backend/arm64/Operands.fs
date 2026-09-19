// Operands.fs - Materialize target operands, registers, and immediates.

module ARM64Operands

open ARM64HeapAllocation

/// Convert LIR.PhysReg to ARM64Symbolic.Reg
let lirPhysRegToARM64Reg (physReg: LIR.PhysReg) : ARM64Symbolic.Reg =
    match physReg with
    | LIR.X0 -> ARM64Symbolic.X0
    | LIR.X1 -> ARM64Symbolic.X1
    | LIR.X2 -> ARM64Symbolic.X2
    | LIR.X3 -> ARM64Symbolic.X3
    | LIR.X4 -> ARM64Symbolic.X4
    | LIR.X5 -> ARM64Symbolic.X5
    | LIR.X6 -> ARM64Symbolic.X6
    | LIR.X7 -> ARM64Symbolic.X7
    | LIR.X8 -> ARM64Symbolic.X8
    | LIR.X9 -> ARM64Symbolic.X9
    | LIR.X10 -> ARM64Symbolic.X10
    | LIR.X11 -> ARM64Symbolic.X11
    | LIR.X12 -> ARM64Symbolic.X12
    | LIR.X13 -> ARM64Symbolic.X13
    | LIR.X14 -> ARM64Symbolic.X14
    | LIR.X15 -> ARM64Symbolic.X15
    | LIR.X16 -> ARM64Symbolic.X16
    | LIR.X17 -> ARM64Symbolic.X17
    | LIR.X19 -> ARM64Symbolic.X19
    | LIR.X20 -> ARM64Symbolic.X20
    | LIR.X21 -> ARM64Symbolic.X21
    | LIR.X22 -> ARM64Symbolic.X22
    | LIR.X23 -> ARM64Symbolic.X23
    | LIR.X24 -> ARM64Symbolic.X24
    | LIR.X25 -> ARM64Symbolic.X25
    | LIR.X26 -> ARM64Symbolic.X26
    | LIR.X27 -> ARM64Symbolic.X27
    | LIR.X29 -> ARM64Symbolic.X29
    | LIR.X30 -> ARM64Symbolic.X30
    | LIR.SP -> ARM64Symbolic.SP

/// Convert LIR.PhysFPReg to ARM64Symbolic.FReg
let lirPhysFPRegToARM64FReg (physReg: LIR.PhysFPReg) : ARM64Symbolic.FReg =
    match physReg with
    | LIR.D0 -> ARM64Symbolic.D0
    | LIR.D1 -> ARM64Symbolic.D1
    | LIR.D2 -> ARM64Symbolic.D2
    | LIR.D3 -> ARM64Symbolic.D3
    | LIR.D4 -> ARM64Symbolic.D4
    | LIR.D5 -> ARM64Symbolic.D5
    | LIR.D6 -> ARM64Symbolic.D6
    | LIR.D7 -> ARM64Symbolic.D7
    | LIR.D8 -> ARM64Symbolic.D8
    | LIR.D9 -> ARM64Symbolic.D9
    | LIR.D10 -> ARM64Symbolic.D10
    | LIR.D11 -> ARM64Symbolic.D11
    | LIR.D12 -> ARM64Symbolic.D12
    | LIR.D13 -> ARM64Symbolic.D13
    | LIR.D14 -> ARM64Symbolic.D14
    | LIR.D15 -> ARM64Symbolic.D15

/// Convert LIR.FReg to ARM64Symbolic.FReg
/// For FVirtual, we use a two-tier allocation scheme to avoid collisions:
/// - FVirtual 1000 -> D18 (left operand temp for binary ops)
/// - FVirtual 1001 -> D17 (right operand temp for binary ops)
/// - FVirtual 1002 -> D27 (third operand temp for fused operations)
/// - FVirtual 3000-3007 -> D14-D15 (temps for float call args)
/// - FVirtual 0-7 -> D2-D9 (dedicated 1:1 mapping for parameters)
/// - FVirtual 8+ -> D10-D13 (4 temps with modulo, for SSA temps and locals)
///
/// The two-tier scheme ensures that parameter VRegs (0-7) never collide with
/// SSA-generated temps (which have high IDs like 12001). Parameters get D2-D9,
/// while temps get D10-D13 with modulo 4.
let lirFRegToARM64FReg (freg: LIR.FReg) : Result<ARM64Symbolic.FReg, string> =
    match freg with
    | LIR.FPhysical physReg -> Ok (lirPhysFPRegToARM64FReg physReg)
    // Special temp registers for specific purposes
    | LIR.FVirtual 1000 -> Ok ARM64Symbolic.D18  // Left temp for binary ops
    | LIR.FVirtual 1001 -> Ok ARM64Symbolic.D17  // Right temp for binary ops
    | LIR.FVirtual 1002 -> Ok ARM64Symbolic.D27  // Third temp for fused operations
    | LIR.FVirtual 2000 -> Ok ARM64Symbolic.D16  // Reserved scratch for FPhi cycles and runtime helpers
    | LIR.FVirtual n when n >= 3000 && n < 4000 ->
        // Temps for float call arguments - use D19-D26 (8 registers)
        // These must not collide with each other since up to 8 floats
        // can be loaded before FArgMoves. Using D19-D26 avoids collision
        // with argument regs D0-D7, parameter VRegs D2-D9, SSA temps D10-D13,
        // and binary op temps D17-D18.
        let tempIdx = (n - 3000) % 8
        match tempIdx with
        | 0 -> Ok ARM64Symbolic.D19
        | 1 -> Ok ARM64Symbolic.D20
        | 2 -> Ok ARM64Symbolic.D21
        | 3 -> Ok ARM64Symbolic.D22
        | 4 -> Ok ARM64Symbolic.D23
        | 5 -> Ok ARM64Symbolic.D24
        | 6 -> Ok ARM64Symbolic.D25
        | _ -> Ok ARM64Symbolic.D26
    | LIR.FVirtual n when n >= 0 && n <= 7 ->
        // Parameters (VRegs 0-7) get dedicated D2-D9 mapping
        // This prevents collisions with SSA-generated temps
        let physReg =
            match n with
            | 0 -> ARM64Symbolic.D2
            | 1 -> ARM64Symbolic.D3
            | 2 -> ARM64Symbolic.D4
            | 3 -> ARM64Symbolic.D5
            | 4 -> ARM64Symbolic.D6
            | 5 -> ARM64Symbolic.D7
            | 6 -> ARM64Symbolic.D8
            | _ -> ARM64Symbolic.D9
        Ok physReg
    | LIR.FVirtual n when n < 10000 ->
        // ANF-level VRegs (8-9999): function params and local bindings
        // These come from ANF TempIds which are sequential across functions.
        // Pool: D0, D1, D10-D15, D27-D31 (13 registers)
        // Using direct index: (n - 8) % 13
        let tempRegs = [| ARM64Symbolic.D0; ARM64Symbolic.D1; ARM64Symbolic.D10; ARM64Symbolic.D11; ARM64Symbolic.D12; ARM64Symbolic.D13; ARM64Symbolic.D14; ARM64Symbolic.D15;
                          ARM64Symbolic.D27; ARM64Symbolic.D28; ARM64Symbolic.D29; ARM64Symbolic.D30; ARM64Symbolic.D31 |]
        let regIdx = (n - 8) % tempRegs.Length
        Ok tempRegs.[regIdx]
    | LIR.FVirtual n ->
        // MIR intermediates (VRegs 10000+): computation temps from freshReg
        // Use same pool but with offset to reduce collisions with ANF-level VRegs
        // The offset of 7 ensures that if ANF VReg k and MIR VReg (10000+k) exist,
        // they map to different registers (since 7 and 13 are coprime)
        let tempRegs = [| ARM64Symbolic.D0; ARM64Symbolic.D1; ARM64Symbolic.D10; ARM64Symbolic.D11; ARM64Symbolic.D12; ARM64Symbolic.D13; ARM64Symbolic.D14; ARM64Symbolic.D15;
                          ARM64Symbolic.D27; ARM64Symbolic.D28; ARM64Symbolic.D29; ARM64Symbolic.D30; ARM64Symbolic.D31 |]
        let regIdx = ((n - 10000) + 7) % tempRegs.Length
        Ok tempRegs.[regIdx]

/// Convert LIR.Reg to ARM64Symbolic.Reg (assumes physical registers only)
let lirRegToARM64Reg (reg: LIR.Reg) : Result<ARM64Symbolic.Reg, string> =
    match reg with
    | LIR.Physical physReg -> Ok (lirPhysRegToARM64Reg physReg)
    | LIR.Virtual vreg -> Error $"Virtual register {vreg} should have been allocated"

/// Convert LIR.Reg (Virtual) to LIR.FReg (FVirtual) for float HeapStore
/// This is used when a float value is stored via HeapStore - the register
/// ID is shared between Virtual and FVirtual address spaces
let virtualToFVirtual (reg: LIR.Reg) : LIR.FReg =
    match reg with
    | LIR.Virtual n -> LIR.FVirtual n
    | LIR.Physical p -> LIR.FPhysical (
        // Map GP physical registers to FP physical registers for edge cases
        match p with
        | LIR.X0 -> LIR.D0 | LIR.X1 -> LIR.D1 | LIR.X2 -> LIR.D2 | LIR.X3 -> LIR.D3
        | LIR.X4 -> LIR.D4 | LIR.X5 -> LIR.D5 | LIR.X6 -> LIR.D6 | LIR.X7 -> LIR.D7
        | _ -> LIR.D15)

/// Generate ARM64 instructions to load an immediate into a register
let loadImmediate (dest: ARM64Symbolic.Reg) (value: int64) : ARM64Symbolic.Instr list =
    // Load 64-bit immediate using MOVZ/MOVN + MOVK sequence
    // For negative numbers, MOVN (move NOT) can be more efficient

    // Extract each 16-bit chunk
    let chunk0 = uint16 (value >>> 0) &&& 0xFFFFus
    let chunk1 = uint16 (value >>> 16) &&& 0xFFFFus
    let chunk2 = uint16 (value >>> 32) &&& 0xFFFFus
    let chunk3 = uint16 (value >>> 48) &&& 0xFFFFus

    // Count how many chunks are all-zeros vs all-ones
    let zeroCount =
        (if chunk0 = 0us then 1 else 0) +
        (if chunk1 = 0us then 1 else 0) +
        (if chunk2 = 0us then 1 else 0) +
        (if chunk3 = 0us then 1 else 0)
    let onesCount =
        (if chunk0 = 0xFFFFus then 1 else 0) +
        (if chunk1 = 0xFFFFus then 1 else 0) +
        (if chunk2 = 0xFFFFus then 1 else 0) +
        (if chunk3 = 0xFFFFus then 1 else 0)

    // Use MOVN if more chunks are 0xFFFF (inverted gives more zeros)
    if onesCount > zeroCount then
        // Use MOVN: start with first non-0xFFFF chunk, then MOVK for remaining non-0xFFFF chunks
        // MOVN Xd, #imm, LSL #shift sets Xd = NOT(imm << shift), filling rest with 1s
        // Find first chunk that is NOT 0xFFFF (so inverting gives a meaningful value)
        let chunks = [(chunk0, 0); (chunk1, 16); (chunk2, 32); (chunk3, 48)]
        let firstNonOnes = chunks |> List.tryFind (fun (c, _) -> c <> 0xFFFFus)
        match firstNonOnes with
        | Some (firstChunk, firstShift) ->
            // Start with MOVN using inverted first non-0xFFFF chunk
            let invFirstChunk = ~~~firstChunk
            [ARM64Symbolic.MOVN (dest, invFirstChunk, firstShift)]
            @ (if firstShift <> 0 && chunk0 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk0, 0)] else [])
            @ (if firstShift <> 16 && chunk1 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk1, 16)] else [])
            @ (if firstShift <> 32 && chunk2 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk2, 32)] else [])
            @ (if firstShift <> 48 && chunk3 <> 0xFFFFus then [ARM64Symbolic.MOVK (dest, chunk3, 48)] else [])
        | None ->
            // All chunks are 0xFFFF, use MOVN #0 to get all 1s (-1)
            [ARM64Symbolic.MOVN (dest, 0us, 0)]
    else
        // Use MOVZ: find first non-zero chunk, then MOVK for remaining non-zero chunks
        // MOVZ Xd, #imm, LSL #shift sets Xd = imm << shift, zeros elsewhere
        let chunks = [(chunk0, 0); (chunk1, 16); (chunk2, 32); (chunk3, 48)]
        let firstNonZero = chunks |> List.tryFind (fun (c, _) -> c <> 0us)
        match firstNonZero with
        | Some (firstChunk, firstShift) ->
            // Start with MOVZ using first non-zero chunk
            [ARM64Symbolic.MOVZ (dest, firstChunk, firstShift)]
            @ (if firstShift <> 0 && chunk0 <> 0us then [ARM64Symbolic.MOVK (dest, chunk0, 0)] else [])
            @ (if firstShift <> 16 && chunk1 <> 0us then [ARM64Symbolic.MOVK (dest, chunk1, 16)] else [])
            @ (if firstShift <> 32 && chunk2 <> 0us then [ARM64Symbolic.MOVK (dest, chunk2, 32)] else [])
            @ (if firstShift <> 48 && chunk3 <> 0us then [ARM64Symbolic.MOVK (dest, chunk3, 48)] else [])
        | None ->
            // All chunks are zero, just use MOVZ #0
            [ARM64Symbolic.MOVZ (dest, 0us, 0)]

/// Generate ARM64 instructions to load a stack slot into a register
/// Stack slots are accessed relative to FP (X29)
/// Uses LDUR for small offsets (-256 to +255), computes address for larger offsets
let loadStackSlot (dest: ARM64Symbolic.Reg) (offset: int) : Result<ARM64Symbolic.Instr list, string> =
    if offset >= -256 && offset <= 255 then
        // Small offset: use LDUR directly
        Ok [ARM64Symbolic.LDUR (dest, ARM64Symbolic.X29, int16 offset)]
    elif offset < 0 && -offset <= 4095 then
        // Larger negative offset: compute address into X10, then load
        // X10 = X29 - (-offset), then LDR dest, [X10, #0]
        Ok [
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 (-offset))
            ARM64Symbolic.LDR (dest, ARM64Symbolic.X10, 0s)
        ]
    elif offset > 0 && offset <= 4095 then
        // Larger positive offset: compute address into X10, then load
        Ok [
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 offset)
            ARM64Symbolic.LDR (dest, ARM64Symbolic.X10, 0s)
        ]
    else
        Error $"Stack offset {offset} exceeds supported range (-4095 to +4095)"

/// Load an integer or managed-string operand for a native CLI helper call.
let internal loadCliOperand (dest: ARM64Symbolic.Reg) (operand: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    match operand with
    | LIR.Imm value -> Ok (loadImmediate dest value)
    | LIR.Reg source ->
        lirRegToARM64Reg source
        |> Result.map (fun sourceReg ->
            if sourceReg = dest then [] else [ARM64Symbolic.MOV_reg (dest, sourceReg)])
    | LIR.StackSlot offset -> loadStackSlot dest offset
    | LIR.StringSymbol value -> Ok (loadStringLiteralPointer dest value)
    | _ -> Error "CLI native operation received a non-integer operand"

/// Generate ARM64 instructions to store a register to a stack slot
/// Stack slots are accessed relative to FP (X29)
/// Uses STUR for small offsets (-256 to +255), computes address for larger offsets
let storeStackSlot (src: ARM64Symbolic.Reg) (offset: int) : Result<ARM64Symbolic.Instr list, string> =
    if offset >= -256 && offset <= 255 then
        // Small offset: use STUR directly
        Ok [ARM64Symbolic.STUR (src, ARM64Symbolic.X29, int16 offset)]
    elif offset < 0 && -offset <= 4095 then
        // Larger negative offset: compute address into X10, then store
        // X10 = X29 - (-offset), then STR src, [X10, #0]
        Ok [
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 (-offset))
            ARM64Symbolic.STR (src, ARM64Symbolic.X10, 0s)
        ]
    elif offset > 0 && offset <= 4095 then
        // Larger positive offset: compute address into X10, then store
        Ok [
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 offset)
            ARM64Symbolic.STR (src, ARM64Symbolic.X10, 0s)
        ]
    else
        Error $"Stack offset {offset} exceeds supported range (-4095 to +4095)"
