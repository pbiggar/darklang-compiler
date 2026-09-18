// FloatingPoint.fs - Emit arm64 instructions for floatingpoint operations.

module ARM64EmitFloatingPoint

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

let internal emitFPhi (ctx: CodeGenContext) : Result<ARM64Symbolic.Instr list, string> =
    // Float phi nodes should be eliminated before code generation (by register allocation)
    Error "Float phi nodes should be eliminated before code generation"

let internal emitFArgMoves (ctx: CodeGenContext) (moves: (LIR.PhysFPReg * LIR.FReg) list) : Result<ARM64Symbolic.Instr list, string> =
    // Float argument moves - move float values to D0-D7
    // Uses parallel move resolution to handle register conflicts correctly

    // First, convert all source FRegs to ARM64 FRegs
    let resolvedMoves =
        moves
        |> List.map (fun (destPhysReg, srcFReg) ->
            let destARM64 = lirPhysFPRegToARM64FReg destPhysReg
            match lirFRegToARM64FReg srcFReg with
            | Ok srcARM64 -> Ok (destARM64, srcARM64)
            | Error e -> Error e)
        |> List.fold (fun acc r ->
            match acc, r with
            | Ok moves, Ok move -> Ok (move :: moves)
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.map List.rev

    match resolvedMoves with
    | Error e -> Error e
    | Ok armMoves ->
        // Use ParallelMoves.resolve to get the correct move order
        // We treat ARM64Symbolic.FReg as both dest and src type
        let getSrcReg (srcReg: ARM64Symbolic.FReg) : ARM64Symbolic.FReg option = Some srcReg
        let actions = ParallelMoves.resolve armMoves getSrcReg

        // Convert actions to ARM64 instructions
        // Use reserved D16 as the temporary register for cycle breaking.
        actions
        |> List.collect (function
            | ParallelMoves.SaveToTemp srcReg ->
                // Save to D16 (temp) - using upper SIMD register
                [ARM64Symbolic.FMOV_reg (ARM64Symbolic.D16, srcReg)]
            | ParallelMoves.Move (dest, src) ->
                if dest <> src then
                    [ARM64Symbolic.FMOV_reg (dest, src)]
                else
                    []
            | ParallelMoves.MoveFromTemp dest ->
                // Move from D16 (temp) to destination
                [ARM64Symbolic.FMOV_reg (dest, ARM64Symbolic.D16)])
        |> Ok

let internal emitFMov (ctx: CodeGenContext) (dest: LIR.FReg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_reg (destReg, srcReg)]))

let internal emitFLoad (ctx: CodeGenContext) (dest: LIR.FReg) (value: float) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.map (fun destReg ->
        if System.BitConverter.DoubleToInt64Bits(value) = 0L then
            [ARM64Symbolic.FMOV_zero destReg]
        elif ARM64.tryEncodeFmovFloatImmediate value |> Option.isSome then
            [ARM64Symbolic.FMOV_imm (destReg, value)]
        else
            let labelRef = floatDataLabel value
            [
                ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)           // Load page address of float
                ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)  // Add page offset
                ARM64Symbolic.LDR_fp (destReg, ARM64Symbolic.X9, 0s)        // Load float from [X9]
            ])

let private floatStackAddress (stackSlot: int) : Result<ARM64Symbolic.Instr list, string> =
    if stackSlot < 0 && -stackSlot <= 4095 then
        Ok [ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 (-stackSlot))]
    elif stackSlot >= 0 && stackSlot <= 4095 then
        Ok [ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X29, uint16 stackSlot)]
    else
        Error $"Float stack offset {stackSlot} exceeds supported range (-4095 to +4095)"

let internal emitFSpillLoad (ctx: CodeGenContext) (dest: LIR.FReg) (stackSlot: int) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        floatStackAddress stackSlot
        |> Result.map (fun address -> address @ [ARM64Symbolic.LDR_fp (destReg, ARM64Symbolic.X10, 0s)]))

let internal emitFSpillStore (ctx: CodeGenContext) (stackSlot: int) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg src
    |> Result.bind (fun srcReg ->
        floatStackAddress stackSlot
        |> Result.map (fun address -> address @ [ARM64Symbolic.STR_fp (srcReg, ARM64Symbolic.X10, 0s)]))

let internal emitFAdd (ctx: CodeGenContext) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.FADD (destReg, leftReg, rightReg)])))

let internal emitFSub (ctx: CodeGenContext) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.FSUB (destReg, leftReg, rightReg)])))

let internal emitFMul (ctx: CodeGenContext) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.FMUL (destReg, leftReg, rightReg)])))

let internal emitFDiv (ctx: CodeGenContext) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.FDIV (destReg, leftReg, rightReg)])))

let internal emitFNeg (ctx: CodeGenContext) (dest: LIR.FReg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FNEG (destReg, srcReg)]))

let internal emitFAbs (ctx: CodeGenContext) (dest: LIR.FReg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FABS (destReg, srcReg)]))

let internal emitFSqrt (ctx: CodeGenContext) (dest: LIR.FReg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FSQRT (destReg, srcReg)]))

let internal emitFCmp (ctx: CodeGenContext) (left: LIR.FReg) (right: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg left
    |> Result.bind (fun leftReg ->
        lirFRegToARM64FReg right
        |> Result.map (fun rightReg -> [ARM64Symbolic.FCMP (leftReg, rightReg)]))

let internal emitFloatToInt64 (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FCVTZS (destReg, srcReg)]))

let internal emitFpToGp (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    // Move bits from FP register to GP register (for floats stored to list)
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_to_gp (destReg, srcReg)]))

let internal emitFloatToBits (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    // Copy Float64 bits to UInt64 (uses FMOV to GP register)
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_to_gp (destReg, srcReg)]))

// Heap operations

let internal emitFloatToString (ctx: CodeGenContext) (dest: LIR.Reg) (value: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    // Convert float in FP register to heap string
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirFRegToARM64FReg value
        |> Result.map (fun valueReg ->
            runtimeInstrs (ARM64FloatFormatting.generateFloatToString destReg valueReg) @ generateLeakCounterInc ctx))
