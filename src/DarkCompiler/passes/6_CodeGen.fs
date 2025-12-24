// 6_CodeGen.fs - Code Generation (Pass 6)
//
// Transforms LIR into ARM64 instructions.
//
// Code generation algorithm:
// - Maps LIR physical registers to ARM64 registers
// - Selects ARM64 instruction forms (immediate vs register operands)
// - Generates MOVZ instructions for loading immediate values
// - Handles 12-bit immediate constraints for ADD/SUB
//
// Assumes register allocation completed (no virtual registers remain)
//
// Example:
//   X0 <- Mov(Imm 42); X1 <- Add(X0, Imm 5)
//   →
//   MOVZ X0, #42, LSL #0; ADD X1, X0, #5

module CodeGen

/// Convert LIR.PhysReg to ARM64.Reg
let lirPhysRegToARM64Reg (physReg: LIR.PhysReg) : ARM64.Reg =
    match physReg with
    | LIR.X0 -> ARM64.X0
    | LIR.X1 -> ARM64.X1
    | LIR.X2 -> ARM64.X2
    | LIR.X3 -> ARM64.X3
    | LIR.X4 -> ARM64.X4
    | LIR.X5 -> ARM64.X5
    | LIR.X6 -> ARM64.X6
    | LIR.X7 -> ARM64.X7
    | LIR.X8 -> ARM64.X8
    | LIR.X9 -> ARM64.X9
    | LIR.X10 -> ARM64.X10
    | LIR.X11 -> ARM64.X11
    | LIR.X12 -> ARM64.X12
    | LIR.X13 -> ARM64.X13
    | LIR.X14 -> ARM64.X14
    | LIR.X15 -> ARM64.X15
    | LIR.X29 -> ARM64.X29
    | LIR.X30 -> ARM64.X30
    | LIR.SP -> ARM64.SP

/// Convert LIR.Reg to ARM64.Reg (assumes physical registers only)
let lirRegToARM64Reg (reg: LIR.Reg) : Result<ARM64.Reg, string> =
    match reg with
    | LIR.Physical physReg -> Ok (lirPhysRegToARM64Reg physReg)
    | LIR.Virtual vreg -> Error $"Virtual register {vreg} should have been allocated"

/// Generate ARM64 instructions to load an immediate into a register
let loadImmediate (dest: ARM64.Reg) (value: int64) : ARM64.Instr list =
    // Load 64-bit immediate using MOVZ + MOVK sequence
    // Extract each 16-bit chunk
    let chunk0 = uint16 (value >>> 0) &&& 0xFFFFus
    let chunk1 = uint16 (value >>> 16) &&& 0xFFFFus
    let chunk2 = uint16 (value >>> 32) &&& 0xFFFFus
    let chunk3 = uint16 (value >>> 48) &&& 0xFFFFus

    // Build instruction sequence
    // Start with MOVZ for the first non-zero chunk (or chunk0 if all zero)
    let mutable instrs = [ARM64.MOVZ (dest, chunk0, 0)]

    // Add MOVK for remaining chunks if non-zero
    if chunk1 <> 0us then
        instrs <- instrs @ [ARM64.MOVK (dest, chunk1, 16)]
    if chunk2 <> 0us then
        instrs <- instrs @ [ARM64.MOVK (dest, chunk2, 32)]
    if chunk3 <> 0us then
        instrs <- instrs @ [ARM64.MOVK (dest, chunk3, 48)]

    instrs

/// Convert LIR instruction to ARM64 instructions
let convertInstr (instr: LIR.Instr) : Result<ARM64.Instr list, string> =
    match instr with
    | LIR.Mov (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match src with
            | LIR.Imm value ->
                Ok (loadImmediate destReg value)
            | LIR.Reg srcReg ->
                lirRegToARM64Reg srcReg
                |> Result.map (fun srcARM64 -> [ARM64.MOV_reg (destReg, srcARM64)])
            | LIR.StackSlot _ ->
                Error "Stack slots not yet supported")

    | LIR.Add (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= 0L && value < 4096L ->
                    // Can use immediate ADD
                    Ok [ARM64.ADD_imm (destReg, leftReg, uint16 value)]
                | LIR.Imm value ->
                    // Need to load immediate into register first
                    let tempReg = ARM64.X9  // Use X9 as temp
                    Ok (loadImmediate tempReg value @ [ARM64.ADD_reg (destReg, leftReg, tempReg)])
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64.ADD_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot _ ->
                    Error "Stack slots not yet supported"))

    | LIR.Sub (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= 0L && value < 4096L ->
                    Ok [ARM64.SUB_imm (destReg, leftReg, uint16 value)]
                | LIR.Imm value ->
                    let tempReg = ARM64.X9
                    Ok (loadImmediate tempReg value @ [ARM64.SUB_reg (destReg, leftReg, tempReg)])
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64.SUB_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot _ ->
                    Error "Stack slots not yet supported"))

    | LIR.Mul (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.MUL (destReg, leftReg, rightReg)])))

    | LIR.Sdiv (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.SDIV (destReg, leftReg, rightReg)])))

    | LIR.Cmp (left, right) ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->
                Ok [ARM64.CMP_imm (leftReg, uint16 value)]
            | LIR.Imm value ->
                let tempReg = ARM64.X9
                Ok (loadImmediate tempReg value @ [ARM64.CMP_reg (leftReg, tempReg)])
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [ARM64.CMP_reg (leftReg, rightARM64)])
            | LIR.StackSlot _ ->
                Error "Stack slots not yet supported")

    | LIR.Cset (dest, cond) ->
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            let arm64Cond =
                match cond with
                | LIR.EQ -> ARM64.EQ
                | LIR.NE -> ARM64.NE
                | LIR.LT -> ARM64.LT
                | LIR.GT -> ARM64.GT
                | LIR.LE -> ARM64.LE
                | LIR.GE -> ARM64.GE
            [ARM64.CSET (destReg, arm64Cond)])

    | LIR.And (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.AND_reg (destReg, leftReg, rightReg)])))

    | LIR.Orr (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.ORR_reg (destReg, leftReg, rightReg)])))

    | LIR.Mvn (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.MVN (destReg, srcReg)]))

    | LIR.PrintBool reg ->
        // For now, print booleans as 0 or 1 (same as PrintInt)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintInt ()
            else
                Runtime.generatePrintInt ())

    | LIR.PrintInt reg ->
        // Value to print should be in X0
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                // Move to X0 if not already there
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintInt ()
            else
                Runtime.generatePrintInt ())

/// Convert LIR terminator to ARM64 instructions
let convertTerminator (terminator: LIR.Terminator) : Result<ARM64.Instr list, string> =
    match terminator with
    | LIR.Ret ->
        Ok [ARM64.RET]

    | LIR.Branch (condReg, trueLabel, falseLabel) ->
        // Branch if register is non-zero (true), otherwise fall through to else
        // Use CBNZ (compare and branch if not zero) to true label
        // Then unconditional branch to false label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            [
                ARM64.CBNZ (arm64Reg, trueLabel)  // If true, jump to then branch
                ARM64.B_label falseLabel           // Otherwise jump to else branch
            ])

    | LIR.Jump label ->
        Ok [ARM64.B_label label]

/// Convert LIR basic block to ARM64 instructions (with label)
let convertBlock (block: LIR.BasicBlock) : Result<ARM64.Instr list, string> =
    // Emit label for this block
    let labelInstr = ARM64.Label block.Label

    // Convert all instructions
    let instrResults = block.Instrs |> List.map convertInstr

    // Convert terminator
    let termResult = convertTerminator block.Terminator

    // Collect all results
    let rec collectResults acc results =
        match results with
        | [] -> Ok (List.rev acc)
        | (Ok instrs) :: rest -> collectResults (List.rev instrs @ acc) rest
        | (Error err) :: _ -> Error err

    match collectResults [] instrResults with
    | Error err -> Error err
    | Ok instrs ->
        match termResult with
        | Error err -> Error err
        | Ok termInstrs -> Ok (labelInstr :: instrs @ termInstrs)

/// Convert LIR CFG to ARM64 instructions
let convertCFG (cfg: LIR.CFG) : Result<ARM64.Instr list, string> =
    // Get blocks in a deterministic order (entry first, then sorted by label)
    let entryBlock =
        match Map.tryFind cfg.Entry cfg.Blocks with
        | Some block -> [block]
        | None -> []

    let otherBlocks =
        cfg.Blocks
        |> Map.toList
        |> List.filter (fun (label, _) -> label <> cfg.Entry)
        |> List.sortBy fst
        |> List.map snd

    let allBlocks = entryBlock @ otherBlocks

    // Convert each block
    allBlocks
    |> List.map convertBlock
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])

/// Convert LIR function to ARM64 instructions
let convertFunction (func: LIR.Function) : Result<ARM64.Instr list, string> =
    convertCFG func.CFG

/// Convert LIR program to ARM64 instructions
let generateARM64 (program: LIR.Program) : Result<ARM64.Instr list, string> =
    let (LIR.Program functions) = program
    functions
    |> List.map convertFunction
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])
