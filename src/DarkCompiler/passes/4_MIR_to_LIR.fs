// 4_MIR_to_LIR.fs - Instruction Selection (Pass 4)
//
// Transforms MIR CFG into LIR CFG.
//
// Instruction selection algorithm:
// - Converts MIR basic blocks to LIR basic blocks
// - Selects appropriate ARM64 instructions for each MIR operation
// - Handles ARM64 operand constraints:
//   - ADD/SUB: support 12-bit immediates, left operand must be register
//   - MUL/SDIV: both operands must be registers
// - Inserts MOV instructions to load immediates when needed
// - Converts MIR terminators to LIR terminators
// - Preserves CFG structure (labels, branches, jumps)

module MIR_to_LIR

/// Convert MIR.VReg to LIR.Reg (virtual)
let vregToLIRReg (MIR.VReg id) : LIR.Reg = LIR.Virtual id

/// Convert MIR.Operand to LIR.Operand
let convertOperand (operand: MIR.Operand) : LIR.Operand =
    match operand with
    | MIR.IntConst n -> LIR.Imm n
    | MIR.BoolConst b -> LIR.Imm (if b then 1L else 0L)  // Booleans as 0/1
    | MIR.FloatRef idx -> LIR.FloatImm 0.0  // Placeholder - actual float loaded from pool later
    | MIR.StringRef idx -> LIR.StringRef idx
    | MIR.Register vreg -> LIR.Reg (vregToLIRReg vreg)

/// Ensure operand is in a register (may need to load immediate)
let ensureInRegister (operand: MIR.Operand) (tempReg: LIR.Reg) : Result<LIR.Instr list * LIR.Reg, string> =
    match operand with
    | MIR.IntConst n ->
        // Need to load constant into a temporary register
        Ok ([LIR.Mov (tempReg, LIR.Imm n)], tempReg)
    | MIR.BoolConst b ->
        // Load boolean (0 or 1) into register
        Ok ([LIR.Mov (tempReg, LIR.Imm (if b then 1L else 0L))], tempReg)
    | MIR.FloatRef idx ->
        // Load float into a register (placeholder - proper FP support needed later)
        Ok ([LIR.Mov (tempReg, LIR.FloatImm 0.0)], tempReg)
    | MIR.StringRef _ ->
        // String references are not used as operands in arithmetic operations
        Error "Internal error: Cannot use string literal as arithmetic operand"
    | MIR.Register vreg ->
        Ok ([], vregToLIRReg vreg)

/// Convert MIR instruction to LIR instructions
let selectInstr (instr: MIR.Instr) (stringPool: MIR.StringPool) : Result<LIR.Instr list, string> =
    match instr with
    | MIR.Mov (dest, src) ->
        let lirDest = vregToLIRReg dest
        let lirSrc = convertOperand src
        Ok [LIR.Mov (lirDest, lirSrc)]

    | MIR.BinOp (dest, op, left, right) ->
        let lirDest = vregToLIRReg dest
        let rightOp = convertOperand right

        match op with
        | MIR.Add ->
            // ADD can have immediate or register as right operand
            // Left operand must be in a register
            match ensureInRegister left lirDest with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Add (lirDest, leftReg, rightOp)])

        | MIR.Sub ->
            // SUB can have immediate or register as right operand
            match ensureInRegister left lirDest with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Sub (lirDest, leftReg, rightOp)])

        | MIR.Mul ->
            // MUL requires both operands in registers
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
            match ensureInRegister right (LIR.Virtual 1001) with
            | Error err -> Error err
            | Ok (rightInstrs, rightReg) ->
                Ok (leftInstrs @ rightInstrs @ [LIR.Mul (lirDest, leftReg, rightReg)])

        | MIR.Div ->
            // SDIV requires both operands in registers
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
            match ensureInRegister right (LIR.Virtual 1001) with
            | Error err -> Error err
            | Ok (rightInstrs, rightReg) ->
                Ok (leftInstrs @ rightInstrs @ [LIR.Sdiv (lirDest, leftReg, rightReg)])

        // Comparisons: CMP + CSET sequence
        | MIR.Eq ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.EQ)])

        | MIR.Neq ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.NE)])

        | MIR.Lt ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.LT)])

        | MIR.Gt ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.GT)])

        | MIR.Lte ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.LE)])

        | MIR.Gte ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
                Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.GE)])

        // Boolean operations (bitwise for 0/1 values)
        | MIR.And ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
            match ensureInRegister right (LIR.Virtual 1001) with
            | Error err -> Error err
            | Ok (rightInstrs, rightReg) ->
                Ok (leftInstrs @ rightInstrs @ [LIR.And (lirDest, leftReg, rightReg)])

        | MIR.Or ->
            match ensureInRegister left (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (leftInstrs, leftReg) ->
            match ensureInRegister right (LIR.Virtual 1001) with
            | Error err -> Error err
            | Ok (rightInstrs, rightReg) ->
                Ok (leftInstrs @ rightInstrs @ [LIR.Orr (lirDest, leftReg, rightReg)])

    | MIR.UnaryOp (dest, op, src) ->
        let lirDest = vregToLIRReg dest

        match op with
        | MIR.Neg ->
            // Check if source is a float - use FP negation
            match src with
            | MIR.FloatRef idx ->
                // Float negation: load float into D1, negate into D0
                Ok [
                    LIR.FLoad (LIR.FPhysical LIR.D1, idx)
                    LIR.FNeg (LIR.FPhysical LIR.D0, LIR.FPhysical LIR.D1)
                ]
            | _ ->
                // Integer negation: 0 - src
                match ensureInRegister src (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (srcInstrs, srcReg) ->
                    Ok (srcInstrs @ [LIR.Mov (lirDest, LIR.Imm 0L); LIR.Sub (lirDest, lirDest, LIR.Reg srcReg)])

        | MIR.Not ->
            // Boolean NOT: 1 - src (since booleans are 0 or 1)
            match ensureInRegister src (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (srcInstrs, srcReg) ->
                Ok (srcInstrs @ [
                    LIR.Mov (lirDest, LIR.Imm 1L)
                    LIR.Sub (lirDest, lirDest, LIR.Reg srcReg)
                ])

    | MIR.Call (dest, funcName, args) ->
        // ARM64 calling convention (AAPCS64):
        // - First 8 arguments in X0-X7
        // - Return value in X0
        // Note: >8 arguments are checked upfront in toLIR
        let lirDest = vregToLIRReg dest
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // IMPORTANT: Save caller-saved registers BEFORE setting up arguments
        // Because argument setup may clobber registers that hold live values
        // We use a special SaveRegs/RestoreRegs instruction pair that CodeGen expands
        let saveInstrs = [LIR.SaveRegs]

        // Move each argument to its corresponding register
        // Take only as many registers as we have arguments
        let moveInstrs =
            List.zip args (List.take (List.length args) argRegs)
            |> List.map (fun (arg, reg) ->
                let lirArg = convertOperand arg
                LIR.Mov (LIR.Physical reg, lirArg))

        // Call instruction (no longer handles caller-save - it's done above)
        let callInstr = LIR.Call (lirDest, funcName, List.map convertOperand args)

        // Restore caller-saved registers after the call
        let restoreInstrs = [LIR.RestoreRegs]

        // Move return value from X0 to destination (if not already X0)
        let moveResult =
            match lirDest with
            | LIR.Physical LIR.X0 -> []
            | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ moveInstrs @ [callInstr] @ restoreInstrs @ moveResult)

    | MIR.HeapAlloc (dest, sizeBytes) ->
        let lirDest = vregToLIRReg dest
        Ok [LIR.HeapAlloc (lirDest, sizeBytes)]

    | MIR.HeapStore (addr, offset, src) ->
        let lirAddr = vregToLIRReg addr
        let lirSrc = convertOperand src
        Ok [LIR.HeapStore (lirAddr, offset, lirSrc)]

    | MIR.HeapLoad (dest, addr, offset) ->
        let lirDest = vregToLIRReg dest
        let lirAddr = vregToLIRReg addr
        Ok [LIR.HeapLoad (lirDest, lirAddr, offset)]

    | MIR.RefCountInc (addr, payloadSize) ->
        let lirAddr = vregToLIRReg addr
        Ok [LIR.RefCountInc (lirAddr, payloadSize)]

    | MIR.RefCountDec (addr, payloadSize) ->
        let lirAddr = vregToLIRReg addr
        Ok [LIR.RefCountDec (lirAddr, payloadSize)]

    | MIR.Print (src, valueType) ->
        // Generate appropriate print instruction based on type
        match valueType with
        | AST.TBool ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIR.PrintBool (LIR.Physical LIR.X0)])
        | AST.TInt64 ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIR.PrintInt (LIR.Physical LIR.X0)])
        | AST.TFloat64 ->
            // Float needs to be in D0 - handle FloatRef specially
            match src with
            | MIR.FloatRef idx ->
                Ok [LIR.FLoad (LIR.FPhysical LIR.D0, idx)
                    LIR.PrintFloat (LIR.FPhysical LIR.D0)]
            | _ ->
                // Computed float should already be in FP reg, but we're receiving general operand
                // For now, just print as int (raw bits)
                let lirSrc = convertOperand src
                let moveToX0 =
                    match lirSrc with
                    | LIR.Reg (LIR.Physical LIR.X0) -> []
                    | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
                Ok (moveToX0 @ [LIR.PrintInt (LIR.Physical LIR.X0)])
        | AST.TString ->
            // String printing uses PrintString for pool strings, PrintHeapString for heap strings
            match src with
            | MIR.StringRef idx ->
                // Pool string: look up the string length from the pool
                match Map.tryFind idx stringPool.Strings with
                | Some (_, len) ->
                    Ok [LIR.PrintString (idx, len)]
                | None ->
                    Error $"Internal error: String index {idx} not found in pool"
            | MIR.Register vreg ->
                // Heap string (from concatenation): use PrintHeapString
                let lirReg = vregToLIRReg vreg
                Ok [LIR.PrintHeapString lirReg]
            | _ ->
                // Other cases (shouldn't happen for strings)
                let lirSrc = convertOperand src
                let moveToX0 =
                    match lirSrc with
                    | LIR.Reg (LIR.Physical LIR.X0) -> []
                    | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
                Ok (moveToX0 @ [LIR.PrintInt (LIR.Physical LIR.X0)])
        | AST.TTuple _ | AST.TRecord _ | AST.TList _ | AST.TSum _ ->
            // Heap types: print address for now
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIR.PrintInt (LIR.Physical LIR.X0)])
        | AST.TUnit ->
            // Unit: print nothing
            Ok []
        | AST.TFunction _ ->
            // Functions shouldn't be printed, but just print address
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIR.PrintInt (LIR.Physical LIR.X0)])
        | AST.TVar _ ->
            // Type variables should be monomorphized away before reaching LIR
            Error "Internal error: Type variable reached MIR_to_LIR (should be monomorphized)"

    | MIR.StringConcat (dest, left, right) ->
        let lirDest = vregToLIRReg dest
        let lirLeft = convertOperand left
        let lirRight = convertOperand right
        Ok [LIR.StringConcat (lirDest, lirLeft, lirRight)]

/// Convert MIR terminator to LIR terminator
/// For Branch, need to convert operand to register (may add instructions)
/// Printing is now handled by MIR.Print instruction, not in terminator
let selectTerminator (terminator: MIR.Terminator) (stringPool: MIR.StringPool) : Result<LIR.Instr list * LIR.Terminator, string> =
    match terminator with
    | MIR.Ret operand ->
        // Move return value to X0
        match operand with
        | MIR.FloatRef idx ->
            // Load float into D0 for return
            let loadFloat = LIR.FLoad (LIR.FPhysical LIR.D0, idx)
            Ok ([loadFloat], LIR.Ret)
        | MIR.BoolConst b ->
            // Return bool as 0/1
            let lirOp = LIR.Imm (if b then 1L else 0L)
            let moveToX0 = [LIR.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIR.Ret)
        | MIR.StringRef _ ->
            // Strings don't have a meaningful register return value
            // The value has been printed, so just exit with code 0
            Ok ([LIR.Exit], LIR.Ret)
        | _ ->
            // Move operand to X0 (return register)
            let lirOp = convertOperand operand
            let moveToX0 = [LIR.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIR.Ret)

    | MIR.Branch (condOp, trueLabel, falseLabel) ->
        // Convert MIR.Label to LIR.Label (just unwrap)
        let (MIR.Label trueLbl) = trueLabel
        let (MIR.Label falseLbl) = falseLabel

        // Condition must be in a register for ARM64 branch instructions
        match ensureInRegister condOp (LIR.Virtual 1002) with
        | Error err -> Error err
        | Ok (condInstrs, condReg) ->
            Ok (condInstrs, LIR.Branch (condReg, trueLbl, falseLbl))

    | MIR.Jump label ->
        let (MIR.Label lbl) = label
        Ok ([], LIR.Jump lbl)

/// Convert MIR label to LIR label
let convertLabel (MIR.Label lbl) : LIR.Label = lbl

/// Helper: collect Results from a list, returning Error on first failure
let private collectResults (results: Result<'a list, string> list) : Result<'a list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | (Error err) :: _ -> Error err
        | (Ok instrs) :: rest -> loop (instrs :: acc) rest
    loop [] results

/// Convert MIR basic block to LIR basic block
let selectBlock (block: MIR.BasicBlock) (stringPool: MIR.StringPool) : Result<LIR.BasicBlock, string> =
    let lirLabel = convertLabel block.Label

    // Convert all instructions
    let instrResults = block.Instrs |> List.map (fun i -> selectInstr i stringPool)
    match collectResults instrResults with
    | Error err -> Error err
    | Ok lirInstrs ->

    // Convert terminator (may add instructions)
    match selectTerminator block.Terminator stringPool with
    | Error err -> Error err
    | Ok (termInstrs, lirTerm) ->

    Ok {
        LIR.Label = lirLabel
        LIR.Instrs = lirInstrs @ termInstrs
        LIR.Terminator = lirTerm
    }

/// Helper: map a function returning Result over a list, returning Error on first failure
let private mapResults (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok result -> loop (result :: acc) rest
    loop [] items

/// Convert MIR CFG to LIR CFG
let selectCFG (cfg: MIR.CFG) (stringPool: MIR.StringPool) : Result<LIR.CFG, string> =
    let lirEntry = convertLabel cfg.Entry

    let blockList = cfg.Blocks |> Map.toList
    match mapResults (fun (label, block) ->
        match selectBlock block stringPool with
        | Error err -> Error err
        | Ok lirBlock -> Ok (convertLabel label, lirBlock)) blockList with
    | Error err -> Error err
    | Ok lirBlockList ->

    Ok {
        LIR.Entry = lirEntry
        LIR.Blocks = Map.ofList lirBlockList
    }

/// Check if any function has more than 8 parameters (ARM64 calling convention limit)
let private checkParameterLimits (mirFuncs: MIR.Function list) : Result<unit, string> =
    let funcWithTooManyParams =
        mirFuncs
        |> List.tryFind (fun f -> List.length f.Params > 8)
    match funcWithTooManyParams with
    | Some f ->
        Error $"Function '{f.Name}' has {List.length f.Params} parameters, but only 8 are supported (ARM64 calling convention limit)"
    | None -> Ok ()

/// Check if any function call has more than 8 arguments
let private checkCallArgLimits (mirFuncs: MIR.Function list) : Result<unit, string> =
    let checkBlock (block: MIR.BasicBlock) =
        block.Instrs
        |> List.tryPick (fun instr ->
            match instr with
            | MIR.Call (_, funcName, args) when List.length args > 8 ->
                Some $"Call to '{funcName}' has {List.length args} arguments, but only 8 are supported (ARM64 calling convention limit)"
            | _ -> None)

    let checkFunc (func: MIR.Function) =
        func.CFG.Blocks
        |> Map.toList
        |> List.tryPick (fun (_, block) -> checkBlock block)

    match mirFuncs |> List.tryPick checkFunc with
    | Some err -> Error err
    | None -> Ok ()

/// Convert MIR program to LIR
let toLIR (program: MIR.Program) : Result<LIR.Program, string> =
    let (MIR.Program (mirFuncs, stringPool, floatPool)) = program

    // Pre-check: verify all functions have ≤8 parameters and calls have ≤8 arguments
    match checkParameterLimits mirFuncs with
    | Error err -> Error err
    | Ok () ->
    match checkCallArgLimits mirFuncs with
    | Error err -> Error err
    | Ok () ->

    // Convert each MIR function to LIR
    let convertFunc (mirFunc: MIR.Function) =
        match selectCFG mirFunc.CFG stringPool with
        | Error err -> Error err
        | Ok lirCFG ->
            // Convert MIR VRegs to LIR Virtual registers for parameters
            let lirParams = mirFunc.Params |> List.map (fun (MIR.VReg id) -> LIR.Virtual id)
            Ok {
                LIR.Name = mirFunc.Name
                LIR.Params = lirParams
                LIR.CFG = lirCFG
                LIR.StackSize = 0  // Will be determined by register allocation
                LIR.UsedCalleeSaved = []  // Will be determined by register allocation
            }

    match mapResults convertFunc mirFuncs with
    | Error err -> Error err
    | Ok lirFuncs ->
        Ok (LIR.Program (lirFuncs, stringPool, floatPool))
