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

/// Convert MIR.VReg to LIR.FReg (virtual float register)
let vregToLIRFReg (MIR.VReg id) : LIR.FReg = LIR.FVirtual id

/// Convert MIR.Operand to LIR.Operand
let convertOperand (operand: MIR.Operand) : LIR.Operand =
    match operand with
    | MIR.IntConst n -> LIR.Imm n
    | MIR.BoolConst b -> LIR.Imm (if b then 1L else 0L)  // Booleans as 0/1
    | MIR.FloatRef idx -> LIR.FloatImm 0.0  // Placeholder - actual float loaded from pool later
    | MIR.StringRef idx -> LIR.StringRef idx
    | MIR.Register vreg -> LIR.Reg (vregToLIRReg vreg)
    | MIR.FuncAddr name -> LIR.FuncAddr name  // Function address (for higher-order functions)

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
    | MIR.FuncAddr name ->
        // Load function address into register using ADR instruction
        Ok ([LIR.LoadFuncAddr (tempReg, name)], tempReg)

/// Ensure float operand is in an FP register
let ensureInFRegister (operand: MIR.Operand) (tempFReg: LIR.FReg) : Result<LIR.Instr list * LIR.FReg, string> =
    match operand with
    | MIR.FloatRef idx ->
        // Load float constant from pool into FP register
        Ok ([LIR.FLoad (tempFReg, idx)], tempFReg)
    | MIR.Register vreg ->
        // Float value already in a virtual register - treat it as FVirtual
        Ok ([], vregToLIRFReg vreg)
    | MIR.IntConst _ | MIR.BoolConst _ ->
        Error "Internal error: Cannot use integer/boolean as float operand"
    | MIR.StringRef _ ->
        Error "Internal error: Cannot use string as float operand"
    | MIR.FuncAddr _ ->
        Error "Internal error: Cannot use function address as float operand"

/// Convert MIR instruction to LIR instructions
let selectInstr (instr: MIR.Instr) (stringPool: MIR.StringPool) : Result<LIR.Instr list, string> =
    match instr with
    | MIR.Mov (dest, src, valueType) ->
        match valueType with
        | Some AST.TFloat64 ->
            // Float move - use FP registers
            let lirFDest = vregToLIRFReg dest
            match src with
            | MIR.FloatRef idx ->
                // Load float constant from pool
                Ok [LIR.FLoad (lirFDest, idx)]
            | MIR.Register vreg ->
                // Move between float registers
                let srcFReg = vregToLIRFReg vreg
                Ok [LIR.FMov (lirFDest, srcFReg)]
            | _ ->
                Error "Internal error: non-float operand in float Mov"
        | _ ->
            // Integer/other move
            let lirDest = vregToLIRReg dest
            let lirSrc = convertOperand src
            Ok [LIR.Mov (lirDest, lirSrc)]

    | MIR.BinOp (dest, op, left, right, operandType) ->
        let lirDest = vregToLIRReg dest
        let lirFDest = vregToLIRFReg dest
        let rightOp = convertOperand right

        // Check if this is a float operation
        match operandType with
        | AST.TFloat64 ->
            // Float operations - use FP registers and instructions
            match op with
            | MIR.Add ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FAdd (lirFDest, leftFReg, rightFReg)])
            | MIR.Sub ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FSub (lirFDest, leftFReg, rightFReg)])
            | MIR.Mul ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FMul (lirFDest, leftFReg, rightFReg)])
            | MIR.Div ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FDiv (lirFDest, leftFReg, rightFReg)])
            | MIR.Mod ->
                Error "Float modulo not yet supported"
            // Float comparisons - result goes in integer register
            | MIR.Eq ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.EQ)])
            | MIR.Neq ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.NE)])
            | MIR.Lt ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.LT)])
            | MIR.Gt ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.GT)])
            | MIR.Lte ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.LE)])
            | MIR.Gte ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.GE)])
            | MIR.And | MIR.Or ->
                Error "Boolean operations not supported on floats"
            | MIR.Shl | MIR.Shr | MIR.BitAnd | MIR.BitOr | MIR.BitXor ->
                Error "Bitwise operations not supported on floats"

        | _ ->
            // Integer operations - existing logic
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

            | MIR.Mod ->
                // Modulo: a % b = a - (a / b) * b
                // ARM64: sdiv temp, left, right; msub dest, temp, right, left
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    let quotReg = LIR.Virtual 1002  // temp for quotient
                    Ok (leftInstrs @ rightInstrs @
                        [LIR.Sdiv (quotReg, leftReg, rightReg);
                         LIR.Msub (lirDest, quotReg, rightReg, leftReg)])

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

            // Bitwise operators
            | MIR.Shl ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Lsl (lirDest, leftReg, rightReg)])

            | MIR.Shr ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Lsr (lirDest, leftReg, rightReg)])

            | MIR.BitAnd ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.And (lirDest, leftReg, rightReg)])

            | MIR.BitOr ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Orr (lirDest, leftReg, rightReg)])

            | MIR.BitXor ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Eor (lirDest, leftReg, rightReg)])

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

        // Use ArgMoves for parallel move - handles register clobbering correctly
        // The code generator will emit moves in a safe order, using SaveRegs stack when needed
        let argMoves =
            if List.isEmpty args then []
            else
                let argPairs =
                    List.zip args (List.take (List.length args) argRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIR.ArgMoves argPairs]

        // Call instruction (no longer handles caller-save - it's done above)
        let callInstr = LIR.Call (lirDest, funcName, List.map convertOperand args)

        // Restore caller-saved registers after the call
        let restoreInstrs = [LIR.RestoreRegs]

        // Move return value from X0 to destination (if not already X0)
        let moveResult =
            match lirDest with
            | LIR.Physical LIR.X0 -> []
            | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ argMoves @ [callInstr] @ restoreInstrs @ moveResult)

    | MIR.IndirectCall (dest, func, args) ->
        // Indirect call through function pointer (BLR instruction)
        // Similar to direct call but uses function address in register
        let lirDest = vregToLIRReg dest
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // Save caller-saved registers
        let saveInstrs = [LIR.SaveRegs]

        // IMPORTANT: Load function address into X9 FIRST, before setting up arguments.
        // The function pointer might be in X0-X7 which will be overwritten by argument moves.
        let funcOp = convertOperand func
        let loadFuncInstrs =
            match funcOp with
            | LIR.Reg r ->
                // Always copy to X9 in case the source register is overwritten by arg moves
                [LIR.Mov (LIR.Physical LIR.X9, LIR.Reg r)]
            | LIR.FuncAddr name ->
                [LIR.LoadFuncAddr (LIR.Physical LIR.X9, name)]
            | other ->
                // Load operand into X9
                [LIR.Mov (LIR.Physical LIR.X9, other)]

        // Use ArgMoves for parallel move - handles register clobbering correctly
        let argMoves =
            if List.isEmpty args then []
            else
                let argPairs =
                    List.zip args (List.take (List.length args) argRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIR.ArgMoves argPairs]

        // Call through X9 (always, since we always copy to X9 now)
        let callInstr = LIR.IndirectCall (lirDest, LIR.Physical LIR.X9, List.map convertOperand args)

        // Restore caller-saved registers
        let restoreInstrs = [LIR.RestoreRegs]

        // Move return value from X0 to destination
        let moveResult =
            match lirDest with
            | LIR.Physical LIR.X0 -> []
            | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ loadFuncInstrs @ argMoves @ [callInstr] @ restoreInstrs @ moveResult)

    | MIR.ClosureAlloc (dest, funcName, captures) ->
        // Allocate closure: (func_addr, cap1, cap2, ...)
        // This is similar to TupleAlloc but first element is a function address
        let lirDest = vregToLIRReg dest
        let numSlots = 1 + List.length captures
        let sizeBytes = numSlots * 8
        let allocInstr = LIR.HeapAlloc (lirDest, sizeBytes)
        // Store function pointer at offset 0
        let storeFuncInstr = LIR.HeapStore (lirDest, 0, LIR.FuncAddr funcName)
        // Store captured values at offsets 8, 16, ...
        let storeInstrs =
            captures
            |> List.mapi (fun i cap -> LIR.HeapStore (lirDest, (i + 1) * 8, convertOperand cap))
        Ok (allocInstr :: storeFuncInstr :: storeInstrs)

    | MIR.ClosureCall (dest, closure, args) ->
        // Call through closure: extract func_ptr from closure[0], call with (closure, args...)
        let lirDest = vregToLIRReg dest
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // Save caller-saved registers
        let saveInstrs = [LIR.SaveRegs]

        // Load closure into a temp register first
        let closureOp = convertOperand closure
        let closureReg = LIR.Physical LIR.X10  // Use X10 for closure (not an arg register)
        let loadClosureInstr =
            match closureOp with
            | LIR.Reg r -> LIR.Mov (closureReg, LIR.Reg r)
            | other -> LIR.Mov (closureReg, other)

        // Load function pointer from closure[0] into X9
        let loadFuncPtrInstr = LIR.HeapLoad (LIR.Physical LIR.X9, closureReg, 0)

        // Use ArgMoves for parallel move - closure goes to X0, args to X1-X7
        // Closure is already safe in X10, include it in the ArgMoves
        let argMoves =
            if List.length args > 7 then
                // Error: too many args for closure call (8 - 1 for closure = 7 max)
                []  // Will be caught by validation
            else
                let closureMove = (LIR.X0, LIR.Reg closureReg)
                let regularArgMoves =
                    args
                    |> List.mapi (fun i arg ->
                        let targetReg = List.item (i + 1) argRegs  // X1, X2, ...
                        (targetReg, convertOperand arg))
                [LIR.ArgMoves (closureMove :: regularArgMoves)]

        let callInstr = LIR.ClosureCall (lirDest, LIR.Physical LIR.X9, List.map convertOperand args)

        // Restore caller-saved registers
        let restoreInstrs = [LIR.RestoreRegs]

        // Move return value from X0 to destination
        let moveResult =
            match lirDest with
            | LIR.Physical LIR.X0 -> []
            | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ [loadClosureInstr; loadFuncPtrInstr] @ argMoves @ [callInstr] @ restoreInstrs @ moveResult)

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
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIR.PrintInt (LIR.Physical LIR.X0)])
        | AST.TFloat64 ->
            // Float needs to be in D0 for printing
            match src with
            | MIR.FloatRef idx ->
                // Literal float - load from pool
                Ok [LIR.FLoad (LIR.FPhysical LIR.D0, idx)
                    LIR.PrintFloat (LIR.FPhysical LIR.D0)]
            | MIR.Register vreg ->
                // Computed float - it's in an FVirtual register, move to D0 for printing
                let srcFReg = vregToLIRFReg vreg
                Ok [LIR.FMov (LIR.FPhysical LIR.D0, srcFReg)
                    LIR.PrintFloat (LIR.FPhysical LIR.D0)]
            | _ ->
                Error "Internal error: unexpected operand type for float print"
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
        | AST.TTuple _ | AST.TRecord _ | AST.TList _ | AST.TSum _ | AST.TDict _ ->
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
        | AST.TRawPtr ->
            // Raw pointer: print address
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

    | MIR.FileReadText (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok [LIR.FileReadText (lirDest, lirPath)]

    | MIR.FileExists (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok [LIR.FileExists (lirDest, lirPath)]

    | MIR.FileWriteText (dest, path, content) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        let lirContent = convertOperand content
        Ok [LIR.FileWriteText (lirDest, lirPath, lirContent)]

    | MIR.FileAppendText (dest, path, content) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        let lirContent = convertOperand content
        Ok [LIR.FileAppendText (lirDest, lirPath, lirContent)]

    | MIR.RawAlloc (dest, numBytes) ->
        let lirDest = vregToLIRReg dest
        // numBytes must be in a register for LIR
        match ensureInRegister numBytes (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (loadInstrs, numBytesReg) ->
            Ok (loadInstrs @ [LIR.RawAlloc (lirDest, numBytesReg)])

    | MIR.RawFree ptr ->
        // ptr must be in a register
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (loadInstrs, ptrReg) ->
            Ok (loadInstrs @ [LIR.RawFree ptrReg])

    | MIR.RawGet (dest, ptr, byteOffset) ->
        let lirDest = vregToLIRReg dest
        // Both ptr and byteOffset must be in registers
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg) ->
        match ensureInRegister byteOffset (LIR.Virtual 1001) with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg) ->
            Ok (ptrInstrs @ offsetInstrs @ [LIR.RawGet (lirDest, ptrReg, offsetReg)])

    | MIR.RawSet (ptr, byteOffset, value) ->
        // All three operands must be in registers
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg) ->
        match ensureInRegister byteOffset (LIR.Virtual 1001) with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg) ->
        match ensureInRegister value (LIR.Virtual 1002) with
        | Error err -> Error err
        | Ok (valueInstrs, valueReg) ->
            Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIR.RawSet (ptrReg, offsetReg, valueReg)])

    | MIR.FloatSqrt (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIR.FSqrt (lirFDest, srcFReg)])

    | MIR.FloatAbs (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIR.FAbs (lirFDest, srcFReg)])

    | MIR.FloatNeg (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIR.FNeg (lirFDest, srcFReg)])

    | MIR.IntToFloat (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        // src is an integer operand that needs to be in an integer register
        match ensureInRegister src (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcReg) ->
            Ok (srcInstrs @ [LIR.IntToFloat (lirFDest, srcReg)])

    | MIR.FloatToInt (dest, src) ->
        let lirDest = vregToLIRReg dest
        // src is a float operand that needs to be in a float register
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIR.FloatToInt (lirDest, srcFReg)])

    | MIR.StringHash (dest, str) ->
        let lirDest = vregToLIRReg dest
        let lirStr = convertOperand str
        Ok [LIR.StringHash (lirDest, lirStr)]

    | MIR.StringEq (dest, left, right) ->
        let lirDest = vregToLIRReg dest
        let lirLeft = convertOperand left
        let lirRight = convertOperand right
        Ok [LIR.StringEq (lirDest, lirLeft, lirRight)]

    | MIR.RefCountIncString str ->
        let lirStr = convertOperand str
        Ok [LIR.RefCountIncString lirStr]

    | MIR.RefCountDecString str ->
        let lirStr = convertOperand str
        Ok [LIR.RefCountDecString lirStr]

    | MIR.Phi _ ->
        // Phi nodes should be eliminated by SSA destruction (pass 3.9) before MIR-to-LIR
        Error "Internal error: Phi node reached MIR-to-LIR. SSA destruction pass should have removed it."

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
            | MIR.IndirectCall (_, _, args) when List.length args > 8 ->
                Some $"Indirect call has {List.length args} arguments, but only 8 are supported (ARM64 calling convention limit)"
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

// ============================================================================
// LIR Program Merging (for stdlib LIR caching optimization)
// ============================================================================

/// Offset pool references in an LIR operand
let private offsetLIROperand (strOffset: int) (fltOffset: int) (op: LIR.Operand) : LIR.Operand =
    match op with
    | LIR.StringRef idx -> LIR.StringRef (idx + strOffset)
    | LIR.FloatRef idx -> LIR.FloatRef (idx + fltOffset)
    | other -> other

/// Offset pool references in a list of LIR operands
let private offsetLIROperands strOffset fltOffset ops =
    List.map (offsetLIROperand strOffset fltOffset) ops

/// Offset pool references in an LIR instruction
let private offsetLIRInstr (strOffset: int) (fltOffset: int) (instr: LIR.Instr) : LIR.Instr =
    match instr with
    // Instructions with Operand fields that may contain StringRef/FloatRef
    | LIR.Mov (dest, src) -> LIR.Mov (dest, offsetLIROperand strOffset fltOffset src)
    | LIR.Add (dest, left, right) -> LIR.Add (dest, left, offsetLIROperand strOffset fltOffset right)
    | LIR.Sub (dest, left, right) -> LIR.Sub (dest, left, offsetLIROperand strOffset fltOffset right)
    | LIR.Cmp (left, right) -> LIR.Cmp (left, offsetLIROperand strOffset fltOffset right)
    | LIR.Call (dest, name, args) -> LIR.Call (dest, name, offsetLIROperands strOffset fltOffset args)
    | LIR.IndirectCall (dest, func, args) -> LIR.IndirectCall (dest, func, offsetLIROperands strOffset fltOffset args)
    | LIR.ClosureAlloc (dest, name, caps) -> LIR.ClosureAlloc (dest, name, offsetLIROperands strOffset fltOffset caps)
    | LIR.ClosureCall (dest, closure, args) -> LIR.ClosureCall (dest, closure, offsetLIROperands strOffset fltOffset args)
    | LIR.ArgMoves moves -> LIR.ArgMoves (moves |> List.map (fun (reg, op) -> (reg, offsetLIROperand strOffset fltOffset op)))
    | LIR.HeapStore (addr, offset, src) -> LIR.HeapStore (addr, offset, offsetLIROperand strOffset fltOffset src)
    | LIR.StringConcat (dest, left, right) -> LIR.StringConcat (dest, offsetLIROperand strOffset fltOffset left, offsetLIROperand strOffset fltOffset right)
    | LIR.FileReadText (dest, path) -> LIR.FileReadText (dest, offsetLIROperand strOffset fltOffset path)
    | LIR.FileExists (dest, path) -> LIR.FileExists (dest, offsetLIROperand strOffset fltOffset path)
    | LIR.FileWriteText (dest, path, content) -> LIR.FileWriteText (dest, offsetLIROperand strOffset fltOffset path, offsetLIROperand strOffset fltOffset content)
    | LIR.FileAppendText (dest, path, content) -> LIR.FileAppendText (dest, offsetLIROperand strOffset fltOffset path, offsetLIROperand strOffset fltOffset content)
    | LIR.StringHash (dest, str) -> LIR.StringHash (dest, offsetLIROperand strOffset fltOffset str)
    | LIR.StringEq (dest, left, right) -> LIR.StringEq (dest, offsetLIROperand strOffset fltOffset left, offsetLIROperand strOffset fltOffset right)
    | LIR.RefCountIncString str -> LIR.RefCountIncString (offsetLIROperand strOffset fltOffset str)
    | LIR.RefCountDecString str -> LIR.RefCountDecString (offsetLIROperand strOffset fltOffset str)
    // Instructions with direct pool indices
    | LIR.PrintString (strIdx, strLen) -> LIR.PrintString (strIdx + strOffset, strLen)
    | LIR.FLoad (dest, floatIdx) -> LIR.FLoad (dest, floatIdx + fltOffset)
    // Instructions without pool references - pass through unchanged
    | LIR.Store _ | LIR.Mul _ | LIR.Sdiv _ | LIR.Msub _ | LIR.Cset _
    | LIR.And _ | LIR.Orr _ | LIR.Eor _ | LIR.Lsl _ | LIR.Lsr _ | LIR.Mvn _
    | LIR.SaveRegs | LIR.RestoreRegs | LIR.PrintInt _ | LIR.PrintBool _ | LIR.PrintFloat _
    | LIR.Exit | LIR.FMov _ | LIR.FAdd _ | LIR.FSub _ | LIR.FMul _ | LIR.FDiv _
    | LIR.FNeg _ | LIR.FAbs _ | LIR.FSqrt _ | LIR.FCmp _ | LIR.IntToFloat _ | LIR.FloatToInt _
    | LIR.HeapAlloc _ | LIR.HeapLoad _ | LIR.RefCountInc _ | LIR.RefCountDec _
    | LIR.PrintHeapString _ | LIR.LoadFuncAddr _ | LIR.RawAlloc _ | LIR.RawFree _
    | LIR.RawGet _ | LIR.RawSet _ -> instr

/// Offset pool references in an LIR basic block
let private offsetLIRBlock (strOffset: int) (fltOffset: int) (block: LIR.BasicBlock) : LIR.BasicBlock =
    { Label = block.Label
      Instrs = List.map (offsetLIRInstr strOffset fltOffset) block.Instrs
      Terminator = block.Terminator }  // Terminators don't contain pool refs in LIR

/// Offset pool references in an LIR function
let private offsetLIRFunction (strOffset: int) (fltOffset: int) (func: LIR.Function) : LIR.Function =
    let offsetBlocks =
        func.CFG.Blocks
        |> Map.map (fun _ block -> offsetLIRBlock strOffset fltOffset block)
    { Name = func.Name
      Params = func.Params
      CFG = { Entry = func.CFG.Entry; Blocks = offsetBlocks }
      StackSize = func.StackSize
      UsedCalleeSaved = func.UsedCalleeSaved }

/// Merge user LIR with cached stdlib LIR.
/// Offsets user's StringRef/FloatRef indices to account for stdlib pools.
/// Excludes stdlib's _start function (user's _start is the entry point).
let mergeLIRPrograms (stdlibLIR: LIR.Program) (userLIR: LIR.Program) : LIR.Program =
    let (LIR.Program (stdlibFuncs, stdlibStrings, stdlibFloats)) = stdlibLIR
    let (LIR.Program (userFuncs, userStrings, userFloats)) = userLIR

    let stringOffset = stdlibStrings.NextId
    let floatOffset = stdlibFloats.NextId

    // Exclude stdlib's _start function (user's _start is the real entry point)
    let stdlibFuncsNoStart = stdlibFuncs |> List.filter (fun f -> f.Name <> "_start")

    // Offset user function pool references
    let offsetUserFuncs = userFuncs |> List.map (offsetLIRFunction stringOffset floatOffset)

    // Merge pools (stdlib first, user appended with offset)
    // Reuse the pool merging functions from ANF_to_MIR
    let mergedStrings = ANF_to_MIR.appendStringPools stdlibStrings userStrings
    let mergedFloats = ANF_to_MIR.appendFloatPools stdlibFloats userFloats

    LIR.Program (stdlibFuncsNoStart @ offsetUserFuncs, mergedStrings, mergedFloats)
