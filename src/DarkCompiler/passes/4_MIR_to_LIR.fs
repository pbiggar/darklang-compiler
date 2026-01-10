// 4_MIR_to_LIR.fs - Instruction Selection (Pass 4)
//
// Transforms MIR CFG into symbolic LIR CFG (string/float constants remain symbolic).
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

/// Convert MIR.VReg to LIRSymbolic.Reg (virtual)
let vregToLIRReg (MIR.VReg id) : LIRSymbolic.Reg = LIR.Virtual id

/// Convert MIR.VReg to LIR.FReg (virtual float register)
let vregToLIRFReg (MIR.VReg id) : LIR.FReg = LIR.FVirtual id

/// Convert MIR.Operand to LIRSymbolic.Operand
let convertOperand (operand: MIR.Operand) : LIRSymbolic.Operand =
    match operand with
    | MIR.IntConst n -> LIRSymbolic.Imm n
    | MIR.BoolConst b -> LIRSymbolic.Imm (if b then 1L else 0L)  // Booleans as 0/1
    | MIR.FloatSymbol value -> LIRSymbolic.FloatSymbol value
    | MIR.StringSymbol value -> LIRSymbolic.StringSymbol value
    | MIR.Register vreg -> LIRSymbolic.Reg (vregToLIRReg vreg)
    | MIR.FuncAddr name -> LIRSymbolic.FuncAddr name  // Function address (for higher-order functions)

/// Apply type substitution - replaces type variables with concrete types
let rec applyTypeSubst (typeParams: string list) (typeArgs: AST.Type list) (typ: AST.Type) : AST.Type =
    // Build substitution map from type params to type args
    let subst = List.zip typeParams typeArgs |> Map.ofList
    let rec substitute t =
        match t with
        | AST.TVar name ->
            match Map.tryFind name subst with
            | Some concrete -> concrete
            | None -> t  // Unbound - keep as-is
        | AST.TFunction (paramTypes, retType) ->
            AST.TFunction (List.map substitute paramTypes, substitute retType)
        | AST.TTuple elemTypes -> AST.TTuple (List.map substitute elemTypes)
        | AST.TList elemType -> AST.TList (substitute elemType)
        | AST.TDict (keyType, valType) -> AST.TDict (substitute keyType, substitute valType)
        | AST.TSum (name, args) -> AST.TSum (name, List.map substitute args)
        | _ -> t  // Concrete types unchanged
    substitute typ

/// Ensure operand is in a register (may need to load immediate)
let ensureInRegister (operand: MIR.Operand) (tempReg: LIRSymbolic.Reg) : Result<LIRSymbolic.Instr list * LIRSymbolic.Reg, string> =
    match operand with
    | MIR.IntConst n ->
        // Need to load constant into a temporary register
        Ok ([LIRSymbolic.Mov (tempReg, LIRSymbolic.Imm n)], tempReg)
    | MIR.BoolConst b ->
        // Load boolean (0 or 1) into register
        Ok ([LIRSymbolic.Mov (tempReg, LIRSymbolic.Imm (if b then 1L else 0L))], tempReg)
    | MIR.FloatSymbol value ->
        // Load float into FP register, then move bits to GP register
        let tempFReg = LIR.FVirtual 999
        Ok ([LIRSymbolic.FLoad (tempFReg, value); LIRSymbolic.FpToGp (tempReg, tempFReg)], tempReg)
    | MIR.StringSymbol _ ->
        // String references are not used as operands in arithmetic operations
        Error "Internal error: Cannot use string literal as arithmetic operand"
    | MIR.Register vreg ->
        Ok ([], vregToLIRReg vreg)
    | MIR.FuncAddr name ->
        // Load function address into register using ADR instruction
        Ok ([LIRSymbolic.LoadFuncAddr (tempReg, name)], tempReg)

/// Ensure float operand is in an FP register
let ensureInFRegister (operand: MIR.Operand) (tempFReg: LIR.FReg) : Result<LIRSymbolic.Instr list * LIR.FReg, string> =
    match operand with
    | MIR.FloatSymbol value ->
        // Load float constant into FP register
        Ok ([LIRSymbolic.FLoad (tempFReg, value)], tempFReg)
    | MIR.Register vreg ->
        // Float value already in a virtual register - treat it as FVirtual
        Ok ([], vregToLIRFReg vreg)
    | MIR.IntConst _ | MIR.BoolConst _ ->
        Error "Internal error: Cannot use integer/boolean as float operand"
    | MIR.StringSymbol _ ->
        Error "Internal error: Cannot use string as float operand"
    | MIR.FuncAddr _ ->
        Error "Internal error: Cannot use function address as float operand"

/// Generate truncation instruction for sized integer arithmetic
/// After a 64-bit operation, this sign/zero extends the result to the target width
/// to ensure proper overflow behavior (e.g., 127y + 1y = -128)
let truncateForType (destReg: LIRSymbolic.Reg) (operandType: AST.Type) : LIRSymbolic.Instr list =
    match operandType with
    | AST.TInt8 -> [LIRSymbolic.Sxtb (destReg, destReg)]      // Sign-extend byte
    | AST.TInt16 -> [LIRSymbolic.Sxth (destReg, destReg)]     // Sign-extend halfword
    | AST.TInt32 -> [LIRSymbolic.Sxtw (destReg, destReg)]     // Sign-extend word
    | AST.TUInt8 -> [LIRSymbolic.Uxtb (destReg, destReg)]     // Zero-extend byte
    | AST.TUInt16 -> [LIRSymbolic.Uxth (destReg, destReg)]    // Zero-extend halfword
    | AST.TUInt32 -> [LIRSymbolic.Uxtw (destReg, destReg)]    // Zero-extend word
    | AST.TInt64 | AST.TUInt64 -> []                  // No truncation needed for 64-bit
    | _ -> []                                          // Non-integer types

/// Convert MIR instruction to LIR instructions
/// floatRegs: Set of VReg IDs that hold float values (from MIR.Function.FloatRegs)
let selectInstr (instr: MIR.Instr) (variantRegistry: MIR.VariantRegistry) (recordRegistry: MIR.RecordRegistry) (floatRegs: Set<int>) : Result<LIRSymbolic.Instr list, string> =
    match instr with
    | MIR.Mov (dest, src, valueType) ->
        // Check if this is a float move - either by valueType or by source operand type
        let isFloatMove =
            match valueType with
            | Some AST.TFloat64 -> true
            | _ -> match src with
                   | MIR.FloatSymbol _ -> true
                   | _ -> false
        if isFloatMove then
            // Float move - use FP registers
            let lirFDest = vregToLIRFReg dest
            match src with
            | MIR.FloatSymbol value ->
                // Load float constant
                Ok [LIRSymbolic.FLoad (lirFDest, value)]
            | MIR.Register vreg ->
                // Move between float registers
                let srcFReg = vregToLIRFReg vreg
                Ok [LIRSymbolic.FMov (lirFDest, srcFReg)]
            | _ ->
                Error "Internal error: non-float operand in float Mov"
        else
            // Integer/other move
            let lirDest = vregToLIRReg dest
            let lirSrc = convertOperand src
            Ok [LIRSymbolic.Mov (lirDest, lirSrc)]

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
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FAdd (lirFDest, leftFReg, rightFReg)])
            | MIR.Sub ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FSub (lirFDest, leftFReg, rightFReg)])
            | MIR.Mul ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FMul (lirFDest, leftFReg, rightFReg)])
            | MIR.Div ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FDiv (lirFDest, leftFReg, rightFReg)])
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
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FCmp (leftFReg, rightFReg); LIRSymbolic.Cset (lirDest, LIR.EQ)])
            | MIR.Neq ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FCmp (leftFReg, rightFReg); LIRSymbolic.Cset (lirDest, LIR.NE)])
            | MIR.Lt ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FCmp (leftFReg, rightFReg); LIRSymbolic.Cset (lirDest, LIR.LT)])
            | MIR.Gt ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FCmp (leftFReg, rightFReg); LIRSymbolic.Cset (lirDest, LIR.GT)])
            | MIR.Lte ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FCmp (leftFReg, rightFReg); LIRSymbolic.Cset (lirDest, LIR.LE)])
            | MIR.Gte ->
                match ensureInFRegister left (LIR.FVirtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg) ->
                match ensureInFRegister right (LIR.FVirtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.FCmp (leftFReg, rightFReg); LIRSymbolic.Cset (lirDest, LIR.GE)])
            | MIR.And | MIR.Or ->
                Error "Boolean operations not supported on floats"
            | MIR.Shl | MIR.Shr | MIR.BitAnd | MIR.BitOr | MIR.BitXor ->
                Error "Bitwise operations not supported on floats"

        | _ ->
            // Integer operations - existing logic
            // Note: After each arithmetic operation, we truncate to the target width
            // to ensure proper overflow behavior (e.g., 127y + 1y = -128 for Int8)
            let truncInstrs = truncateForType lirDest operandType
            match op with
            | MIR.Add ->
                // ADD can have immediate or register as right operand
                // Left operand must be in a register
                match ensureInRegister left lirDest with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Add (lirDest, leftReg, rightOp)] @ truncInstrs)

            | MIR.Sub ->
                // SUB can have immediate or register as right operand
                match ensureInRegister left lirDest with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Sub (lirDest, leftReg, rightOp)] @ truncInstrs)

            | MIR.Mul ->
                // MUL requires both operands in registers
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Mul (lirDest, leftReg, rightReg)] @ truncInstrs)

            | MIR.Div ->
                // SDIV requires both operands in registers
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Sdiv (lirDest, leftReg, rightReg)] @ truncInstrs)

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
                        [LIRSymbolic.Sdiv (quotReg, leftReg, rightReg);
                         LIRSymbolic.Msub (lirDest, quotReg, rightReg, leftReg)] @ truncInstrs)

            // Comparisons: CMP + CSET sequence
            | MIR.Eq ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Cmp (leftReg, rightOp); LIRSymbolic.Cset (lirDest, LIR.EQ)])

            | MIR.Neq ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Cmp (leftReg, rightOp); LIRSymbolic.Cset (lirDest, LIR.NE)])

            | MIR.Lt ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Cmp (leftReg, rightOp); LIRSymbolic.Cset (lirDest, LIR.LT)])

            | MIR.Gt ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Cmp (leftReg, rightOp); LIRSymbolic.Cset (lirDest, LIR.GT)])

            | MIR.Lte ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Cmp (leftReg, rightOp); LIRSymbolic.Cset (lirDest, LIR.LE)])

            | MIR.Gte ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    Ok (leftInstrs @ [LIRSymbolic.Cmp (leftReg, rightOp); LIRSymbolic.Cset (lirDest, LIR.GE)])

            // Boolean operations (bitwise for 0/1 values)
            | MIR.And ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.And (lirDest, leftReg, rightReg)])

            | MIR.Or ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Orr (lirDest, leftReg, rightReg)])

            // Bitwise operators (also need truncation for proper overflow)
            | MIR.Shl ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    // Check if shift amount is a constant (0-63)
                    match right with
                    | MIR.IntConst n when n >= 0L && n < 64L ->
                        Ok (leftInstrs @ [LIRSymbolic.Lsl_imm (lirDest, leftReg, int n)] @ truncInstrs)
                    | _ ->
                        match ensureInRegister right (LIR.Virtual 1001) with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg) ->
                            Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Lsl (lirDest, leftReg, rightReg)] @ truncInstrs)

            | MIR.Shr ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    // Check if shift amount is a constant (0-63)
                    match right with
                    | MIR.IntConst n when n >= 0L && n < 64L ->
                        Ok (leftInstrs @ [LIRSymbolic.Lsr_imm (lirDest, leftReg, int n)] @ truncInstrs)
                    | _ ->
                        match ensureInRegister right (LIR.Virtual 1001) with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg) ->
                            Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Lsr (lirDest, leftReg, rightReg)] @ truncInstrs)

            | MIR.BitAnd ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                    // Check if right operand is a valid bitmask immediate (power-of-2 minus 1)
                    // These are values like 0x1, 0x3, 0x7, 0xF, etc. (ones run from bit 0)
                    let isPowerOf2Minus1 n = n > 0L && (n &&& (n + 1L)) = 0L
                    match right with
                    | MIR.IntConst n when isPowerOf2Minus1 n ->
                        Ok (leftInstrs @ [LIRSymbolic.And_imm (lirDest, leftReg, n)] @ truncInstrs)
                    | _ ->
                        match ensureInRegister right (LIR.Virtual 1001) with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg) ->
                            Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.And (lirDest, leftReg, rightReg)] @ truncInstrs)

            | MIR.BitOr ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Orr (lirDest, leftReg, rightReg)] @ truncInstrs)

            | MIR.BitXor ->
                match ensureInRegister left (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg) ->
                match ensureInRegister right (LIR.Virtual 1001) with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg) ->
                    Ok (leftInstrs @ rightInstrs @ [LIRSymbolic.Eor (lirDest, leftReg, rightReg)] @ truncInstrs)

    | MIR.UnaryOp (dest, op, src) ->
        let lirDest = vregToLIRReg dest

        match op with
        | MIR.Neg ->
            // Check if source is a float - use FP negation
            match src with
            | MIR.FloatSymbol value ->
                // Float negation: load float into D1, negate into D0
                Ok [
                    LIRSymbolic.FLoad (LIR.FPhysical LIR.D1, value)
                    LIRSymbolic.FNeg (LIR.FPhysical LIR.D0, LIR.FPhysical LIR.D1)
                ]
            | _ ->
                // Integer negation: 0 - src
                match ensureInRegister src (LIR.Virtual 1000) with
                | Error err -> Error err
                | Ok (srcInstrs, srcReg) ->
                    Ok (srcInstrs @ [LIRSymbolic.Mov (lirDest, LIRSymbolic.Imm 0L); LIRSymbolic.Sub (lirDest, lirDest, LIRSymbolic.Reg srcReg)])

        | MIR.Not ->
            // Boolean NOT: 1 - src (since booleans are 0 or 1)
            match ensureInRegister src (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (srcInstrs, srcReg) ->
                Ok (srcInstrs @ [
                    LIRSymbolic.Mov (lirDest, LIRSymbolic.Imm 1L)
                    LIRSymbolic.Sub (lirDest, lirDest, LIRSymbolic.Reg srcReg)
                ])

        | MIR.BitNot ->
            // Bitwise NOT: flip all bits using MVN instruction
            match ensureInRegister src (LIR.Virtual 1000) with
            | Error err -> Error err
            | Ok (srcInstrs, srcReg) ->
                Ok (srcInstrs @ [LIRSymbolic.Mvn (lirDest, srcReg)])

    | MIR.Call (dest, funcName, args, argTypes, returnType) ->
        // ARM64 calling convention (AAPCS64):
        // - Integer arguments in X0-X7 (using separate counter)
        // - Float arguments in D0-D7 (using separate counter)
        // - Return value in X0 (int) or D0 (float)
        let lirDest = vregToLIRReg dest
        let intRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
        let floatRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

        // IMPORTANT: Save caller-saved registers BEFORE setting up arguments
        // Empty placeholder - register allocator will fill in the actual registers to save
        let saveInstrs = [LIRSymbolic.SaveRegs ([], [])]

        // Separate args into int and float based on argTypes
        let argsWithTypes = List.zip args argTypes
        let intArgs = argsWithTypes |> List.filter (fun (_, t) -> t <> AST.TFloat64)
        let floatArgs = argsWithTypes |> List.filter (fun (_, t) -> t = AST.TFloat64)

        // Generate ArgMoves for integer arguments
        let intArgMoves =
            if List.isEmpty intArgs then []
            else
                let argPairs =
                    List.zip (List.map fst intArgs) (List.take (List.length intArgs) intRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIRSymbolic.ArgMoves argPairs]

        // Generate FArgMoves for float arguments
        // For float literals, load into a temp FReg first, then move to D0-D7
        let floatArgMoves =
            if List.isEmpty floatArgs then []
            else
                // For each float arg, generate load if needed and create move pair
                let mutable tempFRegCounter = 3000
                let loadInstrsAndPairs =
                    List.zip (List.map fst floatArgs) (List.take (List.length floatArgs) floatRegs)
                    |> List.map (fun (arg, destReg) ->
                        match arg with
                        | MIR.FloatSymbol value ->
                            // Need to load float constant into a temp FReg
                            let tempFReg = LIR.FVirtual tempFRegCounter
                            tempFRegCounter <- tempFRegCounter + 1
                            ([LIRSymbolic.FLoad (tempFReg, value)], (destReg, tempFReg))
                        | MIR.Register vreg ->
                            // Already in a virtual register, convert to FReg
                            ([], (destReg, vregToLIRFReg vreg))
                        | _ ->
                            // Unexpected - use dummy
                            ([], (destReg, LIR.FVirtual 9999)))
                let loadInstrs = loadInstrsAndPairs |> List.collect fst
                let argPairs = loadInstrsAndPairs |> List.map snd
                loadInstrs @ [LIRSymbolic.FArgMoves argPairs]

        // Call instruction
        let callInstr = LIRSymbolic.Call (lirDest, funcName, List.map convertOperand args)

        // Restore caller-saved registers after the call
        // Empty placeholder - register allocator will fill in the actual registers to restore
        let restoreInstrs = [LIRSymbolic.RestoreRegs ([], [])]

        // Move return value from X0 or D0 to destination based on return type
        // For float returns, we use D8 (callee-saved) as intermediate to avoid conflicts:
        // - Save D0 to D8 BEFORE RestoreRegs (which clobbers D0)
        // - After RestoreRegs, copy from D8 to destination
        // This handles the case where destFReg maps to D0 (which would be clobbered by RestoreRegs).
        let moveResult =
            if returnType = AST.TFloat64 then
                // Float return: value is in D0, use D8 as safe intermediate
                let destFReg = vregToLIRFReg dest
                // First save D0 to D8 (callee-saved, not touched by RestoreRegs)
                let saveToD8 = [LIRSymbolic.FMov (LIR.FPhysical LIR.D8, LIR.FPhysical LIR.D0)]
                // After RestoreRegs, copy from D8 to actual destination
                let copyToFinal = [LIRSymbolic.FMov (destFReg, LIR.FPhysical LIR.D8)]
                (saveToD8, copyToFinal)
            else
                // Integer return: value is in X0
                let intMove =
                    match lirDest with
                    | LIR.Physical LIR.X0 -> []
                    | _ -> [LIRSymbolic.Mov (lirDest, LIRSymbolic.Reg (LIR.Physical LIR.X0))]
                ([], intMove)

        let (saveReturnValue, copyReturnValue) = moveResult
        let result = saveInstrs @ intArgMoves @ floatArgMoves @ [callInstr] @ saveReturnValue @ restoreInstrs @ copyReturnValue
        Ok result

    | MIR.TailCall (funcName, args, argTypes, _returnType) ->
        // Tail call optimization: Skip SaveRegs/RestoreRegs, use B instead of BL
        let intRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
        let floatRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

        // Separate args into int and float based on argTypes
        let argsWithTypes = List.zip args argTypes
        let intArgs = argsWithTypes |> List.filter (fun (_, t) -> t <> AST.TFloat64)
        let floatArgs = argsWithTypes |> List.filter (fun (_, t) -> t = AST.TFloat64)

        // Generate TailArgMoves for integer arguments (uses temp registers, no SaveRegs)
        let intArgMoves =
            if List.isEmpty intArgs then []
            else
                let argPairs =
                    List.zip (List.map fst intArgs) (List.take (List.length intArgs) intRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIRSymbolic.TailArgMoves argPairs]

        // Generate FArgMoves for float arguments
        let floatArgMoves =
            if List.isEmpty floatArgs then []
            else
                let mutable tempFRegCounter = 3000
                let loadInstrsAndPairs =
                    List.zip (List.map fst floatArgs) (List.take (List.length floatArgs) floatRegs)
                    |> List.map (fun (arg, destReg) ->
                        match arg with
                        | MIR.FloatSymbol value ->
                            let tempFReg = LIR.FVirtual tempFRegCounter
                            tempFRegCounter <- tempFRegCounter + 1
                            ([LIRSymbolic.FLoad (tempFReg, value)], (destReg, tempFReg))
                        | MIR.Register vreg ->
                            ([], (destReg, vregToLIRFReg vreg))
                        | _ ->
                            ([], (destReg, LIR.FVirtual 9999)))
                let loadInstrs = loadInstrsAndPairs |> List.collect fst
                let argPairs = loadInstrsAndPairs |> List.map snd
                loadInstrs @ [LIRSymbolic.FArgMoves argPairs]

        // Tail call instruction (no SaveRegs/RestoreRegs)
        let callInstr = LIRSymbolic.TailCall (funcName, List.map convertOperand args)

        Ok (intArgMoves @ floatArgMoves @ [callInstr])

    | MIR.IndirectCall (dest, func, args, _argTypes, returnType) ->
        // Indirect call through function pointer (BLR instruction)
        // Similar to direct call but uses function address in register
        let lirDest = vregToLIRReg dest
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // Save caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to save
        let saveInstrs = [LIRSymbolic.SaveRegs ([], [])]

        // IMPORTANT: Load function address into X9 FIRST, before setting up arguments.
        // The function pointer might be in X0-X7 which will be overwritten by argument moves.
        let funcOp = convertOperand func
        let loadFuncInstrs =
            match funcOp with
            | LIRSymbolic.Reg r ->
                // Always copy to X9 in case the source register is overwritten by arg moves
                [LIRSymbolic.Mov (LIR.Physical LIR.X9, LIRSymbolic.Reg r)]
            | LIRSymbolic.FuncAddr name ->
                [LIRSymbolic.LoadFuncAddr (LIR.Physical LIR.X9, name)]
            | other ->
                // Load operand into X9
                [LIRSymbolic.Mov (LIR.Physical LIR.X9, other)]

        // Use ArgMoves for parallel move - handles register clobbering correctly
        let argMoves =
            if List.isEmpty args then []
            else
                let argPairs =
                    List.zip args (List.take (List.length args) argRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIRSymbolic.ArgMoves argPairs]

        // Call through X9 (always, since we always copy to X9 now)
        let callInstr = LIRSymbolic.IndirectCall (lirDest, LIR.Physical LIR.X9, List.map convertOperand args)

        // Restore caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to restore
        let restoreInstrs = [LIRSymbolic.RestoreRegs ([], [])]

        // Move return value from X0 or D0 to destination based on return type
        let moveResult =
            if returnType = AST.TFloat64 then
                // Float return: value is in D0, move to FVirtual
                let destFReg = vregToLIRFReg dest
                [LIRSymbolic.FMov (destFReg, LIR.FPhysical LIR.D0)]
            else
                // Integer return: value is in X0
                match lirDest with
                | LIR.Physical LIR.X0 -> []
                | _ -> [LIRSymbolic.Mov (lirDest, LIRSymbolic.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ loadFuncInstrs @ argMoves @ [callInstr] @ restoreInstrs @ moveResult)

    | MIR.IndirectTailCall (func, args, _argTypes, _returnType) ->
        // Indirect tail call: use BR instead of BLR, no SaveRegs/RestoreRegs
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // Load function address into X9 FIRST
        let funcOp = convertOperand func
        let loadFuncInstrs =
            match funcOp with
            | LIRSymbolic.Reg r -> [LIRSymbolic.Mov (LIR.Physical LIR.X9, LIRSymbolic.Reg r)]
            | LIRSymbolic.FuncAddr name -> [LIRSymbolic.LoadFuncAddr (LIR.Physical LIR.X9, name)]
            | other -> [LIRSymbolic.Mov (LIR.Physical LIR.X9, other)]

        // Use TailArgMoves for parallel move (uses temp registers, no SaveRegs)
        let argMoves =
            if List.isEmpty args then []
            else
                let argPairs =
                    List.zip args (List.take (List.length args) argRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIRSymbolic.TailArgMoves argPairs]

        // Indirect tail call through X9
        let callInstr = LIRSymbolic.IndirectTailCall (LIR.Physical LIR.X9, List.map convertOperand args)

        Ok (loadFuncInstrs @ argMoves @ [callInstr])

    | MIR.ClosureAlloc (dest, funcName, captures) ->
        // Allocate closure: (func_addr, cap1, cap2, ...)
        // This is similar to TupleAlloc but first element is a function address
        let lirDest = vregToLIRReg dest
        let numSlots = 1 + List.length captures
        let sizeBytes = numSlots * 8
        let allocInstr = LIRSymbolic.HeapAlloc (lirDest, sizeBytes)
        // Store function pointer at offset 0 (always int/pointer type)
        let storeFuncInstr = LIRSymbolic.HeapStore (lirDest, 0, LIRSymbolic.FuncAddr funcName, None)
        // Store captured values at offsets 8, 16, ... (assume int/pointer for captures)
        let storeInstrs =
            captures
            |> List.mapi (fun i cap -> LIRSymbolic.HeapStore (lirDest, (i + 1) * 8, convertOperand cap, None))
        Ok (allocInstr :: storeFuncInstr :: storeInstrs)

    | MIR.ClosureCall (dest, closure, args, argTypes) ->
        // Call through closure: extract func_ptr from closure[0], call with (closure, args...)
        let lirDest = vregToLIRReg dest
        let intRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
        let floatRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

        // Save caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to save
        let saveInstrs = [LIRSymbolic.SaveRegs ([], [])]

        // Load closure into a temp register first
        let closureOp = convertOperand closure
        let closureReg = LIR.Physical LIR.X10  // Use X10 for closure (not an arg register)
        let loadClosureInstr =
            match closureOp with
            | LIRSymbolic.Reg r -> LIRSymbolic.Mov (closureReg, LIRSymbolic.Reg r)
            | other -> LIRSymbolic.Mov (closureReg, other)

        // Separate args into int and float based on argTypes
        let argsWithTypes = List.zip args argTypes
        let intArgs = argsWithTypes |> List.filter (fun (_, t) -> t <> AST.TFloat64)
        let floatArgs = argsWithTypes |> List.filter (fun (_, t) -> t = AST.TFloat64)

        // Generate ArgMoves for closure (X0) and integer arguments (X1-X7)
        let intArgMoves =
            let closureMove = (LIR.X0, LIRSymbolic.Reg closureReg)
            if List.isEmpty intArgs then
                [LIRSymbolic.ArgMoves [closureMove]]
            else
                let regularArgMoves =
                    List.zip (List.map fst intArgs) (List.skip 1 intRegs |> List.take (List.length intArgs))
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIRSymbolic.ArgMoves (closureMove :: regularArgMoves)]

        // Generate FArgMoves for float arguments (D0-D7)
        let floatArgMoves =
            if List.isEmpty floatArgs then []
            else
                let mutable tempFRegCounter = 3000
                let loadInstrsAndPairs =
                    List.zip (List.map fst floatArgs) (List.take (List.length floatArgs) floatRegs)
                    |> List.map (fun (arg, destReg) ->
                        match arg with
                        | MIR.FloatSymbol value ->
                            let tempFReg = LIR.FVirtual tempFRegCounter
                            tempFRegCounter <- tempFRegCounter + 1
                            ([LIRSymbolic.FLoad (tempFReg, value)], (destReg, tempFReg))
                        | MIR.Register vreg ->
                            ([], (destReg, vregToLIRFReg vreg))
                        | _ ->
                            ([], (destReg, LIR.FVirtual 9999)))
                let loadInstrs = loadInstrsAndPairs |> List.collect fst
                let argPairs = loadInstrsAndPairs |> List.map snd
                loadInstrs @ [LIRSymbolic.FArgMoves argPairs]

        // Load function pointer from closure[0] into X9
        // IMPORTANT: This must come AFTER argMoves because ArgMoves may use X9 as a temp
        // (e.g., StringSymbol conversion uses X9 for ADRP/ADD_label)
        // After ArgMoves, X0 contains the closure, so we load from [X0, 0]
        let loadFuncPtrInstr = LIRSymbolic.HeapLoad (LIR.Physical LIR.X9, LIR.Physical LIR.X0, 0)

        let callInstr = LIRSymbolic.ClosureCall (lirDest, LIR.Physical LIR.X9, List.map convertOperand args)

        // Restore caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to restore
        let restoreInstrs = [LIRSymbolic.RestoreRegs ([], [])]

        // Move return value from X0 to destination
        let moveResult =
            match lirDest with
            | LIR.Physical LIR.X0 -> []
            | _ -> [LIRSymbolic.Mov (lirDest, LIRSymbolic.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ [loadClosureInstr] @ intArgMoves @ floatArgMoves @ [loadFuncPtrInstr] @ [callInstr] @ restoreInstrs @ moveResult)

    | MIR.ClosureTailCall (closure, args, argTypes) ->
        // Closure tail call: skip SaveRegs/RestoreRegs, use BR
        let intRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
        let floatRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

        // Load closure into a temp register first
        let closureOp = convertOperand closure
        let closureReg = LIR.Physical LIR.X10
        let loadClosureInstr =
            match closureOp with
            | LIRSymbolic.Reg r -> LIRSymbolic.Mov (closureReg, LIRSymbolic.Reg r)
            | other -> LIRSymbolic.Mov (closureReg, other)

        // Separate args into int and float based on argTypes
        let argsWithTypes = List.zip args argTypes
        let intArgs = argsWithTypes |> List.filter (fun (_, t) -> t <> AST.TFloat64)
        let floatArgs = argsWithTypes |> List.filter (fun (_, t) -> t = AST.TFloat64)

        // Generate TailArgMoves for closure (X0) and integer arguments (X1-X7)
        let intArgMoves =
            let closureMove = (LIR.X0, LIRSymbolic.Reg closureReg)
            if List.isEmpty intArgs then
                [LIRSymbolic.TailArgMoves [closureMove]]
            else
                let regularArgMoves =
                    List.zip (List.map fst intArgs) (List.skip 1 intRegs |> List.take (List.length intArgs))
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIRSymbolic.TailArgMoves (closureMove :: regularArgMoves)]

        // Generate FArgMoves for float arguments (D0-D7)
        let floatArgMoves =
            if List.isEmpty floatArgs then []
            else
                let mutable tempFRegCounter = 3000
                let loadInstrsAndPairs =
                    List.zip (List.map fst floatArgs) (List.take (List.length floatArgs) floatRegs)
                    |> List.map (fun (arg, destReg) ->
                        match arg with
                        | MIR.FloatSymbol value ->
                            let tempFReg = LIR.FVirtual tempFRegCounter
                            tempFRegCounter <- tempFRegCounter + 1
                            ([LIRSymbolic.FLoad (tempFReg, value)], (destReg, tempFReg))
                        | MIR.Register vreg ->
                            ([], (destReg, vregToLIRFReg vreg))
                        | _ ->
                            ([], (destReg, LIR.FVirtual 9999)))
                let loadInstrs = loadInstrsAndPairs |> List.collect fst
                let argPairs = loadInstrsAndPairs |> List.map snd
                loadInstrs @ [LIRSymbolic.FArgMoves argPairs]

        // Load function pointer from closure[0] into X9
        // IMPORTANT: This must come AFTER argMoves because TailArgMoves may use X9 as a temp
        // (e.g., StringSymbol conversion uses X9 for ADRP/ADD_label)
        // After TailArgMoves, X0 contains the closure, so we load from [X0, 0]
        let loadFuncPtrInstr = LIRSymbolic.HeapLoad (LIR.Physical LIR.X9, LIR.Physical LIR.X0, 0)

        let callInstr = LIRSymbolic.ClosureTailCall (LIR.Physical LIR.X9, List.map convertOperand args)

        Ok ([loadClosureInstr] @ intArgMoves @ floatArgMoves @ [loadFuncPtrInstr] @ [callInstr])

    | MIR.HeapAlloc (dest, sizeBytes) ->
        let lirDest = vregToLIRReg dest
        Ok [LIRSymbolic.HeapAlloc (lirDest, sizeBytes)]

    | MIR.HeapStore (addr, offset, src, valueType) ->
        let lirAddr = vregToLIRReg addr
        // For float values, we need to move the float bits from FReg to GP register
        // since HeapStore uses GP registers. Use FpToGp to transfer bits.
        match src, valueType with
        | MIR.Register vreg, Some AST.TFloat64 ->
            // Float in FVirtual register - need to move bits to GP register first
            let srcFReg = vregToLIRFReg vreg
            let tempReg = LIR.Physical LIR.X9  // Use temp register for FpToGp
            // After FpToGp, value is in GP register, so use None for valueType
            // (otherwise CodeGen would try to treat X9 as a float register)
            Ok [LIRSymbolic.FpToGp (tempReg, srcFReg); LIRSymbolic.HeapStore (lirAddr, offset, LIRSymbolic.Reg tempReg, None)]
        | _ ->
            let lirSrc = convertOperand src
            Ok [LIRSymbolic.HeapStore (lirAddr, offset, lirSrc, valueType)]

    | MIR.HeapLoad (dest, addr, offset, valueType) ->
        let lirAddr = vregToLIRReg addr
        match valueType with
        | Some AST.TFloat64 ->
            // Float load: load into integer register, then move bits to float register
            let lirFDest = vregToLIRFReg dest
            let tempReg = LIR.Physical LIR.X9  // Use temp register for heap load
            Ok [LIRSymbolic.HeapLoad (tempReg, lirAddr, offset)
                LIRSymbolic.GpToFp (lirFDest, tempReg)]
        | _ ->
            // Integer/other load
            let lirDest = vregToLIRReg dest
            Ok [LIRSymbolic.HeapLoad (lirDest, lirAddr, offset)]

    | MIR.RefCountInc (addr, payloadSize) ->
        let lirAddr = vregToLIRReg addr
        Ok [LIRSymbolic.RefCountInc (lirAddr, payloadSize)]

    | MIR.RefCountDec (addr, payloadSize) ->
        let lirAddr = vregToLIRReg addr
        Ok [LIRSymbolic.RefCountDec (lirAddr, payloadSize)]

    | MIR.Print (src, valueType) ->
        // Generate appropriate print instruction based on type
        match valueType with
        | AST.TBool ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIRSymbolic.PrintBool (LIR.Physical LIR.X0)])
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIRSymbolic.PrintInt (LIR.Physical LIR.X0)])
        | AST.TFloat64 ->
            // Float needs to be in D0 for printing
            match src with
            | MIR.FloatSymbol value ->
                // Literal float - load into D0
                Ok [LIRSymbolic.FLoad (LIR.FPhysical LIR.D0, value)
                    LIRSymbolic.PrintFloat (LIR.FPhysical LIR.D0)]
            | MIR.Register vreg ->
                // Computed float - it's in an FVirtual register, move to D0 for printing
                let srcFReg = vregToLIRFReg vreg
                Ok [LIRSymbolic.FMov (LIR.FPhysical LIR.D0, srcFReg)
                    LIRSymbolic.PrintFloat (LIR.FPhysical LIR.D0)]
            | _ ->
                Error "Internal error: unexpected operand type for float print"
        | AST.TString | AST.TChar ->
            // String/Char printing uses PrintString for pool strings, PrintHeapString for heap strings
            // Char is stored as a string at runtime (single EGC)
            match src with
            | MIR.StringSymbol value ->
                Ok [LIRSymbolic.PrintString value]
            | MIR.Register vreg ->
                // Heap string (from concatenation): use PrintHeapString
                let lirReg = vregToLIRReg vreg
                Ok [LIRSymbolic.PrintHeapString lirReg]
            | other ->
                Error $"Print: Unexpected operand type for string: {other}"
        | AST.TTuple elemTypes ->
            // Tuple printing: (elem1, elem2, ...)
            // Use X19 (callee-saved) to hold tuple address throughout printing
            // since PrintChars clobbers caller-saved registers (X0-X3)
            let tupleAddrReg = LIR.Physical LIR.X19
            let saveTupleAddr =
                let srcOp = convertOperand src
                [LIRSymbolic.Mov (tupleAddrReg, srcOp)]

            // Helper to generate print instructions for a value based on its type
            let rec printValue (valueReg: LIRSymbolic.Reg) (valueType: AST.Type) : LIRSymbolic.Instr list =
                match valueType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 ->
                    [LIRSymbolic.Mov (LIR.Physical LIR.X0, LIRSymbolic.Reg valueReg)
                     LIRSymbolic.PrintInt (LIR.Physical LIR.X0)]
                | AST.TBool ->
                    [LIRSymbolic.Mov (LIR.Physical LIR.X0, LIRSymbolic.Reg valueReg)
                     LIRSymbolic.PrintBool (LIR.Physical LIR.X0)]
                | AST.TFloat64 ->
                    // Float value is in integer register as raw bits, move to D0 for printing
                    [LIRSymbolic.Mov (LIR.Physical LIR.X0, LIRSymbolic.Reg valueReg)
                     LIRSymbolic.GpToFp (LIR.FPhysical LIR.D0, LIR.Physical LIR.X0)
                     LIRSymbolic.PrintFloat (LIR.FPhysical LIR.D0)]
                | _ ->
                    // Other types: print address for now
                    [LIRSymbolic.Mov (LIR.Physical LIR.X0, LIRSymbolic.Reg valueReg)
                     LIRSymbolic.PrintInt (LIR.Physical LIR.X0)]

            // Generate instructions to print each element
            // Use no-newline versions for tuple elements
            let elemInstrs =
                elemTypes
                |> List.mapi (fun i elemType ->
                    let elemReg = LIR.Physical LIR.X0  // Load directly to X0 for printing
                    let loadInstr = LIRSymbolic.HeapLoad (elemReg, tupleAddrReg, i * 8)
                    let sepInstrs =
                        if i > 0 then [LIRSymbolic.PrintChars [byte ','; byte ' ']]  // ", "
                        else []
                    let printInstrs =
                        match elemType with
                        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 ->
                            [LIRSymbolic.PrintIntNoNewline (LIR.Physical LIR.X0)]
                        | AST.TBool ->
                            [LIRSymbolic.PrintBoolNoNewline (LIR.Physical LIR.X0)]
                        | AST.TFloat64 ->
                            // Float is in X0 as raw bits, move to D0 for printing
                            [LIRSymbolic.GpToFp (LIR.FPhysical LIR.D0, LIR.Physical LIR.X0)
                             LIRSymbolic.PrintFloatNoNewline (LIR.FPhysical LIR.D0)]
                        | t ->
                            failwith $"Unsupported tuple element type for printing: {t}"
                    sepInstrs @ [loadInstr] @ printInstrs)
                |> List.concat

            // Combine: save addr + "(" + elements + ")\n"
            let openParen = [LIRSymbolic.PrintChars [byte '(']]
            let closeParenNewline = [LIRSymbolic.PrintChars [byte ')'; byte '\n']]
            Ok (saveTupleAddr @ openParen @ elemInstrs @ closeParenNewline)

        | AST.TList elemType ->
            // Print list as [elem1, elem2, ...]
            let lirSrc = convertOperand src
            let moveToX19 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X19) -> []
                | LIRSymbolic.Reg r -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, LIRSymbolic.Reg r)]
                | other -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, other)]
            Ok (moveToX19 @ [LIRSymbolic.PrintList (LIR.Physical LIR.X19, elemType)])

        | AST.TSum (typeName, typeArgs) ->
            // Sum type printing: look up variants and generate PrintSum
            match Map.tryFind typeName variantRegistry with
            | Some typeVariants ->
                // Apply type substitution to payload types
                let substitutedVariants =
                    typeVariants.Variants |> List.map (fun v ->
                        let subPayload =
                            match v.Payload with
                            | Some payload when List.length typeVariants.TypeParams = List.length typeArgs ->
                                Some (applyTypeSubst typeVariants.TypeParams typeArgs payload)
                            | other -> other
                        (v.Name, v.Tag, subPayload))
                // Move sum pointer to X19 (callee-saved for print operations)
                let lirSrc = convertOperand src
                let moveToX19 =
                    match lirSrc with
                    | LIRSymbolic.Reg (LIR.Physical LIR.X19) -> []
                    | LIRSymbolic.Reg r -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, LIRSymbolic.Reg r)]
                    | other -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, other)]
                Ok (moveToX19 @ [LIRSymbolic.PrintSum (LIR.Physical LIR.X19, substitutedVariants)])
            | None ->
                // Unknown type, just print address
                let lirSrc = convertOperand src
                let moveToX0 =
                    match lirSrc with
                    | LIRSymbolic.Reg (LIR.Physical LIR.X0) -> []
                    | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirSrc)]
                Ok (moveToX0 @ [LIRSymbolic.PrintInt (LIR.Physical LIR.X0)])

        | AST.TRecord typeName ->
            // Print record with field names and values
            match Map.tryFind typeName recordRegistry with
            | Some fields ->
                // Move record address to callee-saved X19 (preserved through syscalls)
                let lirSrc = convertOperand src
                let moveToX19 =
                    match lirSrc with
                    | LIRSymbolic.Reg (LIR.Physical LIR.X19) -> []
                    | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, lirSrc)]
                // Convert RecordField list to tuple format for LIR
                let fieldTuples = fields |> List.map (fun f -> (f.Name, f.Type))
                Ok (moveToX19 @ [LIRSymbolic.PrintRecord (LIR.Physical LIR.X19, typeName, fieldTuples)])
            | None ->
                Error $"Print: Record type '{typeName}' not found in recordRegistry"
        | AST.TDict _ ->
            // Dict: print address for now
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIRSymbolic.PrintInt (LIR.Physical LIR.X0)])
        | AST.TUnit ->
            // Unit: print "()" with newline
            Ok [LIRSymbolic.PrintChars [byte '('; byte ')'; byte '\n']]
        | AST.TFunction _ ->
            // Functions shouldn't be printed, but just print address
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIRSymbolic.PrintInt (LIR.Physical LIR.X0)])
        | AST.TRawPtr ->
            // Raw pointer: print address
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirSrc)]
            Ok (moveToX0 @ [LIRSymbolic.PrintInt (LIR.Physical LIR.X0)])
        | AST.TBytes ->
            // Bytes: print as "<N bytes>" where N is the length
            let lirSrc = convertOperand src
            let moveToX19 =
                match lirSrc with
                | LIRSymbolic.Reg (LIR.Physical LIR.X19) -> []
                | LIRSymbolic.Reg r -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, LIRSymbolic.Reg r)]
                | other -> [LIRSymbolic.Mov (LIR.Physical LIR.X19, other)]
            Ok (moveToX19 @ [LIRSymbolic.PrintBytes (LIR.Physical LIR.X19)])
        | AST.TVar _ ->
            // Type variables should be monomorphized away before reaching LIR
            Error "Internal error: Type variable reached MIR_to_LIR (should be monomorphized)"

    | MIR.StringConcat (dest, left, right) ->
        let lirDest = vregToLIRReg dest
        let lirLeft = convertOperand left
        let lirRight = convertOperand right
        Ok [LIRSymbolic.StringConcat (lirDest, lirLeft, lirRight)]

    | MIR.FileReadText (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok [LIRSymbolic.FileReadText (lirDest, lirPath)]

    | MIR.FileExists (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok [LIRSymbolic.FileExists (lirDest, lirPath)]

    | MIR.FileWriteText (dest, path, content) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        let lirContent = convertOperand content
        Ok [LIRSymbolic.FileWriteText (lirDest, lirPath, lirContent)]

    | MIR.FileAppendText (dest, path, content) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        let lirContent = convertOperand content
        Ok [LIRSymbolic.FileAppendText (lirDest, lirPath, lirContent)]

    | MIR.FileDelete (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok [LIRSymbolic.FileDelete (lirDest, lirPath)]

    | MIR.FileSetExecutable (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok [LIRSymbolic.FileSetExecutable (lirDest, lirPath)]

    | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        // ptr and length must be in registers
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg) ->
        match ensureInRegister length (LIR.Virtual 1001) with
        | Error err -> Error err
        | Ok (lengthInstrs, lengthReg) ->
            Ok (ptrInstrs @ lengthInstrs @ [LIRSymbolic.FileWriteFromPtr (lirDest, lirPath, ptrReg, lengthReg)])

    | MIR.RawAlloc (dest, numBytes) ->
        let lirDest = vregToLIRReg dest
        // numBytes must be in a register for LIR
        match ensureInRegister numBytes (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (loadInstrs, numBytesReg) ->
            Ok (loadInstrs @ [LIRSymbolic.RawAlloc (lirDest, numBytesReg)])

    | MIR.RawFree ptr ->
        // ptr must be in a register
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (loadInstrs, ptrReg) ->
            Ok (loadInstrs @ [LIRSymbolic.RawFree ptrReg])

    | MIR.RawGet (dest, ptr, byteOffset, valueType) ->
        // Both ptr and byteOffset must be in registers
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg) ->
        match ensureInRegister byteOffset (LIR.Virtual 1001) with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg) ->
            match valueType with
            | Some AST.TFloat64 ->
                // Float load: load raw bits into GP register, then move to FP register
                let lirFDest = vregToLIRFReg dest
                let tempReg = LIR.Physical LIR.X9  // Use temp register for raw get
                Ok (ptrInstrs @ offsetInstrs @ [LIRSymbolic.RawGet (tempReg, ptrReg, offsetReg); LIRSymbolic.GpToFp (lirFDest, tempReg)])
            | _ ->
                // Integer/other load
                let lirDest = vregToLIRReg dest
                Ok (ptrInstrs @ offsetInstrs @ [LIRSymbolic.RawGet (lirDest, ptrReg, offsetReg)])

    | MIR.RawGetByte (dest, ptr, byteOffset) ->
        let lirDest = vregToLIRReg dest
        // Both ptr and byteOffset must be in registers
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg) ->
        match ensureInRegister byteOffset (LIR.Virtual 1001) with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg) ->
            Ok (ptrInstrs @ offsetInstrs @ [LIRSymbolic.RawGetByte (lirDest, ptrReg, offsetReg)])

    | MIR.RawSet (ptr, byteOffset, value, valueType) ->
        // All three operands must be in registers
        match ensureInRegister ptr (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg) ->
        match ensureInRegister byteOffset (LIR.Virtual 1001) with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg) ->
        // Handle StringSymbol specially - use Mov which CodeGen handles (converts to heap format)
        match value with
        | MIR.StringSymbol _ ->
            let tempReg = LIR.Virtual 1002
            let movInstr = LIRSymbolic.Mov (tempReg, convertOperand value)
            Ok (ptrInstrs @ offsetInstrs @ [movInstr; LIRSymbolic.RawSet (ptrReg, offsetReg, tempReg)])
        | _ ->
            match valueType with
            | Some AST.TFloat64 ->
                // Float store: ensure value is in FP register, then convert to GP for storage
                match ensureInFRegister value (LIR.FVirtual 1002) with
                | Error err -> Error err
                | Ok (valueInstrs, valueFReg) ->
                    let tempReg = LIR.Physical LIR.X9  // Use temp register for FpToGp
                    Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIRSymbolic.FpToGp (tempReg, valueFReg); LIRSymbolic.RawSet (ptrReg, offsetReg, tempReg)])
            | _ ->
                match ensureInRegister value (LIR.Virtual 1002) with
                | Error err -> Error err
                | Ok (valueInstrs, valueReg) ->
                    Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIRSymbolic.RawSet (ptrReg, offsetReg, valueReg)])

    | MIR.RawSetByte (ptr, byteOffset, value) ->
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
            Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIRSymbolic.RawSetByte (ptrReg, offsetReg, valueReg)])

    | MIR.FloatSqrt (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIRSymbolic.FSqrt (lirFDest, srcFReg)])

    | MIR.FloatAbs (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIRSymbolic.FAbs (lirFDest, srcFReg)])

    | MIR.FloatNeg (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIRSymbolic.FNeg (lirFDest, srcFReg)])

    | MIR.IntToFloat (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        // src is an integer operand that needs to be in an integer register
        match ensureInRegister src (LIR.Virtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcReg) ->
            Ok (srcInstrs @ [LIRSymbolic.IntToFloat (lirFDest, srcReg)])

    | MIR.FloatToInt (dest, src) ->
        let lirDest = vregToLIRReg dest
        // src is a float operand that needs to be in a float register
        match ensureInFRegister src (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg) ->
            Ok (srcInstrs @ [LIRSymbolic.FloatToInt (lirDest, srcFReg)])

    | MIR.StringHash (dest, str) ->
        let lirDest = vregToLIRReg dest
        let lirStr = convertOperand str
        Ok [LIRSymbolic.StringHash (lirDest, lirStr)]

    | MIR.StringEq (dest, left, right) ->
        let lirDest = vregToLIRReg dest
        let lirLeft = convertOperand left
        let lirRight = convertOperand right
        Ok [LIRSymbolic.StringEq (lirDest, lirLeft, lirRight)]

    | MIR.RefCountIncString str ->
        let lirStr = convertOperand str
        Ok [LIRSymbolic.RefCountIncString lirStr]

    | MIR.RefCountDecString str ->
        let lirStr = convertOperand str
        Ok [LIRSymbolic.RefCountDecString lirStr]

    | MIR.RandomInt64 dest ->
        let lirDest = vregToLIRReg dest
        Ok [LIRSymbolic.RandomInt64 lirDest]

    | MIR.FloatToString (dest, value) ->
        let lirDest = vregToLIRReg dest
        // Ensure value is in an FP register
        match ensureInFRegister value (LIR.FVirtual 1000) with
        | Error err -> Error err
        | Ok (valueInstrs, valueFReg) ->
            Ok (valueInstrs @ [LIRSymbolic.FloatToString (lirDest, valueFReg)])

    | MIR.CoverageHit exprId ->
        Ok [LIRSymbolic.CoverageHit exprId]

    | MIR.Phi (dest, sources, valueType) ->
        // Convert MIR.Phi to LIRSymbolic.Phi (int) or LIRSymbolic.FPhi (float)
        // Check if this is a float phi by:
        // 1. valueType is Some TFloat64 (set by SSA for parameters), OR
        // 2. destination VReg is in floatRegs (set during MIR generation and SSA renaming)
        let (MIR.VReg destId) = dest
        let isFloatPhi =
            match valueType with
            | Some AST.TFloat64 -> true
            | _ -> Set.contains destId floatRegs
        if isFloatPhi then
            // Float phi uses FReg (FVirtual) registers
            let lirDest = vregToLIRFReg dest
            let rec buildSources (remaining: (MIR.Operand * MIR.Label) list) : Result<(LIR.FReg * LIR.Label) list, string> =
                match remaining with
                | [] -> Ok []
                | (op, MIR.Label lbl) :: rest ->
                    match op with
                    | MIR.Register vreg ->
                        match buildSources rest with
                        | Error err -> Error err
                        | Ok tail -> Ok ((vregToLIRFReg vreg, LIR.Label lbl) :: tail)
                    | _ -> Error $"FPhi source must be a register, got: {op}"
            match buildSources sources with
            | Error err -> Error err
            | Ok lirSources -> Ok [LIRSymbolic.FPhi (lirDest, lirSources)]
        else
            // Integer phi uses Reg (Virtual) registers
            let lirDest = vregToLIRReg dest
            let lirSources =
                sources |> List.map (fun (op, MIR.Label lbl) ->
                    (convertOperand op, LIR.Label lbl))
            Ok [LIRSymbolic.Phi (lirDest, lirSources, valueType)]

/// Convert MIR terminator to LIR terminator
/// For Branch, need to convert operand to register (may add instructions)
/// Printing is now handled by MIR.Print instruction, not in terminator
let selectTerminator (terminator: MIR.Terminator) (returnType: AST.Type) : Result<LIRSymbolic.Instr list * LIRSymbolic.Terminator, string> =
    match terminator with
    | MIR.Ret operand ->
        match operand with
        | MIR.FloatSymbol value ->
            // Load float into D0 for return
            let loadFloat = LIRSymbolic.FLoad (LIR.FPhysical LIR.D0, value)
            Ok ([loadFloat], LIRSymbolic.Ret)
        | MIR.BoolConst b ->
            // Return bool as 0/1
            let lirOp = LIRSymbolic.Imm (if b then 1L else 0L)
            let moveToX0 = [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIRSymbolic.Ret)
        | MIR.StringSymbol _ ->
            // Strings don't have a meaningful register return value
            // The value has been printed, so just exit with code 0
            Ok ([LIRSymbolic.Exit], LIRSymbolic.Ret)
        | MIR.Register vreg when returnType = AST.TFloat64 ->
            // Float return - move to D0 via FMov
            let srcFReg = vregToLIRFReg vreg
            let moveToD0 = [LIRSymbolic.FMov (LIR.FPhysical LIR.D0, srcFReg)]
            Ok (moveToD0, LIRSymbolic.Ret)
        | _ ->
            // Integer/other return - move operand to X0
            let lirOp = convertOperand operand
            let moveToX0 = [LIRSymbolic.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIRSymbolic.Ret)

    | MIR.Branch (condOp, trueLabel, falseLabel) ->
        // Convert MIR.Label to LIR.Label
        let (MIR.Label trueLbl) = trueLabel
        let (MIR.Label falseLbl) = falseLabel

        // Condition must be in a register for ARM64 branch instructions
        match ensureInRegister condOp (LIR.Virtual 1002) with
        | Error err -> Error err
        | Ok (condInstrs, condReg) ->
            Ok (condInstrs, LIRSymbolic.Branch (condReg, LIR.Label trueLbl, LIR.Label falseLbl))

    | MIR.Jump label ->
        let (MIR.Label lbl) = label
        Ok ([], LIRSymbolic.Jump (LIR.Label lbl))

/// Convert MIR label to LIR label
let convertLabel (MIR.Label lbl) : LIR.Label = LIR.Label lbl

/// Helper: collect Results from a list, returning Error on first failure
let private collectResults (results: Result<'a list, string> list) : Result<'a list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | (Error err) :: _ -> Error err
        | (Ok instrs) :: rest -> loop (instrs :: acc) rest
    loop [] results

/// Convert MIR basic block to LIR basic block
let selectBlock (block: MIR.BasicBlock) (variantRegistry: MIR.VariantRegistry) (recordRegistry: MIR.RecordRegistry) (returnType: AST.Type) (floatRegs: Set<int>) : Result<LIRSymbolic.BasicBlock, string> =
    let lirLabel = convertLabel block.Label

    // Convert all instructions
    let instrResults = block.Instrs |> List.map (fun i -> selectInstr i variantRegistry recordRegistry floatRegs)
    match collectResults instrResults with
    | Error err -> Error err
    | Ok lirInstrs ->

    // Convert terminator (may add instructions)
    match selectTerminator block.Terminator returnType with
    | Error err -> Error err
    | Ok (termInstrs, lirTerm) ->

    Ok {
        Label = lirLabel
        Instrs = lirInstrs @ termInstrs
        Terminator = lirTerm
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
let selectCFG (cfg: MIR.CFG) (variantRegistry: MIR.VariantRegistry) (recordRegistry: MIR.RecordRegistry) (returnType: AST.Type) (floatRegs: Set<int>) : Result<LIRSymbolic.CFG, string> =
    let lirEntry = convertLabel cfg.Entry

    let blockList = cfg.Blocks |> Map.toList
    match mapResults (fun (label, block) ->
        match selectBlock block variantRegistry recordRegistry returnType floatRegs with
        | Error err -> Error err
        | Ok lirBlock -> Ok (convertLabel label, lirBlock)) blockList with
    | Error err -> Error err
    | Ok lirBlockList ->

    Ok {
        Entry = lirEntry
        Blocks = Map.ofList lirBlockList
    }

/// Check if any function has more than 8 parameters (ARM64 calling convention limit)
let private checkParameterLimits (mirFuncs: MIR.Function list) : Result<unit, string> =
    let funcWithTooManyParams =
        mirFuncs
        |> List.tryFind (fun f -> List.length f.TypedParams > 8)
    match funcWithTooManyParams with
    | Some f ->
        Error $"Function '{f.Name}' has {List.length f.TypedParams} parameters, but only 8 are supported (ARM64 calling convention limit)"
    | None -> Ok ()

/// Check if any function call has more than 8 arguments
let private checkCallArgLimits (mirFuncs: MIR.Function list) : Result<unit, string> =
    let checkBlock (block: MIR.BasicBlock) =
        block.Instrs
        |> List.tryPick (fun instr ->
            match instr with
            | MIR.Call (_, funcName, args, _, _) when List.length args > 8 ->
                Some $"Call to '{funcName}' has {List.length args} arguments, but only 8 are supported (ARM64 calling convention limit)"
            | MIR.IndirectCall (_, _, args, _, _) when List.length args > 8 ->
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
let toLIR (program: MIR.Program) : Result<LIRSymbolic.Program, string> =
    let (MIR.Program (mirFuncs, variantRegistry, recordRegistry)) = program

    // Pre-check: verify all functions have ≤8 parameters and calls have ≤8 arguments
    match checkParameterLimits mirFuncs with
    | Error err -> Error err
    | Ok () ->
    match checkCallArgLimits mirFuncs with
    | Error err -> Error err
    | Ok () ->

    // Convert each MIR function to LIR
    let convertFunc (mirFunc: MIR.Function) : Result<LIRSymbolic.Function, string> =
        match selectCFG mirFunc.CFG variantRegistry recordRegistry mirFunc.ReturnType mirFunc.FloatRegs with
        | Error err -> Error err
        | Ok lirCFG ->
            // Convert MIR TypedParams to LIR TypedLIRParams
            let lirTypedParams : LIRSymbolic.TypedLIRParam list =
                mirFunc.TypedParams
                |> List.map (fun tp ->
                    let (MIR.VReg id) = tp.Reg
                    { Reg = LIR.Virtual id; Type = tp.Type })
            Ok
                { Name = mirFunc.Name
                  TypedParams = lirTypedParams
                  CFG = lirCFG
                  StackSize = 0  // Will be determined by register allocation
                  UsedCalleeSaved = [] }  // Will be determined by register allocation

    match mapResults convertFunc mirFuncs with
    | Error err -> Error err
    | Ok lirFuncs ->
        Ok (LIRSymbolic.Program lirFuncs)
