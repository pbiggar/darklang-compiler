// 4_MIR_to_LIR.fs - Instruction Selection (Pass 4)
//
// Transforms MIR CFG into symbolic LIR CFG (string/float constants remain symbolic).
//
// Instruction selection algorithm:
// - Converts MIR basic blocks to LIR basic blocks
// - Selects target-neutral LIR operations for each MIR operation
// - Handles LIR operand constraints that later backends lower to real instructions:
//   - ADD/SUB: support 12-bit immediates, left operand must be register
//   - MUL/SDIV: both operands must be registers
// - Inserts MOV instructions to load immediates when needed
// - Converts MIR terminators to LIR terminators
// - Preserves CFG structure (labels, branches, jumps)

module MIR_to_LIR

open ResultList

let moduloNegativeDivisorErrorMessage =
    "Error when executing Script. Call-stack:\nCall stack (last call at bottom):\n\nScript error: Cannot evaluate modulus against a negative number"

/// Convert MIR.VReg to LIR.Reg (virtual)
let vregToLIRReg (MIR.VReg id) : LIR.Reg = LIR.Virtual id

/// Convert MIR.VReg to LIR.FReg (virtual float register)
let vregToLIRFReg (MIR.VReg id) : LIR.FReg = LIR.FVirtual id

/// Convert MIR.Operand to LIR.Operand
let convertOperand (operand: MIR.Operand) : LIR.Operand =
    match operand with
    | MIR.Int64Const n -> LIR.Imm n
    | MIR.BoolConst b -> LIR.Imm (if b then 1L else 0L)  // Booleans as 0/1
    | MIR.FloatSymbol value -> LIR.FloatSymbol value
    | MIR.StringSymbol value -> LIR.StringSymbol value
    | MIR.Register vreg -> LIR.Reg (vregToLIRReg vreg)
    | MIR.FuncAddr name -> LIR.FuncAddr name  // Function address (for higher-order functions)

/// Apply type substitution - replaces type variables with concrete types
let rec applyTypeSubst (typeParams: string list) (typeArgs: AST.Type list) (typ: AST.Type) : AST.Type =
    // Build substitution map from type params to type args
    let subst =
        if List.length typeParams = List.length typeArgs then
            List.zip typeParams typeArgs |> Map.ofList
        else
            Crash.crash $"applyTypeSubst: type argument mismatch: params={typeParams.Length}, args={typeArgs.Length}"
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

let private collectTypeVars (typ: AST.Type) : string list =
    let rec collect t =
        match t with
        | AST.TVar name -> [name]
        | AST.TFunction (paramTypes, retType) -> List.collect collect paramTypes @ collect retType
        | AST.TTuple elemTypes -> List.collect collect elemTypes
        | AST.TList elemType -> collect elemType
        | AST.TDict (keyType, valType) -> collect keyType @ collect valType
        | AST.TRecord (_, args) | AST.TSum (_, args) -> List.collect collect args
        | _ -> []
    collect typ |> List.distinct

let private inferRecordTypeParamsFromFields (fields: MIR.RecordField list) : string list =
    fields |> List.collect (fun field -> collectTypeVars field.Type) |> List.distinct

type TempState =
    { NextRegId: int
      NextFRegId: int }

let freshTempReg (state: TempState) : LIR.Reg * TempState =
    let reg = LIR.Virtual state.NextRegId
    (reg, { state with NextRegId = state.NextRegId + 1 })

let freshTempFReg (state: TempState) : LIR.FReg * TempState =
    let reg = LIR.FVirtual state.NextFRegId
    (reg, { state with NextFRegId = state.NextFRegId + 1 })

let private rcSumShapeRegistryFromVariantRegistry (variantRegistry: MIR.VariantRegistry) : ANF.RcSumShapeRegistry =
    variantRegistry
    |> Map.map (fun _typeName typeVariants ->
        { ANF.TypeParams = typeVariants.TypeParams
          ANF.Payloads =
            typeVariants.Variants
            |> List.sortBy (fun variant -> variant.Tag)
            |> List.map (fun variant -> variant.Tag, variant.Payload) })

let private rcMetadataForPrintType
    (variantRegistry: MIR.VariantRegistry)
    (recordRegistry: MIR.RecordRegistry)
    (typ: AST.Type)
    : ANF.RcMetadata =
    let anfRecordRegistry =
        recordRegistry
        |> Map.map (fun _typeName fields ->
            fields |> List.map (fun field -> field.Name, field.Type))
    { ANF.ReleasePlan = Some (ANF.rcReleasePlanOfTypeWithSums anfRecordRegistry (rcSumShapeRegistryFromVariantRegistry variantRegistry) typ)
      ANF.SourceType = Some typ }

let private releasePrintedValueFromReg
    (variantRegistry: MIR.VariantRegistry)
    (recordRegistry: MIR.RecordRegistry)
    (reg: LIR.Reg)
    (typ: AST.Type)
    : LIR.Instr list =
    let anfRecordRegistry =
        recordRegistry
        |> Map.map (fun _typeName fields ->
            fields |> List.map (fun field -> field.Name, field.Type))
    let shape = ANF.rcShapeOfTypeWithSums anfRecordRegistry (rcSumShapeRegistryFromVariantRegistry variantRegistry) typ
    match ANF.rcShapeReleaseOperation shape with
    | Some ANF.DynamicStringBuffer ->
        [LIR.RefCountDecString (LIR.Reg reg)]
    | Some ANF.DynamicBlobBuffer ->
        [LIR.RefCountDecBlob (LIR.Reg reg)]
    | Some (ANF.FixedSizeRoot (payloadSize, kind)) ->
        let lirKind =
            match kind with
            | ANF.GenericHeap -> LIR.GenericHeap
            | ANF.TaggedList -> LIR.TaggedList
            | ANF.DictHeap -> LIR.DictHeap
            | ANF.ClosureHeap -> LIR.ClosureHeap
        [LIR.RefCountDec (reg, payloadSize, lirKind, Some (rcMetadataForPrintType variantRegistry recordRegistry typ))]
    | None ->
        []

let private releasePrintedValue
    (variantRegistry: MIR.VariantRegistry)
    (recordRegistry: MIR.RecordRegistry)
    (src: MIR.Operand)
    (typ: AST.Type)
    : LIR.Instr list =
    match src with
    | MIR.Register vreg ->
        releasePrintedValueFromReg variantRegistry recordRegistry (vregToLIRReg vreg) typ
    | _ ->
        []

/// Ensure operand is in a register (may need to load immediate)
let ensureInRegister (operand: MIR.Operand) (state: TempState) : Result<LIR.Instr list * LIR.Reg * TempState, string> =
    match operand with
    | MIR.Int64Const n ->
        // Need to load constant into a temporary register
        let (tempReg, nextState) = freshTempReg state
        Ok ([LIR.Mov (tempReg, LIR.Imm n)], tempReg, nextState)
    | MIR.BoolConst b ->
        // Load boolean (0 or 1) into register
        let (tempReg, nextState) = freshTempReg state
        Ok ([LIR.Mov (tempReg, LIR.Imm (if b then 1L else 0L))], tempReg, nextState)
    | MIR.FloatSymbol value ->
        // Load float into FP register, then move bits to GP register
        let (tempReg, stateAfterReg) = freshTempReg state
        let (tempFReg, nextState) = freshTempFReg stateAfterReg
        Ok ([LIR.FLoad (tempFReg, value); LIR.FpToGp (tempReg, tempFReg)], tempReg, nextState)
    | MIR.StringSymbol _ ->
        // String references are not used as operands in arithmetic operations
        Error "Internal error: Cannot use string literal as arithmetic operand"
    | MIR.Register vreg ->
        Ok ([], vregToLIRReg vreg, state)
    | MIR.FuncAddr name ->
        // Load function address into register using ADR instruction
        let (tempReg, nextState) = freshTempReg state
        Ok ([LIR.LoadFuncAddr (tempReg, name)], tempReg, nextState)

/// Ensure a Blob handle is in a register. Blob.empty reuses the immutable
/// dynamic-buffer literal pool but is compared as a handle, never as a String.
let ensureBlobInRegister (operand: MIR.Operand) (state: TempState) : Result<LIR.Instr list * LIR.Reg * TempState, string> =
    match operand with
    | MIR.StringSymbol value ->
        let (tempReg, nextState) = freshTempReg state
        Ok ([LIR.Mov (tempReg, LIR.StringSymbol value)], tempReg, nextState)
    | MIR.Register vreg -> Ok ([], vregToLIRReg vreg, state)
    | _ -> Error "Internal error: Blob handle must be a literal or register"

/// Ensure float operand is in an FP register
let ensureInFRegister (operand: MIR.Operand) (state: TempState) : Result<LIR.Instr list * LIR.FReg * TempState, string> =
    match operand with
    | MIR.FloatSymbol value ->
        // Load float constant into FP register
        let (tempFReg, nextState) = freshTempFReg state
        Ok ([LIR.FLoad (tempFReg, value)], tempFReg, nextState)
    | MIR.Register vreg ->
        // Float value already in a virtual register - treat it as FVirtual
        Ok ([], vregToLIRFReg vreg, state)
    | MIR.Int64Const _ | MIR.BoolConst _ ->
        Error "Internal error: Cannot use integer/boolean as float operand"
    | MIR.StringSymbol _ ->
        Error "Internal error: Cannot use string as float operand"
    | MIR.FuncAddr _ ->
        Error "Internal error: Cannot use function address as float operand"

/// Generate truncation instruction for sized integer arithmetic
/// After a 64-bit operation, this sign/zero extends the result to the target width
/// to ensure proper overflow behavior (e.g., 127y + 1y = -128)
let truncateForType (destReg: LIR.Reg) (operandType: AST.Type) : LIR.Instr list =
    match operandType with
    | AST.TInt8 -> [LIR.Sxtb (destReg, destReg)]      // Sign-extend byte
    | AST.TInt16 -> [LIR.Sxth (destReg, destReg)]     // Sign-extend halfword
    | AST.TInt32 -> [LIR.Sxtw (destReg, destReg)]     // Sign-extend word
    | AST.TUInt8 -> [LIR.Uxtb (destReg, destReg)]     // Zero-extend byte
    | AST.TUInt16 -> [LIR.Uxth (destReg, destReg)]    // Zero-extend halfword
    | AST.TUInt32 -> [LIR.Uxtw (destReg, destReg)]    // Zero-extend word
    | AST.TInt64 | AST.TUInt64 -> []                  // No truncation needed for 64-bit
    | _ -> []                                          // Non-integer types

let shouldCheckNegativeDivisor (operandType: AST.Type) : bool =
    match operandType with
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 -> true
    | _ -> false

let isUnsignedIntegerType (operandType: AST.Type) : bool =
    match operandType with
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true
    | _ -> false

let comparisonCondition (operandType: AST.Type) (op: MIR.BinOp) : LIR.Condition =
    let unsigned = isUnsignedIntegerType operandType
    match op, unsigned with
    | MIR.Eq, _ -> LIR.EQ
    | MIR.Neq, _ -> LIR.NE
    | MIR.Lt, true -> LIR.ULT
    | MIR.Gt, true -> LIR.UGT
    | MIR.Lte, true -> LIR.ULE
    | MIR.Gte, true -> LIR.UGE
    | MIR.Lt, false -> LIR.LT
    | MIR.Gt, false -> LIR.GT
    | MIR.Lte, false -> LIR.LE
    | MIR.Gte, false -> LIR.GE
    | _ -> Crash.crash $"comparisonCondition: non-comparison op {op}"

let buildIntegerModuloParts
    (destReg: LIR.Reg)
    (left: MIR.Operand)
    (right: MIR.Operand)
    (operandType: AST.Type)
    (state: TempState)
    : Result<LIR.Instr list * LIR.Reg * LIR.Instr list * TempState, string> =
    match ensureInRegister left state with
    | Error err -> Error err
    | Ok (leftInstrs, leftReg, stateAfterLeft) ->
    match ensureInRegister right stateAfterLeft with
    | Error err -> Error err
    | Ok (rightInstrs, rightReg, stateAfterRight) ->
        let (quotReg, stateAfterQuot) = freshTempReg stateAfterRight
        let truncInstrs = truncateForType destReg operandType
        let (modInstrs, nextState) =
            if isUnsignedIntegerType operandType then
                ([LIR.Udiv (quotReg, leftReg, rightReg);
                  LIR.Msub (destReg, quotReg, rightReg, leftReg)]
                 @ truncInstrs,
                 stateAfterQuot)
            else
                let (xorReg, stateAfterXor) = freshTempReg stateAfterQuot
                let (remNonZeroReg, stateAfterRemNonZero) = freshTempReg stateAfterXor
                let (signMismatchReg, stateAfterSignMismatch) = freshTempReg stateAfterRemNonZero
                let (adjustFlagReg, stateAfterAdjustFlag) = freshTempReg stateAfterSignMismatch
                let (adjustReg, nextState) = freshTempReg stateAfterAdjustFlag
                ([LIR.Sdiv (quotReg, leftReg, rightReg);
                  LIR.Msub (destReg, quotReg, rightReg, leftReg);
                  LIR.Cmp (destReg, LIR.Imm 0L);
                  LIR.Cset (remNonZeroReg, LIR.NE);
                  LIR.Eor (xorReg, destReg, rightReg);
                  LIR.Cmp (xorReg, LIR.Imm 0L);
                  LIR.Cset (signMismatchReg, LIR.LT);
                  LIR.And (adjustFlagReg, remNonZeroReg, signMismatchReg);
                  LIR.Mul (adjustReg, adjustFlagReg, rightReg);
                  LIR.Add (destReg, destReg, LIR.Reg adjustReg)]
                 @ truncInstrs,
                 nextState)
        Ok (leftInstrs @ rightInstrs, rightReg, modInstrs, nextState)

let buildFloatArgMoves
    (floatArgs: MIR.Operand list)
    (destRegs: LIR.PhysFPReg list)
    (state: TempState)
    : Result<LIR.Instr list * TempState, string> =
    if List.isEmpty floatArgs then
        Ok ([], state)
    else
        let rec loop remaining regs currentState loadInstrs pairs =
            match remaining, regs with
            | [], _ -> Ok (List.rev loadInstrs |> List.concat, List.rev pairs, currentState)
            | _, [] -> Error "Internal error: not enough float arg registers"
            | arg :: rest, destReg :: regTail ->
                match arg with
                | MIR.FloatSymbol value ->
                    let (tempFReg, nextState) = freshTempFReg currentState
                    loop rest regTail nextState ([LIR.FLoad (tempFReg, value)] :: loadInstrs) ((destReg, tempFReg) :: pairs)
                | MIR.Register vreg ->
                    loop rest regTail currentState loadInstrs ((destReg, vregToLIRFReg vreg) :: pairs)
                | _ ->
                    Error "Internal error: float arg must be a float literal or register"
        match loop floatArgs destRegs state [] [] with
        | Error err -> Error err
        | Ok (loadInstrs, argPairs, nextState) ->
            Ok (loadInstrs @ [LIR.FArgMoves argPairs], nextState)

/// Convert MIR instruction to LIR instructions
/// floatRegs: Set of VReg IDs that hold float values (from MIR.Function.FloatRegs)
let selectInstr
    (instr: MIR.Instr)
    (variantRegistry: MIR.VariantRegistry)
    (recordRegistry: MIR.RecordRegistry)
    (floatRegs: Set<int>)
    (state: TempState)
    : Result<LIR.Instr list * TempState, string> =
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
                Ok ([LIR.FLoad (lirFDest, value)], state)
            | MIR.Register ((MIR.VReg regId) as vreg) ->
                if Set.contains regId floatRegs then
                    // Move between float registers
                    let srcFReg = vregToLIRFReg vreg
                    Ok ([LIR.FMov (lirFDest, srcFReg)], state)
                else
                    // Reinterpret GP register bits as float (e.g., heap-loaded float payloads)
                    let srcReg = vregToLIRReg vreg
                    Ok ([LIR.GpToFp (lirFDest, srcReg)], state)
            | _ ->
                Error "Internal error: non-float operand in float Mov"
        else
            // Integer/other move
            let lirDest = vregToLIRReg dest
            let lirSrc = convertOperand src
            Ok ([LIR.Mov (lirDest, lirSrc)], state)

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
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FAdd (lirFDest, leftFReg, rightFReg)], nextState)
            | MIR.Sub ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FSub (lirFDest, leftFReg, rightFReg)], nextState)
            | MIR.Mul ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FMul (lirFDest, leftFReg, rightFReg)], nextState)
            | MIR.Div ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FDiv (lirFDest, leftFReg, rightFReg)], nextState)
            | MIR.Mod ->
                Error "Float modulo not yet supported"
            // Float comparisons - result goes in integer register
            | MIR.Eq ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.EQ)], nextState)
            | MIR.Neq ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.NE)], nextState)
            | MIR.Lt ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.LT)], nextState)
            | MIR.Gt ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.GT)], nextState)
            | MIR.Lte ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.LE)], nextState)
            | MIR.Gte ->
                match ensureInFRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftFReg, stateAfterLeft) ->
                match ensureInFRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightFReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.FCmp (leftFReg, rightFReg); LIR.Cset (lirDest, LIR.GE)], nextState)
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
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, nextState) ->
                    Ok (leftInstrs @ [LIR.Add (lirDest, leftReg, rightOp)] @ truncInstrs, nextState)

            | MIR.Sub ->
                // SUB can have immediate or register as right operand
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, nextState) ->
                    Ok (leftInstrs @ [LIR.Sub (lirDest, leftReg, rightOp)] @ truncInstrs, nextState)

            | MIR.Mul ->
                // MUL requires both operands in registers
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                match ensureInRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Mul (lirDest, leftReg, rightReg)] @ truncInstrs, nextState)

            | MIR.Div ->
                // Division requires both operands in registers.
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                match ensureInRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg, nextState) ->
                    let divInstr =
                        if isUnsignedIntegerType operandType then
                            LIR.Udiv (lirDest, leftReg, rightReg)
                        else
                            LIR.Sdiv (lirDest, leftReg, rightReg)
                    Ok (leftInstrs @ rightInstrs @ [divInstr] @ truncInstrs, nextState)

            | MIR.Mod ->
                buildIntegerModuloParts lirDest left right operandType state
                |> Result.map (fun (loadInstrs, _rightReg, modInstrs, nextState) ->
                    (loadInstrs @ modInstrs, nextState))

            // Comparisons: CMP + CSET sequence
            | MIR.Eq ->
                let ensure = if operandType = AST.TBlob then ensureBlobInRegister else ensureInRegister
                match ensure left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                    if operandType = AST.TBlob then
                        match ensure right stateAfterLeft with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg, nextState) ->
                            Ok (leftInstrs @ rightInstrs @ [LIR.Cmp (leftReg, LIR.Reg rightReg); LIR.Cset (lirDest, LIR.EQ)], nextState)
                    else
                        Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.EQ)], stateAfterLeft)

            | MIR.Neq ->
                let ensure = if operandType = AST.TBlob then ensureBlobInRegister else ensureInRegister
                match ensure left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                    if operandType = AST.TBlob then
                        match ensure right stateAfterLeft with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg, nextState) ->
                            Ok (leftInstrs @ rightInstrs @ [LIR.Cmp (leftReg, LIR.Reg rightReg); LIR.Cset (lirDest, LIR.NE)], nextState)
                    else
                        Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.NE)], stateAfterLeft)

            | MIR.Lt ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, nextState) ->
                    Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, comparisonCondition operandType MIR.Lt)], nextState)

            | MIR.Gt ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, nextState) ->
                    Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, comparisonCondition operandType MIR.Gt)], nextState)

            | MIR.Lte ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, nextState) ->
                    Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, comparisonCondition operandType MIR.Lte)], nextState)

            | MIR.Gte ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, nextState) ->
                    Ok (leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, comparisonCondition operandType MIR.Gte)], nextState)

            // Boolean operations (bitwise for 0/1 values)
            | MIR.And ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                match ensureInRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.And (lirDest, leftReg, rightReg)], nextState)

            | MIR.Or ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                match ensureInRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Orr (lirDest, leftReg, rightReg)], nextState)

            // Bitwise operators (also need truncation for proper overflow)
            | MIR.Shl ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                    // Check if shift amount is a constant (0-63)
                    match right with
                    | MIR.Int64Const n when n >= 0L && n < 64L ->
                        Ok (leftInstrs @ [LIR.Lsl_imm (lirDest, leftReg, int n)] @ truncInstrs, stateAfterLeft)
                    | _ ->
                        match ensureInRegister right stateAfterLeft with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg, nextState) ->
                            Ok (leftInstrs @ rightInstrs @ [LIR.Lsl (lirDest, leftReg, rightReg)] @ truncInstrs, nextState)

            | MIR.Shr ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                    // Check if shift amount is a constant (0-63)
                    match right with
                    | MIR.Int64Const n when n >= 0L && n < 64L ->
                        Ok (leftInstrs @ [LIR.Lsr_imm (lirDest, leftReg, int n)] @ truncInstrs, stateAfterLeft)
                    | _ ->
                        match ensureInRegister right stateAfterLeft with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg, nextState) ->
                            Ok (leftInstrs @ rightInstrs @ [LIR.Lsr (lirDest, leftReg, rightReg)] @ truncInstrs, nextState)

            | MIR.BitAnd ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                    // Check if right operand is a valid bitmask immediate (power-of-2 minus 1)
                    // These are values like 0x1, 0x3, 0x7, 0xF, etc. (ones run from bit 0)
                    let isPowerOf2Minus1 n = n > 0L && (n &&& (n + 1L)) = 0L
                    match right with
                    | MIR.Int64Const n when isPowerOf2Minus1 n ->
                        Ok (leftInstrs @ [LIR.And_imm (lirDest, leftReg, n)] @ truncInstrs, stateAfterLeft)
                    | _ ->
                        match ensureInRegister right stateAfterLeft with
                        | Error err -> Error err
                        | Ok (rightInstrs, rightReg, nextState) ->
                            Ok (leftInstrs @ rightInstrs @ [LIR.And (lirDest, leftReg, rightReg)] @ truncInstrs, nextState)

            | MIR.BitOr ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                match ensureInRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Orr (lirDest, leftReg, rightReg)] @ truncInstrs, nextState)

            | MIR.BitXor ->
                match ensureInRegister left state with
                | Error err -> Error err
                | Ok (leftInstrs, leftReg, stateAfterLeft) ->
                match ensureInRegister right stateAfterLeft with
                | Error err -> Error err
                | Ok (rightInstrs, rightReg, nextState) ->
                    Ok (leftInstrs @ rightInstrs @ [LIR.Eor (lirDest, leftReg, rightReg)] @ truncInstrs, nextState)

    | MIR.UnaryOp (dest, op, src) ->
        let lirDest = vregToLIRReg dest

        match op with
        | MIR.Neg ->
            // Check if source is a float - use FP negation
            match src with
            | MIR.FloatSymbol value ->
                // Float negation: load float into D1, negate into D0
                Ok ([
                    LIR.FLoad (LIR.FPhysical LIR.D1, value)
                    LIR.FNeg (LIR.FPhysical LIR.D0, LIR.FPhysical LIR.D1)
                ], state)
            | _ ->
                // Integer negation: 0 - src
                match ensureInRegister src state with
                | Error err -> Error err
                | Ok (srcInstrs, srcReg, nextState) ->
                    Ok (srcInstrs @ [LIR.Mov (lirDest, LIR.Imm 0L); LIR.Sub (lirDest, lirDest, LIR.Reg srcReg)], nextState)

        | MIR.Not ->
            // Boolean NOT: 1 - src (since booleans are 0 or 1)
            match ensureInRegister src state with
            | Error err -> Error err
            | Ok (srcInstrs, srcReg, nextState) ->
                Ok (srcInstrs @ [
                    LIR.Mov (lirDest, LIR.Imm 1L)
                    LIR.Sub (lirDest, lirDest, LIR.Reg srcReg)
                ], nextState)

        | MIR.BitNot ->
            // Bitwise NOT: flip all bits using MVN instruction
            match ensureInRegister src state with
            | Error err -> Error err
            | Ok (srcInstrs, srcReg, nextState) ->
                Ok (srcInstrs @ [LIR.Mvn (lirDest, srcReg)], nextState)

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
        let saveInstrs = [LIR.SaveRegs ([], [])]

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
                [LIR.ArgMoves argPairs]

        let floatArgMovesResult =
            if List.isEmpty floatArgs then
                Ok ([], state)
            else
                let floatOperands = List.map fst floatArgs
                let destRegs = List.take (List.length floatArgs) floatRegs
                buildFloatArgMoves floatOperands destRegs state

        // Call instruction
        let callInstr = LIR.Call (lirDest, funcName, List.map convertOperand args)

        // Restore caller-saved registers after the call
        // Empty placeholder - register allocator will fill in the actual registers to restore
        let restoreInstrs = [LIR.RestoreRegs ([], [])]

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
                let saveToD8 = [LIR.FMov (LIR.FPhysical LIR.D8, LIR.FPhysical LIR.D0)]
                // After RestoreRegs, copy from D8 to actual destination
                let copyToFinal = [LIR.FMov (destFReg, LIR.FPhysical LIR.D8)]
                (saveToD8, copyToFinal)
            else
                // Integer return: value is in X0
                let intMove =
                    match lirDest with
                    | LIR.Physical LIR.X0 -> []
                    | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]
                ([], intMove)

        let (saveReturnValue, copyReturnValue) = moveResult
        match floatArgMovesResult with
        | Error err -> Error err
        | Ok (floatArgMoves, nextState) ->
            let result = saveInstrs @ intArgMoves @ floatArgMoves @ [callInstr] @ saveReturnValue @ restoreInstrs @ copyReturnValue
            Ok (result, nextState)

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
                [LIR.TailArgMoves argPairs]

        // Generate FArgMoves for float arguments
        let floatArgMovesResult =
            if List.isEmpty floatArgs then
                Ok ([], state)
            else
                let floatOperands = List.map fst floatArgs
                let destRegs = List.take (List.length floatArgs) floatRegs
                buildFloatArgMoves floatOperands destRegs state

        // Tail call instruction (no SaveRegs/RestoreRegs)
        let callInstr = LIR.TailCall (funcName, List.map convertOperand args)

        match floatArgMovesResult with
        | Error err -> Error err
        | Ok (floatArgMoves, nextState) ->
            Ok (intArgMoves @ floatArgMoves @ [callInstr], nextState)

    | MIR.IndirectCall (dest, func, args, _argTypes, returnType) ->
        // Indirect call through function pointer (BLR instruction)
        // Similar to direct call but uses function address in register
        let lirDest = vregToLIRReg dest
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // Save caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to save
        let saveInstrs = [LIR.SaveRegs ([], [])]

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
        // Empty placeholder - register allocator will fill in the actual registers to restore
        let restoreInstrs = [LIR.RestoreRegs ([], [])]

        // Move return value from X0 or D0 to destination based on return type
        let moveResult =
            if returnType = AST.TFloat64 then
                // Float return: value is in D0, move to FVirtual
                let destFReg = vregToLIRFReg dest
                [LIR.FMov (destFReg, LIR.FPhysical LIR.D0)]
            else
                // Integer return: value is in X0
                match lirDest with
                | LIR.Physical LIR.X0 -> []
                | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]

        Ok (saveInstrs @ loadFuncInstrs @ argMoves @ [callInstr] @ restoreInstrs @ moveResult, state)

    | MIR.IndirectTailCall (func, args, _argTypes, _returnType) ->
        // Indirect tail call: use BR instead of BLR, no SaveRegs/RestoreRegs
        let argRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]

        // Load function address into X9 FIRST
        let funcOp = convertOperand func
        let loadFuncInstrs =
            match funcOp with
            | LIR.Reg r -> [LIR.Mov (LIR.Physical LIR.X9, LIR.Reg r)]
            | LIR.FuncAddr name -> [LIR.LoadFuncAddr (LIR.Physical LIR.X9, name)]
            | other -> [LIR.Mov (LIR.Physical LIR.X9, other)]

        // Use TailArgMoves for parallel move (uses temp registers, no SaveRegs)
        let argMoves =
            if List.isEmpty args then []
            else
                let argPairs =
                    List.zip args (List.take (List.length args) argRegs)
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIR.TailArgMoves argPairs]

        // Indirect tail call through X9
        let callInstr = LIR.IndirectTailCall (LIR.Physical LIR.X9, List.map convertOperand args)

        Ok (loadFuncInstrs @ argMoves @ [callInstr], state)

    | MIR.ClosureAlloc (dest, funcName, captures) ->
        // Allocate closure: (func_addr, cap1, cap2, ...)
        // This is similar to TupleAlloc but first element is a function address
        let lirDest = vregToLIRReg dest
        let numSlots = 1 + List.length captures
        let sizeBytes = numSlots * 8
        let allocInstr = LIR.HeapAlloc (lirDest, sizeBytes)
        // Store function pointer at offset 0 (always int/pointer type)
        let storeFuncInstr = LIR.HeapStore (lirDest, 0, LIR.FuncAddr funcName, None)
        // Store captured values at offsets 8, 16, ... (assume int/pointer for captures)
        let storeInstrs =
            captures
            |> List.mapi (fun i cap -> LIR.HeapStore (lirDest, (i + 1) * 8, convertOperand cap, None))
        Ok (allocInstr :: storeFuncInstr :: storeInstrs, state)

    | MIR.ClosureCall (dest, closure, args, argTypes, returnType) ->
        // Call through closure: extract func_ptr from closure[0], call with (closure, args...)
        let lirDest = vregToLIRReg dest
        let intRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
        let floatRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

        // Save caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to save
        let saveInstrs = [LIR.SaveRegs ([], [])]

        // Load closure into a temp register first
        let closureOp = convertOperand closure
        let closureReg = LIR.Physical LIR.X10  // Use X10 for closure (not an arg register)
        let loadClosureInstr =
            match closureOp with
            | LIR.Reg r -> LIR.Mov (closureReg, LIR.Reg r)
            | other -> LIR.Mov (closureReg, other)

        // Separate args into int and float based on argTypes
        let argsWithTypes = List.zip args argTypes
        let intArgs = argsWithTypes |> List.filter (fun (_, t) -> t <> AST.TFloat64)
        let floatArgs = argsWithTypes |> List.filter (fun (_, t) -> t = AST.TFloat64)

        // Generate ArgMoves for closure (X0) and integer arguments (X1-X7)
        let intArgMoves =
            let closureMove = (LIR.X0, LIR.Reg closureReg)
            if List.isEmpty intArgs then
                [LIR.ArgMoves [closureMove]]
            else
                let regularArgMoves =
                    List.zip (List.map fst intArgs) (List.skip 1 intRegs |> List.take (List.length intArgs))
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIR.ArgMoves (closureMove :: regularArgMoves)]

        let floatArgMovesResult =
            if List.isEmpty floatArgs then
                Ok ([], state)
            else
                let floatOperands = List.map fst floatArgs
                let destRegs = List.take (List.length floatArgs) floatRegs
                buildFloatArgMoves floatOperands destRegs state

        // Load function pointer from closure[0] into X9
        // IMPORTANT: This must come AFTER argMoves because ArgMoves may use X9 as a temp
        // (e.g., StringSymbol conversion uses X9 for ADRP/ADD_label)
        // After ArgMoves, X0 contains the closure, so we load from [X0, 0]
        let loadFuncPtrInstr = LIR.HeapLoad (LIR.Physical LIR.X9, LIR.Physical LIR.X0, 0)

        let callInstr = LIR.ClosureCall (lirDest, LIR.Physical LIR.X9, List.map convertOperand args)

        // Restore caller-saved registers
        // Empty placeholder - register allocator will fill in the actual registers to restore
        let restoreInstrs = [LIR.RestoreRegs ([], [])]

        // Move return value from X0 or D0 to destination based on return type
        // For float returns, save D0 to D8 before RestoreRegs, then copy to dest after.
        let (moveBeforeRestore, moveAfterRestore) =
            if returnType = AST.TFloat64 then
                let destFReg = vregToLIRFReg dest
                let saveToD8 = [LIR.FMov (LIR.FPhysical LIR.D8, LIR.FPhysical LIR.D0)]
                let copyToFinal = [LIR.FMov (destFReg, LIR.FPhysical LIR.D8)]
                (saveToD8, copyToFinal)
            else
                let moveResult =
                    match lirDest with
                    | LIR.Physical LIR.X0 -> []
                    | _ -> [LIR.Mov (lirDest, LIR.Reg (LIR.Physical LIR.X0))]
                ([], moveResult)

        match floatArgMovesResult with
        | Error err -> Error err
        | Ok (floatArgMoves, nextState) ->
            Ok (saveInstrs @ [loadClosureInstr] @ intArgMoves @ floatArgMoves @ [loadFuncPtrInstr] @ [callInstr] @ moveBeforeRestore @ restoreInstrs @ moveAfterRestore, nextState)

    | MIR.ClosureTailCall (closure, args, argTypes) ->
        // Closure tail call: skip SaveRegs/RestoreRegs, use BR
        let intRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
        let floatRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

        // Load closure into a temp register first
        let closureOp = convertOperand closure
        let closureReg = LIR.Physical LIR.X10
        let loadClosureInstr =
            match closureOp with
            | LIR.Reg r -> LIR.Mov (closureReg, LIR.Reg r)
            | other -> LIR.Mov (closureReg, other)

        // Separate args into int and float based on argTypes
        let argsWithTypes = List.zip args argTypes
        let intArgs = argsWithTypes |> List.filter (fun (_, t) -> t <> AST.TFloat64)
        let floatArgs = argsWithTypes |> List.filter (fun (_, t) -> t = AST.TFloat64)

        // Generate TailArgMoves for closure (X0) and integer arguments (X1-X7)
        let intArgMoves =
            let closureMove = (LIR.X0, LIR.Reg closureReg)
            if List.isEmpty intArgs then
                [LIR.TailArgMoves [closureMove]]
            else
                let regularArgMoves =
                    List.zip (List.map fst intArgs) (List.skip 1 intRegs |> List.take (List.length intArgs))
                    |> List.map (fun (arg, reg) -> (reg, convertOperand arg))
                [LIR.TailArgMoves (closureMove :: regularArgMoves)]

        let floatArgMovesResult =
            if List.isEmpty floatArgs then
                Ok ([], state)
            else
                let floatOperands = List.map fst floatArgs
                let destRegs = List.take (List.length floatArgs) floatRegs
                buildFloatArgMoves floatOperands destRegs state

        // Load function pointer from closure[0] into X9
        // IMPORTANT: This must come AFTER argMoves because TailArgMoves may use X9 as a temp
        // (e.g., StringSymbol conversion uses X9 for ADRP/ADD_label)
        // After TailArgMoves, X0 contains the closure, so we load from [X0, 0]
        let loadFuncPtrInstr = LIR.HeapLoad (LIR.Physical LIR.X9, LIR.Physical LIR.X0, 0)

        let callInstr = LIR.ClosureTailCall (LIR.Physical LIR.X9, List.map convertOperand args)

        match floatArgMovesResult with
        | Error err -> Error err
        | Ok (floatArgMoves, nextState) ->
            Ok ([loadClosureInstr] @ intArgMoves @ floatArgMoves @ [loadFuncPtrInstr] @ [callInstr], nextState)

    | MIR.HeapAlloc (dest, sizeBytes) ->
        let lirDest = vregToLIRReg dest
        Ok ([LIR.HeapAlloc (lirDest, sizeBytes)], state)

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
            Ok ([LIR.FpToGp (tempReg, srcFReg); LIR.HeapStore (lirAddr, offset, LIR.Reg tempReg, None)], state)
        | _ ->
            let lirSrc = convertOperand src
            Ok ([LIR.HeapStore (lirAddr, offset, lirSrc, valueType)], state)

    | MIR.HeapLoad (dest, addr, offset, valueType) ->
        let lirAddr = vregToLIRReg addr
        match valueType with
        | Some AST.TFloat64 ->
            // Float load: load into integer register, then move bits to float register
            let lirFDest = vregToLIRFReg dest
            let tempReg = LIR.Physical LIR.X9  // Use temp register for heap load
            Ok ([LIR.HeapLoad (tempReg, lirAddr, offset)
                 LIR.GpToFp (lirFDest, tempReg)], state)
        | _ ->
            // Integer/other load
            let lirDest = vregToLIRReg dest
            Ok ([LIR.HeapLoad (lirDest, lirAddr, offset)], state)

    | MIR.RefCountInc (addr, payloadSize, kind, sourceType) ->
        let lirAddr = vregToLIRReg addr
        Ok ([LIR.RefCountInc (lirAddr, payloadSize, (match kind with | MIR.GenericHeap -> LIR.GenericHeap | MIR.TaggedList -> LIR.TaggedList | MIR.DictHeap -> LIR.DictHeap | MIR.ClosureHeap -> LIR.ClosureHeap), sourceType)], state)

    | MIR.RefCountDec (addr, payloadSize, kind, sourceType) ->
        let lirAddr = vregToLIRReg addr
        Ok ([LIR.RefCountDec (lirAddr, payloadSize, (match kind with | MIR.GenericHeap -> LIR.GenericHeap | MIR.TaggedList -> LIR.TaggedList | MIR.DictHeap -> LIR.DictHeap | MIR.ClosureHeap -> LIR.ClosureHeap), sourceType)], state)

    | MIR.Print (src, valueType) ->
        // Generate appropriate print instruction based on type
        let finishPrint instrs =
            Ok (instrs @ releasePrintedValue variantRegistry recordRegistry src valueType, state)
        let finishPrintFromReg reg instrs =
            Ok (instrs @ releasePrintedValueFromReg variantRegistry recordRegistry reg valueType, state)
        match valueType with
        | AST.TBool ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            finishPrint (moveToX0 @ [LIR.PrintBool (LIR.Physical LIR.X0)])
        | AST.TDateTime ->
            Error "DateTime values must be rendered through Stdlib.DateTime.toString before print lowering"
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            finishPrint (moveToX0 @ [LIR.PrintInt64 (LIR.Physical LIR.X0)])
        | AST.TUInt64 ->
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            finishPrint (moveToX0 @ [LIR.PrintUInt64 (LIR.Physical LIR.X0)])
        | AST.TFloat64 ->
            // Float needs to be in D0 for printing
            match src with
            | MIR.FloatSymbol value ->
                // Literal float - load into D0
                finishPrint
                    [LIR.FLoad (LIR.FPhysical LIR.D0, value)
                     LIR.PrintFloat (LIR.FPhysical LIR.D0)]
            | MIR.Register vreg ->
                // Computed float - it's in an FVirtual register, move to D0 for printing
                let srcFReg = vregToLIRFReg vreg
                finishPrint
                    [LIR.FMov (LIR.FPhysical LIR.D0, srcFReg)
                     LIR.PrintFloat (LIR.FPhysical LIR.D0)]
            | _ ->
                Error "Internal error: unexpected operand type for float print"
        | AST.TString | AST.TChar | AST.TInt | AST.TInt128 | AST.TUInt128 ->
            // String/Char printing uses PrintString for pool strings, PrintHeapString for heap strings.
            // Char is stored as a string at runtime (single EGC).
            // Int128/UInt128 are lowered as canonical decimal strings.
            match src with
            | MIR.StringSymbol value ->
                finishPrint [LIR.PrintString value]
            | MIR.Register vreg ->
                // Heap string (from concatenation): use PrintHeapString
                let lirReg = vregToLIRReg vreg
                finishPrint [LIR.PrintHeapString lirReg]
            | other ->
                Error $"Print: Unexpected operand type for string: {other}"
        | AST.TEnumFields _ ->
            Crash.crash "TEnumFields is declaration metadata and cannot be printed as a standalone value"

        | AST.TTuple elemTypes ->
            // Tuple printing: (elem1, elem2, ...)
            // Use X19 (callee-saved) to hold tuple address throughout printing
            // since PrintChars clobbers caller-saved registers (X0-X3)
            let tupleAddrReg = LIR.Physical LIR.X19
            let saveTupleAddr =
                let srcOp = convertOperand src
                [LIR.Mov (tupleAddrReg, srcOp)]

            // Generate instructions to print each element
            // Use no-newline versions for tuple elements
            let elemInstrs =
                elemTypes
                |> List.mapi (fun i elemType ->
                    let elemReg = LIR.Physical LIR.X0  // Load directly to X0 for printing
                    let loadInstr = LIR.HeapLoad (elemReg, tupleAddrReg, i * 8)
                    let sepInstrs =
                        if i > 0 then [LIR.PrintChars [byte ','; byte ' ']]  // ", "
                        else []
                    let printInstrs =
                        match elemType with
                        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 ->
                            [LIR.PrintInt64NoNewline (LIR.Physical LIR.X0)]
                        | AST.TUInt64 ->
                            [LIR.PrintUInt64NoNewline (LIR.Physical LIR.X0)]
                        | AST.TBool ->
                            [LIR.PrintBoolNoNewline (LIR.Physical LIR.X0)]
                        | AST.TFloat64 ->
                            // Float is in X0 as raw bits, move to D0 for printing
                            [LIR.GpToFp (LIR.FPhysical LIR.D0, LIR.Physical LIR.X0)
                             LIR.PrintFloatNoNewline (LIR.FPhysical LIR.D0)]
                        | AST.TString | AST.TChar | AST.TInt128 | AST.TUInt128 ->
                            [LIR.PrintHeapStringNoNewline (LIR.Physical LIR.X0)]
                        | AST.TList _ ->
                            Crash.crash $"Unsupported nested list tuple element type for printing: {elemType}"
                        | t ->
                            Crash.crash $"Unsupported tuple element type for printing: {t}"
                    sepInstrs @ [loadInstr] @ printInstrs)
                |> List.concat

            // Combine: save addr + "(" + elements + ")\n"
            let openParen = [LIR.PrintChars [byte '(']]
            let closeParenNewline = [LIR.PrintChars [byte ')'; byte '\n']]
            finishPrintFromReg tupleAddrReg (saveTupleAddr @ openParen @ elemInstrs @ closeParenNewline)

        | AST.TList elemType when elemType = AST.TInt128 || elemType = AST.TUInt128 ->
            Crash.crash $"Unsupported Int128/UInt128 list element type for printing: {elemType}"

        | AST.TList elemType ->
            // Print list as [elem1, elem2, ...]
            let lirSrc = convertOperand src
            let moveToX19 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X19) -> []
                | LIR.Reg r -> [LIR.Mov (LIR.Physical LIR.X19, LIR.Reg r)]
                | other -> [LIR.Mov (LIR.Physical LIR.X19, other)]
            finishPrintFromReg (LIR.Physical LIR.X19) (moveToX19 @ [LIR.PrintList (LIR.Physical LIR.X19, elemType)])

        | AST.TSum (typeName, typeArgs) ->
            // Sum type printing: look up variants and generate PrintSum
            match Map.tryFind typeName variantRegistry with
            | Some typeVariants ->
                // Apply type substitution to payload types
                let substitutedVariants =
                    typeVariants.Variants |> List.map (fun v ->
                        let subPayload =
                            match v.Payload with
                            | Some payload ->
                                Some (applyTypeSubst typeVariants.TypeParams typeArgs payload)
                            | other -> other
                        (v.Name, v.Tag, subPayload))
                // Move sum pointer to X19 (callee-saved for print operations)
                let lirSrc = convertOperand src
                let moveToX19 =
                    match lirSrc with
                    | LIR.Reg (LIR.Physical LIR.X19) -> []
                    | LIR.Reg r -> [LIR.Mov (LIR.Physical LIR.X19, LIR.Reg r)]
                    | other -> [LIR.Mov (LIR.Physical LIR.X19, other)]
                finishPrintFromReg (LIR.Physical LIR.X19) (moveToX19 @ [LIR.PrintSum (LIR.Physical LIR.X19, substitutedVariants)])
            | None ->
                Crash.crash $"Missing sum variant metadata for printing sum type '{typeName}'"

        | AST.TRecord (typeName, typeArgs) ->
            // Print record with field names and values
            match Map.tryFind typeName recordRegistry with
            | Some fields ->
                // Move record address to callee-saved X19 (preserved through syscalls)
                let lirSrc = convertOperand src
                let moveToX19 =
                    match lirSrc with
                    | LIR.Reg (LIR.Physical LIR.X19) -> []
                    | _ -> [LIR.Mov (LIR.Physical LIR.X19, lirSrc)]
                // Convert RecordField list to tuple format for LIR
                let typeParams = inferRecordTypeParamsFromFields fields
                let fieldTuples =
                    fields
                    |> List.map (fun f ->
                        let fieldType =
                            if List.length typeParams = List.length typeArgs then
                                applyTypeSubst typeParams typeArgs f.Type
                            else
                                Crash.crash $"Record print type argument mismatch for '{typeName}': params={typeParams.Length}, args={typeArgs.Length}"
                        (f.Name, fieldType))
                finishPrintFromReg (LIR.Physical LIR.X19) (moveToX19 @ [LIR.PrintRecord (LIR.Physical LIR.X19, typeName, fieldTuples)])
            | None ->
                Error $"Print: Record type '{typeName}' not found in recordRegistry"
        | AST.TDict _ ->
            // Dict: print address for now
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            finishPrint (moveToX0 @ [LIR.PrintInt64 (LIR.Physical LIR.X0)])
        | AST.TUnit ->
            // Unit: print "()" with newline
            finishPrint [LIR.PrintChars [byte '('; byte ')'; byte '\n']]
        | AST.TRuntimeError ->
            // Runtime-error expressions are normalized to Unit before print insertion,
            // but keep this branch explicit for exhaustiveness.
            finishPrint [LIR.PrintChars [byte '('; byte ')'; byte '\n']]
        | AST.TFunction _ ->
            // Functions shouldn't be printed, but just print address
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            finishPrint (moveToX0 @ [LIR.PrintInt64 (LIR.Physical LIR.X0)])
        | AST.TRawPtr ->
            // Raw pointer: print address
            let lirSrc = convertOperand src
            let moveToX0 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X0) -> []
                | _ -> [LIR.Mov (LIR.Physical LIR.X0, lirSrc)]
            finishPrint (moveToX0 @ [LIR.PrintInt64 (LIR.Physical LIR.X0)])
        | AST.TBlob ->
            // Blob: render as the interpreter-compatible ephemeral marker.
            let lirSrc = convertOperand src
            let moveToX19 =
                match lirSrc with
                | LIR.Reg (LIR.Physical LIR.X19) -> []
                | LIR.Reg r -> [LIR.Mov (LIR.Physical LIR.X19, LIR.Reg r)]
                | other -> [LIR.Mov (LIR.Physical LIR.X19, other)]
            finishPrintFromReg (LIR.Physical LIR.X19) (moveToX19 @ [LIR.PrintBlob (LIR.Physical LIR.X19)])
        | AST.TVar _ ->
            // Type variables should be monomorphized away before reaching LIR
            Error "Internal error: Type variable reached MIR_to_LIR (should be monomorphized)"

    | MIR.RuntimeError message ->
        Ok ([LIR.RuntimeError message], state)
    | MIR.RuntimeErrorString message ->
        match convertOperand message with
        | LIR.Reg register -> Ok ([LIR.RuntimeErrorString register], state)
        | _ -> Error "Internal error: dynamic runtime error message must be held in a register"

    | MIR.StringConcat (dest, left, right) ->
        let lirDest = vregToLIRReg dest
        let lirLeft = convertOperand left
        let lirRight = convertOperand right
        Ok ([LIR.StringConcat (lirDest, lirLeft, lirRight)], state)

    | MIR.StdoutWrite (effectId, value, appendNewline) ->
        Ok ([LIR.StdoutWrite (effectId, convertOperand value, appendNewline)], state)

    | MIR.StdinReadLine dest ->
        let (MIR.VReg effectId) = dest
        Ok ([LIR.StdinReadLine (effectId, vregToLIRReg dest)], state)

    | MIR.FileReadText (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok ([LIR.FileReadText (lirDest, lirPath)], state)

    | MIR.FileExists (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok ([LIR.FileExists (lirDest, lirPath)], state)

    | MIR.FileWriteText (dest, path, content) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        let lirContent = convertOperand content
        Ok ([LIR.FileWriteText (lirDest, lirPath, lirContent)], state)

    | MIR.FileAppendText (dest, path, content) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        let lirContent = convertOperand content
        Ok ([LIR.FileAppendText (lirDest, lirPath, lirContent)], state)

    | MIR.FileDelete (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok ([LIR.FileDelete (lirDest, lirPath)], state)

    | MIR.FileSetExecutable (dest, path) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        Ok ([LIR.FileSetExecutable (lirDest, lirPath)], state)

    | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
        let lirDest = vregToLIRReg dest
        let lirPath = convertOperand path
        // ptr and length must be in registers
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister length stateAfterPtr with
        | Error err -> Error err
        | Ok (lengthInstrs, lengthReg, nextState) ->
            Ok (ptrInstrs @ lengthInstrs @ [LIR.FileWriteFromPtr (lirDest, lirPath, ptrReg, lengthReg)], nextState)

    | MIR.RawAlloc (dest, numBytes) ->
        let lirDest = vregToLIRReg dest
        // numBytes must be in a register for LIR
        match ensureInRegister numBytes state with
        | Error err -> Error err
        | Ok (loadInstrs, numBytesReg, nextState) ->
            Ok (loadInstrs @ [LIR.RawAlloc (lirDest, numBytesReg)], nextState)

    | MIR.RawFree ptr ->
        // ptr must be in a register
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (loadInstrs, ptrReg, nextState) ->
            Ok (loadInstrs @ [LIR.RawFree ptrReg], nextState)

    | MIR.RawGet (dest, ptr, byteOffset, valueType) ->
        // Both ptr and byteOffset must be in registers
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister byteOffset stateAfterPtr with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg, nextState) ->
            match valueType with
            | Some AST.TFloat64 ->
                // Float load: load raw bits into GP register, then move to FP register
                let lirFDest = vregToLIRFReg dest
                let tempReg = LIR.Physical LIR.X9  // Use temp register for raw get
                Ok (ptrInstrs @ offsetInstrs @ [LIR.RawGet (tempReg, ptrReg, offsetReg); LIR.GpToFp (lirFDest, tempReg)], nextState)
            | _ ->
                // Integer/other load
                let lirDest = vregToLIRReg dest
                Ok (ptrInstrs @ offsetInstrs @ [LIR.RawGet (lirDest, ptrReg, offsetReg)], nextState)

    | MIR.RawGetByte (dest, ptr, byteOffset) ->
        let lirDest = vregToLIRReg dest
        // Both ptr and byteOffset must be in registers
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister byteOffset stateAfterPtr with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg, nextState) ->
            Ok (ptrInstrs @ offsetInstrs @ [LIR.RawGetByte (lirDest, ptrReg, offsetReg)], nextState)

    | MIR.RawWriteWord (ptr, byteOffset, value) ->
        // All three operands must be in registers
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister byteOffset stateAfterPtr with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg, stateAfterOffset) ->
        match ensureInRegister value stateAfterOffset with
        | Error err -> Error err
        | Ok (valueInstrs, valueReg, nextState) ->
            Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)], nextState)

    | MIR.RawWriteByte (ptr, byteOffset, value) ->
        // All three operands must be in registers
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister byteOffset stateAfterPtr with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg, stateAfterOffset) ->
        match ensureInRegister value stateAfterOffset with
        | Error err -> Error err
        | Ok (valueInstrs, valueReg, nextState) ->
            Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)], nextState)

    | MIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
        // All three operands must be in registers
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister byteOffset stateAfterPtr with
        | Error err -> Error err
        | Ok (offsetInstrs, offsetReg, stateAfterOffset) ->
        // Handle StringSymbol specially - use Mov which CodeGen handles (converts to heap format)
        match value with
        | MIR.StringSymbol _ ->
            let (tempReg, nextState) = freshTempReg stateAfterOffset
            let movInstr = LIR.Mov (tempReg, convertOperand value)
            Ok (ptrInstrs @ offsetInstrs @ [movInstr; LIR.RawSlotInit (ptrReg, offsetReg, tempReg, valueType)], nextState)
        | _ ->
            match valueType with
            | AST.TFloat64 ->
                // Float store: ensure value is in FP register, then convert to GP for storage
                match ensureInFRegister value stateAfterOffset with
                | Error err -> Error err
                | Ok (valueInstrs, valueFReg, nextState) ->
                    let tempReg = LIR.Physical LIR.X9  // Use temp register for FpToGp
                    Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIR.FpToGp (tempReg, valueFReg); LIR.RawSlotInit (ptrReg, offsetReg, tempReg, valueType)], nextState)
            | _ ->
                match ensureInRegister value stateAfterOffset with
                | Error err -> Error err
                | Ok (valueInstrs, valueReg, nextState) ->
                    Ok (ptrInstrs @ offsetInstrs @ valueInstrs @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)], nextState)

    | MIR.StringToRawPtr (dest, value) ->
        Ok ([LIR.Mov (vregToLIRReg dest, convertOperand value)], state)

    | MIR.RawPtrToString (dest, ptr) ->
        Ok ([LIR.Mov (vregToLIRReg dest, convertOperand ptr)], state)

    | MIR.BlobToRawPtr (dest, value) ->
        Ok ([LIR.Mov (vregToLIRReg dest, convertOperand value)], state)

    | MIR.RawPtrToBlob (dest, ptr) ->
        Ok ([LIR.Mov (vregToLIRReg dest, convertOperand ptr)], state)

    | MIR.DictToRawPtr (dest, dict) ->
        match ensureInRegister dict state with
        | Error err -> Error err
        | Ok (dictInstrs, dictReg, stateAfterDict) ->
        match ensureInRegister (MIR.Int64Const -4L) stateAfterDict with
        | Error err -> Error err
        | Ok (maskInstrs, maskReg, nextState) ->
            Ok (dictInstrs @ maskInstrs @ [LIR.And (vregToLIRReg dest, dictReg, maskReg)], nextState)

    | MIR.RawPtrToDict (dest, ptr, tag) ->
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister tag stateAfterPtr with
        | Error err -> Error err
        | Ok (tagInstrs, tagReg, nextState) ->
            Ok (ptrInstrs @ tagInstrs @ [LIR.Orr (vregToLIRReg dest, ptrReg, tagReg)], nextState)

    | MIR.ListToRawPtr (dest, list) ->
        match ensureInRegister list state with
        | Error err -> Error err
        | Ok (listInstrs, listReg, stateAfterList) ->
        match ensureInRegister (MIR.Int64Const -8L) stateAfterList with
        | Error err -> Error err
        | Ok (maskInstrs, maskReg, nextState) ->
            Ok (listInstrs @ maskInstrs @ [LIR.And (vregToLIRReg dest, listReg, maskReg)], nextState)

    | MIR.RawPtrToList (dest, ptr, tag) ->
        match ensureInRegister ptr state with
        | Error err -> Error err
        | Ok (ptrInstrs, ptrReg, stateAfterPtr) ->
        match ensureInRegister tag stateAfterPtr with
        | Error err -> Error err
        | Ok (tagInstrs, tagReg, nextState) ->
            Ok (ptrInstrs @ tagInstrs @ [LIR.Orr (vregToLIRReg dest, ptrReg, tagReg)], nextState)

    | MIR.FloatSqrt (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src state with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg, nextState) ->
            Ok (srcInstrs @ [LIR.FSqrt (lirFDest, srcFReg)], nextState)

    | MIR.FloatAbs (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src state with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg, nextState) ->
            Ok (srcInstrs @ [LIR.FAbs (lirFDest, srcFReg)], nextState)

    | MIR.FloatNeg (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        match ensureInFRegister src state with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg, nextState) ->
            Ok (srcInstrs @ [LIR.FNeg (lirFDest, srcFReg)], nextState)

    | MIR.Int64ToFloat (dest, src) ->
        let lirFDest = vregToLIRFReg dest
        // src is an integer operand that needs to be in an integer register
        match ensureInRegister src state with
        | Error err -> Error err
        | Ok (srcInstrs, srcReg, nextState) ->
            Ok (srcInstrs @ [LIR.Int64ToFloat (lirFDest, srcReg)], nextState)

    | MIR.FloatToInt64 (dest, src) ->
        let lirDest = vregToLIRReg dest
        // src is a float operand that needs to be in a float register
        match ensureInFRegister src state with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg, nextState) ->
            Ok (srcInstrs @ [LIR.FloatToInt64 (lirDest, srcFReg)], nextState)

    | MIR.FloatToBits (dest, src) ->
        let lirDest = vregToLIRReg dest
        // src is a float operand that needs to be in a float register
        match ensureInFRegister src state with
        | Error err -> Error err
        | Ok (srcInstrs, srcFReg, nextState) ->
            // FloatToBits uses FpToGp (bit copy, not conversion)
            Ok (srcInstrs @ [LIR.FloatToBits (lirDest, srcFReg)], nextState)

    | MIR.RefCountIncString str ->
        let lirStr = convertOperand str
        Ok ([LIR.RefCountIncString lirStr], state)

    | MIR.RefCountDecString str ->
        let lirStr = convertOperand str
        Ok ([LIR.RefCountDecString lirStr], state)
    | MIR.RefCountIncBlob bytes ->
        let lirBytes = convertOperand bytes
        Ok ([LIR.RefCountIncBlob lirBytes], state)

    | MIR.RefCountDecBlob bytes ->
        let lirBytes = convertOperand bytes
        Ok ([LIR.RefCountDecBlob lirBytes], state)

    | MIR.RandomInt64 dest ->
        let lirDest = vregToLIRReg dest
        Ok ([LIR.RandomInt64 lirDest], state)

    | MIR.DateTimeNow dest ->
        let lirDest = vregToLIRReg dest
        Ok ([LIR.DateTimeNow lirDest], state)

    | MIR.FloatToString (dest, value) ->
        let lirDest = vregToLIRReg dest
        // Ensure value is in an FP register
        match ensureInFRegister value state with
        | Error err -> Error err
        | Ok (valueInstrs, valueFReg, nextState) ->
            Ok (valueInstrs @ [LIR.FloatToString (lirDest, valueFReg)], nextState)

    | MIR.CoverageHit exprId ->
        Ok ([LIR.CoverageHit exprId], state)

    | MIR.Phi (dest, sources, valueType) ->
        // Convert MIR.Phi to LIR.Phi (int) or LIR.FPhi (float)
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
            | Ok lirSources -> Ok ([LIR.FPhi (lirDest, lirSources)], state)
        else
            // Integer phi uses Reg (Virtual) registers
            let lirDest = vregToLIRReg dest
            let lirSources =
                sources |> List.map (fun (op, MIR.Label lbl) ->
                    (convertOperand op, LIR.Label lbl))
            Ok ([LIR.Phi (lirDest, lirSources, valueType)], state)

/// Convert MIR terminator to LIR terminator
/// For Branch, need to convert operand to register (may add instructions)
/// Printing is now handled by MIR.Print instruction, not in terminator
let selectTerminator
    (terminator: MIR.Terminator)
    (returnType: AST.Type)
    (state: TempState)
    : Result<LIR.Instr list * LIR.Terminator * TempState, string> =
    match terminator with
    | MIR.Ret operand ->
        match operand with
        | MIR.FloatSymbol value ->
            // Load float into D0 for return
            let loadFloat = LIR.FLoad (LIR.FPhysical LIR.D0, value)
            Ok ([loadFloat], LIR.Ret, state)
        | MIR.BoolConst b ->
            // Return bool as 0/1
            let lirOp = LIR.Imm (if b then 1L else 0L)
            let moveToX0 = [LIR.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIR.Ret, state)
        | MIR.StringSymbol _ ->
            // Return string symbol address in X0 so callers can use the value.
            // Top-level printing is handled by MIR.Print, not return lowering.
            let lirOp = convertOperand operand
            let moveToX0 = [LIR.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIR.Ret, state)
        | MIR.Register vreg when returnType = AST.TFloat64 ->
            // Float return - move to D0 via FMov
            let srcFReg = vregToLIRFReg vreg
            let moveToD0 = [LIR.FMov (LIR.FPhysical LIR.D0, srcFReg)]
            Ok (moveToD0, LIR.Ret, state)
        | _ ->
            // Integer/other return - move operand to X0
            let lirOp = convertOperand operand
            let moveToX0 = [LIR.Mov (LIR.Physical LIR.X0, lirOp)]
            Ok (moveToX0, LIR.Ret, state)

    | MIR.Branch (condOp, trueLabel, falseLabel) ->
        // Convert MIR.Label to LIR.Label
        let (MIR.Label trueLbl) = trueLabel
        let (MIR.Label falseLbl) = falseLabel

        // Condition must be in a register for ARM64 branch instructions
        match ensureInRegister condOp state with
        | Error err -> Error err
        | Ok (condInstrs, condReg, nextState) ->
            Ok (condInstrs, LIR.Branch (condReg, LIR.Label trueLbl, LIR.Label falseLbl), nextState)

    | MIR.Jump label ->
        let (MIR.Label lbl) = label
        Ok ([], LIR.Jump (LIR.Label lbl), state)

/// Convert MIR label to LIR label
let convertLabel (MIR.Label lbl) : LIR.Label = LIR.Label lbl

let maxVRegId (MIR.VReg id) (currentMax: int) : int =
    max currentMax id

let maxVRegIdFromOperand (operand: MIR.Operand) (currentMax: int) : int =
    match operand with
    | MIR.Register reg -> maxVRegId reg currentMax
    | _ -> currentMax

let maxVRegIdsFromOperands (operands: MIR.Operand list) (currentMax: int) : int =
    operands
    |> List.fold (fun maxId operand -> maxVRegIdFromOperand operand maxId) currentMax

let maxVRegIdFromInstr (instr: MIR.Instr) (currentMax: int) : int =
    match instr with
    | MIR.Mov (dest, src, _) ->
        currentMax |> maxVRegId dest |> maxVRegIdFromOperand src
    | MIR.BinOp (dest, _, left, right, _) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand left
        |> maxVRegIdFromOperand right
    | MIR.UnaryOp (dest, _, src) ->
        currentMax |> maxVRegId dest |> maxVRegIdFromOperand src
    | MIR.Call (dest, _, args, _, _) ->
        currentMax |> maxVRegId dest |> maxVRegIdsFromOperands args
    | MIR.TailCall (_, args, _, _) ->
        currentMax |> maxVRegIdsFromOperands args
    | MIR.IndirectCall (dest, func, args, _, _) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand func
        |> maxVRegIdsFromOperands args
    | MIR.IndirectTailCall (func, args, _, _) ->
        currentMax |> maxVRegIdFromOperand func |> maxVRegIdsFromOperands args
    | MIR.ClosureAlloc (dest, _, captures) ->
        currentMax |> maxVRegId dest |> maxVRegIdsFromOperands captures
    | MIR.ClosureCall (dest, closure, args, _, _) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand closure
        |> maxVRegIdsFromOperands args
    | MIR.ClosureTailCall (closure, args, _) ->
        currentMax |> maxVRegIdFromOperand closure |> maxVRegIdsFromOperands args
    | MIR.HeapAlloc (dest, _) -> maxVRegId dest currentMax
    | MIR.HeapStore (addr, _, src, _) ->
        currentMax |> maxVRegId addr |> maxVRegIdFromOperand src
    | MIR.HeapLoad (dest, addr, _, _) ->
        currentMax |> maxVRegId dest |> maxVRegId addr
    | MIR.StringConcat (dest, left, right) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand left
        |> maxVRegIdFromOperand right
    | MIR.RefCountInc (addr, _, _, _)
    | MIR.RefCountDec (addr, _, _, _) -> maxVRegId addr currentMax
    | MIR.Print (src, _) -> maxVRegIdFromOperand src currentMax
    | MIR.StdoutWrite (_, src, _) -> maxVRegIdFromOperand src currentMax
    | MIR.StdinReadLine dest -> maxVRegId dest currentMax
    | MIR.RuntimeError _ -> currentMax
    | MIR.RuntimeErrorString message -> maxVRegIdFromOperand message currentMax
    | MIR.FileReadText (dest, path)
    | MIR.FileExists (dest, path)
    | MIR.FileDelete (dest, path)
    | MIR.FileSetExecutable (dest, path) ->
        currentMax |> maxVRegId dest |> maxVRegIdFromOperand path
    | MIR.FileWriteText (dest, path, content)
    | MIR.FileAppendText (dest, path, content) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand path
        |> maxVRegIdFromOperand content
    | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand path
        |> maxVRegIdFromOperand ptr
        |> maxVRegIdFromOperand length
    | MIR.FloatSqrt (dest, src)
    | MIR.FloatAbs (dest, src)
    | MIR.FloatNeg (dest, src)
    | MIR.Int64ToFloat (dest, src)
    | MIR.FloatToInt64 (dest, src)
    | MIR.FloatToBits (dest, src)
    | MIR.RawAlloc (dest, src)
    | MIR.StringToRawPtr (dest, src)
    | MIR.RawPtrToString (dest, src)
    | MIR.BlobToRawPtr (dest, src)
    | MIR.RawPtrToBlob (dest, src)
    | MIR.DictToRawPtr (dest, src)
    | MIR.ListToRawPtr (dest, src)
    | MIR.FloatToString (dest, src) ->
        currentMax |> maxVRegId dest |> maxVRegIdFromOperand src
    | MIR.RawFree ptr
    | MIR.RefCountIncString ptr
    | MIR.RefCountDecString ptr
    | MIR.RefCountIncBlob ptr
    | MIR.RefCountDecBlob ptr -> maxVRegIdFromOperand ptr currentMax
    | MIR.RawGet (dest, ptr, byteOffset, _)
    | MIR.RawGetByte (dest, ptr, byteOffset)
    | MIR.RawPtrToDict (dest, ptr, byteOffset)
    | MIR.RawPtrToList (dest, ptr, byteOffset) ->
        currentMax
        |> maxVRegId dest
        |> maxVRegIdFromOperand ptr
        |> maxVRegIdFromOperand byteOffset
    | MIR.RawWriteWord (ptr, byteOffset, value) ->
        currentMax
        |> maxVRegIdFromOperand ptr
        |> maxVRegIdFromOperand byteOffset
        |> maxVRegIdFromOperand value
    | MIR.RawWriteByte (ptr, byteOffset, value)
    | MIR.RawSlotInit (ptr, byteOffset, value, _) ->
        currentMax
        |> maxVRegIdFromOperand ptr
        |> maxVRegIdFromOperand byteOffset
        |> maxVRegIdFromOperand value
    | MIR.RandomInt64 dest
    | MIR.DateTimeNow dest -> maxVRegId dest currentMax
    | MIR.Phi (dest, sources, _) ->
        sources
        |> List.fold
            (fun maxId (operand, _) -> maxVRegIdFromOperand operand maxId)
            (maxVRegId dest currentMax)
    | MIR.CoverageHit _ -> currentMax

let maxVRegIdFromTerminator (terminator: MIR.Terminator) (currentMax: int) : int =
    match terminator with
    | MIR.Ret operand
    | MIR.Branch (operand, _, _) -> maxVRegIdFromOperand operand currentMax
    | MIR.Jump _ -> currentMax

let initTempState (mirFunc: MIR.Function) : TempState =
    let maxParamRegId =
        mirFunc.TypedParams
        |> List.fold (fun maxId param -> maxVRegId param.Reg maxId) -1
    let maxRegId =
        mirFunc.CFG.Blocks
        |> Map.fold (fun maxId _ block ->
            block.Instrs
            |> List.fold (fun blockMax instr -> maxVRegIdFromInstr instr blockMax) maxId
            |> maxVRegIdFromTerminator block.Terminator) maxParamRegId
    let maxFRegId =
        mirFunc.FloatRegs |> Set.fold max -1
    { NextRegId = maxRegId + 1
      NextFRegId = maxFRegId + 1 }

let moduloNegativeDivisorErrorBlock (label: LIR.Label) : LIR.BasicBlock =
    {
        Label = label
        Instrs = [LIR.PrintString moduloNegativeDivisorErrorMessage]
        Terminator = LIR.Ret
    }

let selectBlocksWithModuloChecks
    (block: MIR.BasicBlock)
    (variantRegistry: MIR.VariantRegistry)
    (recordRegistry: MIR.RecordRegistry)
    (returnType: AST.Type)
    (floatRegs: Set<int>)
    (errorLabel: LIR.Label)
    (state: TempState)
    : Result<LIR.BasicBlock list * LIR.Label * TempState, string> =
    let (MIR.Label baseLabel) = block.Label
    let rec loop instrs counter currentLabel currentInstrsRev blocksRev currentState =
        match instrs with
        | [] -> Ok (blocksRev, counter, currentLabel, currentInstrsRev, currentState)
        | instr :: rest ->
            match instr with
            | MIR.BinOp (dest, MIR.Mod, left, right, operandType) when shouldCheckNegativeDivisor operandType ->
                let lirDest = vregToLIRReg dest
                match buildIntegerModuloParts lirDest left right operandType currentState with
                | Error err -> Error err
                | Ok (loadInstrs, rightReg, modInstrs, nextState) ->
                    let nextLabel = LIR.Label $"{baseLabel}_mod_cont_{counter}"
                    let checkInstrs =
                        List.rev currentInstrsRev
                        @ loadInstrs
                        @ [LIR.Cmp (rightReg, LIR.Imm 0L)]
                    let checkBlock : LIR.BasicBlock =
                        {
                            Label = currentLabel
                            Instrs = checkInstrs
                            Terminator = LIR.CondBranch (LIR.LT, errorLabel, nextLabel)
                        }
                    loop rest (counter + 1) nextLabel (List.rev modInstrs) (checkBlock :: blocksRev) nextState
            | _ ->
                match selectInstr instr variantRegistry recordRegistry floatRegs currentState with
                | Error err -> Error err
                | Ok (lirInstrs, nextState) ->
                    let nextInstrsRev =
                        lirInstrs
                        |> List.fold (fun instrsRev lirInstr -> lirInstr :: instrsRev) currentInstrsRev
                    loop rest counter currentLabel nextInstrsRev blocksRev nextState

    match loop block.Instrs 0 (convertLabel block.Label) [] [] state with
    | Error err -> Error err
    | Ok (blocksRev, _counter, currentLabel, currentInstrsRev, stateAfterInstrs) ->
        match selectTerminator block.Terminator returnType stateAfterInstrs with
        | Error err -> Error err
        | Ok (termInstrs, lirTerm, nextState) ->
            let finalBlock : LIR.BasicBlock =
                {
                    Label = currentLabel
                    Instrs = List.rev currentInstrsRev @ termInstrs
                    Terminator = lirTerm
                }
            Ok (List.rev (finalBlock :: blocksRev), currentLabel, nextState)

/// Convert MIR CFG to LIR CFG
let selectCFG
    (cfg: MIR.CFG)
    (variantRegistry: MIR.VariantRegistry)
    (recordRegistry: MIR.RecordRegistry)
    (returnType: AST.Type)
    (floatRegs: Set<int>)
    (errorLabel: LIR.Label)
    (state: TempState)
    : Result<LIR.CFG, string> =
    let lirEntry = convertLabel cfg.Entry

    match Map.tryFind cfg.Entry cfg.Blocks with
    | None -> Error $"MIR to LIR: missing entry block {cfg.Entry}"
    | Some _ ->
    let blockList = cfg.Blocks |> Map.toList
    let rec buildBlocks remaining currentState blocksAcc labelMapAcc =
        match remaining with
        | [] -> Ok (List.rev blocksAcc |> List.concat, labelMapAcc |> Map.ofList, currentState)
        | (_label, block) :: rest ->
            match selectBlocksWithModuloChecks block variantRegistry recordRegistry returnType floatRegs errorLabel currentState with
            | Error err -> Error err
            | Ok (lirBlocks, finalLabel, nextState) ->
                let originalLabel = convertLabel block.Label
                buildBlocks rest nextState (lirBlocks :: blocksAcc) ((originalLabel, finalLabel) :: labelMapAcc)

    match buildBlocks blockList state [] [] with
    | Error err -> Error err
    | Ok (lirBlocks, labelMap, _finalState) ->
        let needsErrorBlock =
            lirBlocks
            |> List.exists (fun block ->
                match block.Terminator with
                | LIR.Branch (_, trueLabel, falseLabel) ->
                    trueLabel = errorLabel || falseLabel = errorLabel
                | LIR.BranchZero (_, zeroLabel, nonZeroLabel) ->
                    zeroLabel = errorLabel || nonZeroLabel = errorLabel
                | LIR.BranchBitZero (_, _, zeroLabel, nonZeroLabel) ->
                    zeroLabel = errorLabel || nonZeroLabel = errorLabel
                | LIR.BranchBitNonZero (_, _, nonZeroLabel, zeroLabel) ->
                    zeroLabel = errorLabel || nonZeroLabel = errorLabel
                | LIR.CondBranch (_, trueLabel, falseLabel) ->
                    trueLabel = errorLabel || falseLabel = errorLabel
                | LIR.Jump label ->
                    label = errorLabel
                | LIR.Ret -> false)
        let blocksWithError =
            if needsErrorBlock then
                lirBlocks @ [moduloNegativeDivisorErrorBlock errorLabel]
            else
                lirBlocks
        let hasDuplicate =
            blocksWithError
            |> List.countBy (fun block -> block.Label)
            |> List.exists (fun (_, count) -> count > 1)
        if hasDuplicate then
            Error "Internal error: duplicate LIR labels after modulo check insertion"
        else
            let remapLabel (label: LIR.Label) : LIR.Label =
                match Map.tryFind label labelMap with
                | Some mapped -> mapped
                | None -> label

            let remapPhiInstr (instr: LIR.Instr) : LIR.Instr =
                match instr with
                | LIR.Phi (dest, sources, valueType) ->
                    let sources' = sources |> List.map (fun (op, pred) -> (op, remapLabel pred))
                    LIR.Phi (dest, sources', valueType)
                | LIR.FPhi (dest, sources) ->
                    let sources' = sources |> List.map (fun (src, pred) -> (src, remapLabel pred))
                    LIR.FPhi (dest, sources')
                | _ -> instr

            let remappedBlocks =
                blocksWithError
                |> List.map (fun block -> { block with Instrs = block.Instrs |> List.map remapPhiInstr })

            Ok {
                Entry = lirEntry
                Blocks = remappedBlocks |> List.map (fun block -> (block.Label, block)) |> Map.ofList
            }

let private argRegisterCounts (types: AST.Type list) : int * int =
    types
    |> List.fold (fun (intCount, floatCount) typ ->
        if typ = AST.TFloat64 then
            (intCount, floatCount + 1)
        else
            (intCount + 1, floatCount)) (0, 0)

let private validateRegisterBankLimits
    (description: string)
    (intCount: int)
    (floatCount: int)
    : string option =
    if intCount > 8 then
        Some $"{description} has {intCount} integer/pointer arguments, but only 8 are supported (ARM64 calling convention limit)"
    elif floatCount > 8 then
        Some $"{description} has {floatCount} float arguments, but only 8 are supported (ARM64 calling convention limit)"
    else
        None

/// Check if any function exceeds ARM64 parameter register-bank limits.
let private checkParameterLimits (mirFuncs: MIR.Function list) : Result<unit, string> =
    let invalidFunc =
        mirFuncs
        |> List.tryPick (fun f ->
            let types = f.TypedParams |> List.map (fun tp -> tp.Type)
            let (intCount, floatCount) = argRegisterCounts types
            validateRegisterBankLimits $"Function '{f.Name}'" intCount floatCount)
    match invalidFunc with
    | Some err -> Error err
    | None -> Ok ()

/// Check if any function call exceeds ARM64 argument register-bank limits.
let private checkCallArgLimits (mirFuncs: MIR.Function list) : Result<unit, string> =
    let validateCall description argTypes =
        let (intCount, floatCount) = argRegisterCounts argTypes
        validateRegisterBankLimits description intCount floatCount

    let validateClosureCall description argTypes =
        let (intArgCount, floatCount) = argRegisterCounts argTypes
        validateRegisterBankLimits description (intArgCount + 1) floatCount

    let checkBlock (block: MIR.BasicBlock) =
        block.Instrs
        |> List.tryPick (fun instr ->
            match instr with
            | MIR.Call (_, funcName, _, argTypes, _) ->
                validateCall $"Call to '{funcName}'" argTypes
            | MIR.TailCall (funcName, _, argTypes, _) ->
                validateCall $"Tail call to '{funcName}'" argTypes
            | MIR.IndirectCall (_, _, _, argTypes, _) ->
                validateCall "Indirect call" argTypes
            | MIR.IndirectTailCall (_, _, argTypes, _) ->
                validateCall "Indirect tail call" argTypes
            | MIR.ClosureCall (_, _, _, argTypes, _) ->
                validateClosureCall "Closure call" argTypes
            | MIR.ClosureTailCall (_, _, argTypes) ->
                validateClosureCall "Closure tail call" argTypes
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
    let (MIR.Program (mirFuncs, variantRegistry, recordRegistry)) = program

    // Pre-check: verify all functions have ≤8 parameters and calls have ≤8 arguments
    match checkParameterLimits mirFuncs with
    | Error err -> Error err
    | Ok () ->
    match checkCallArgLimits mirFuncs with
    | Error err -> Error err
    | Ok () ->

    // Convert each MIR function to LIR
    let convertFunc (mirFunc: MIR.Function) : Result<LIR.Function, string> =
        let errorLabel = LIR.Label $"__modulo_negative_divisor_error_{mirFunc.Name}"
        let tempState = initTempState mirFunc
        match selectCFG mirFunc.CFG variantRegistry recordRegistry mirFunc.ReturnType mirFunc.FloatRegs errorLabel tempState with
        | Error err -> Error err
        | Ok lirCFG ->
            // Convert MIR TypedParams to LIR TypedLIRParams
            let lirTypedParams : LIR.TypedLIRParam list =
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
        let lirVariantRegistry : LIR.VariantRegistry =
            variantRegistry
            |> Map.map (fun _ typeVariants ->
                let lirVariants : LIR.VariantInfo list =
                    typeVariants.Variants
                    |> List.map (fun variant ->
                        { Name = variant.Name
                          Tag = variant.Tag
                          Payload = variant.Payload })
                { LIR.TypeParams = typeVariants.TypeParams
                  LIR.Variants = lirVariants })
        let lirRecordRegistry =
            recordRegistry
            |> Map.map (fun _ fields ->
                fields |> List.map (fun field -> (field.Name, field.Type)))
        Ok (LIR.Program (lirFuncs, lirVariantRegistry, lirRecordRegistry))
