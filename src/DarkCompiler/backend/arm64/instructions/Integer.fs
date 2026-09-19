// Integer.fs - Emit arm64 instructions for integer operations.

module ARM64EmitInteger

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

let internal emitPhi (ctx: CodeGenContext) : Result<ARM64Symbolic.Instr list, string> =
    // Phi nodes should be eliminated before code generation (by register allocation)
    Error "Phi nodes should be eliminated before code generation"

let internal emitMov (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        match src with
        | LIR.Imm value ->
            Ok (loadImmediate destReg value)
        | LIR.FloatImm _ ->
            Error "Float code generation not yet implemented"
        | LIR.Reg srcReg ->
            lirRegToARM64Reg srcReg
            |> Result.map (fun srcARM64 ->
                // Skip self-moves (can happen after register allocation coalesces VRegs)
                if destReg = srcARM64 then []
                else [ARM64Symbolic.MOV_reg (destReg, srcARM64)])
        | LIR.StackSlot offset ->
            // Load from stack slot into destination register
            loadStackSlot destReg offset
        | LIR.StringSymbol value ->
            Ok (loadStringLiteralPointer destReg value)
        | LIR.FloatSymbol _ ->
            Error "Cannot MOV float reference - use FLoad instruction"
        | LIR.FuncAddr funcName ->
            // Load function address using ADR instruction
            Ok [ARM64Symbolic.ADR (destReg, codeLabel (functionName ctx funcName))])

let internal emitStore (ctx: CodeGenContext) (offset: int) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Store register to stack slot
    lirRegToARM64Reg src
    |> Result.bind (fun srcReg -> storeStackSlot srcReg offset)

let internal emitAdd (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->
                // Can use immediate ADD
                Ok [ARM64Symbolic.ADD_imm (destReg, leftReg, uint16 value)]
            | LIR.Imm value ->
                // Need to load immediate into register first
                let tempReg = ARM64Symbolic.X9  // Use X9 as temp
                Ok (loadImmediate tempReg value @ [ARM64Symbolic.ADD_reg (destReg, leftReg, tempReg)])
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [ARM64Symbolic.ADD_reg (destReg, leftReg, rightARM64)])
            | LIR.StackSlot offset ->
                // Load stack slot into temp register, then add
                let tempReg = ARM64Symbolic.X9
                loadStackSlot tempReg offset
                |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64Symbolic.ADD_reg (destReg, leftReg, tempReg)])
            | LIR.StringSymbol _ ->
                Error "Cannot use string reference in arithmetic operation"
            | LIR.FloatSymbol _ ->
                Error "Cannot use float reference in integer arithmetic"
            | LIR.FuncAddr _ ->
                Error "Cannot use function address in arithmetic operation"))

let internal emitSub (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->
                Ok [ARM64Symbolic.SUB_imm (destReg, leftReg, uint16 value)]
            | LIR.Imm value ->
                let tempReg = ARM64Symbolic.X9
                Ok (loadImmediate tempReg value @ [ARM64Symbolic.SUB_reg (destReg, leftReg, tempReg)])
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [ARM64Symbolic.SUB_reg (destReg, leftReg, rightARM64)])
            | LIR.StackSlot offset ->
                // Load stack slot into temp register, then subtract
                let tempReg = ARM64Symbolic.X9
                loadStackSlot tempReg offset
                |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64Symbolic.SUB_reg (destReg, leftReg, tempReg)])
            | LIR.StringSymbol _ ->
                Error "Cannot use string reference in arithmetic operation"
            | LIR.FloatSymbol _ ->
                Error "Cannot use float reference in integer arithmetic"
            | LIR.FuncAddr _ ->
                Error "Cannot use function address in arithmetic operation"))

let internal emitMul (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.MUL (destReg, leftReg, rightReg)])))

let internal emitSdiv (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.SDIV (destReg, leftReg, rightReg)])))

let internal emitUdiv (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.UDIV (destReg, leftReg, rightReg)])))

let internal emitMsub (ctx: CodeGenContext) (dest: LIR.Reg) (mulLeft: LIR.Reg) (mulRight: LIR.Reg) (sub: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // MSUB: dest = sub - mulLeft * mulRight
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg mulLeft
        |> Result.bind (fun mulLeftReg ->
            lirRegToARM64Reg mulRight
            |> Result.bind (fun mulRightReg ->
                lirRegToARM64Reg sub
                |> Result.map (fun subReg ->
                    [ARM64Symbolic.MSUB (destReg, mulLeftReg, mulRightReg, subReg)]))))

let internal emitMadd (ctx: CodeGenContext) (dest: LIR.Reg) (mulLeft: LIR.Reg) (mulRight: LIR.Reg) (add: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // MADD: dest = add + mulLeft * mulRight
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg mulLeft
        |> Result.bind (fun mulLeftReg ->
            lirRegToARM64Reg mulRight
            |> Result.bind (fun mulRightReg ->
                lirRegToARM64Reg add
                |> Result.map (fun addReg ->
                    [ARM64Symbolic.MADD (destReg, mulLeftReg, mulRightReg, addReg)]))))

let internal emitCmp (ctx: CodeGenContext) (left: LIR.Reg) (right: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg left
    |> Result.bind (fun leftReg ->
        match right with
        | LIR.Imm value when value >= 0L && value < 4096L ->
            Ok [ARM64Symbolic.CMP_imm (leftReg, uint16 value)]
        | LIR.Imm value ->
            let tempReg = ARM64Symbolic.X9
            Ok (loadImmediate tempReg value @ [ARM64Symbolic.CMP_reg (leftReg, tempReg)])
        | LIR.FloatImm _ ->
            Error "Float code generation not yet implemented"
        | LIR.Reg rightReg ->
            lirRegToARM64Reg rightReg
            |> Result.map (fun rightARM64 -> [ARM64Symbolic.CMP_reg (leftReg, rightARM64)])
        | LIR.StackSlot offset ->
            // Load stack slot into temp register, then compare
            let tempReg = ARM64Symbolic.X9
            loadStackSlot tempReg offset
            |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64Symbolic.CMP_reg (leftReg, tempReg)])
        | LIR.StringSymbol _ ->
            Error "Cannot compare string references directly"
        | LIR.FloatSymbol _ ->
            Error "Cannot compare float references directly - use FCmp"
        | LIR.FuncAddr _ ->
            Error "Cannot compare function addresses directly")

let internal emitCset (ctx: CodeGenContext) (dest: LIR.Reg) (cond: LIR.Condition) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        let arm64Cond =
            match cond with
            | LIR.EQ -> ARM64Symbolic.EQ
            | LIR.NE -> ARM64Symbolic.NE
            | LIR.LT -> ARM64Symbolic.LT
            | LIR.GT -> ARM64Symbolic.GT
            | LIR.LE -> ARM64Symbolic.LE
            | LIR.GE -> ARM64Symbolic.GE
            | LIR.ULT -> ARM64Symbolic.LO
            | LIR.UGT -> ARM64Symbolic.HI
            | LIR.ULE -> ARM64Symbolic.LS
            | LIR.UGE -> ARM64Symbolic.HS
        [ARM64Symbolic.CSET (destReg, arm64Cond)])

let internal emitSelect
    (ctx: CodeGenContext)
    (dest: LIR.Reg)
    (whenTrue: LIR.Reg)
    (whenFalse: LIR.Reg)
    (cond: LIR.Condition)
    : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg whenTrue
        |> Result.bind (fun trueReg ->
            lirRegToARM64Reg whenFalse
            |> Result.map (fun falseReg ->
                let arm64Cond =
                    match cond with
                    | LIR.EQ -> ARM64Symbolic.EQ | LIR.NE -> ARM64Symbolic.NE
                    | LIR.LT -> ARM64Symbolic.LT | LIR.GT -> ARM64Symbolic.GT
                    | LIR.LE -> ARM64Symbolic.LE | LIR.GE -> ARM64Symbolic.GE
                    | LIR.ULT -> ARM64Symbolic.LO | LIR.UGT -> ARM64Symbolic.HI
                    | LIR.ULE -> ARM64Symbolic.LS | LIR.UGE -> ARM64Symbolic.HS
                [ARM64Symbolic.CSEL (destReg, trueReg, falseReg, arm64Cond)])))

let internal emitAnd (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.AND_reg (destReg, leftReg, rightReg)])))

let internal emitAnd_imm (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (imm: int64) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.AND_imm (destReg, srcReg, uint64 imm)]))

let internal emitOrr (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.ORR_reg (destReg, leftReg, rightReg)])))

let internal emitEor (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [ARM64Symbolic.EOR_reg (destReg, leftReg, rightReg)])))

let internal emitLsl (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (shift: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.bind (fun srcReg ->
            lirRegToARM64Reg shift
            |> Result.map (fun shiftReg -> [ARM64Symbolic.LSL_reg (destReg, srcReg, shiftReg)])))

let internal emitLsr (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (shift: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.bind (fun srcReg ->
            lirRegToARM64Reg shift
            |> Result.map (fun shiftReg -> [ARM64Symbolic.LSR_reg (destReg, srcReg, shiftReg)])))

let internal emitAsr (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (shift: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.bind (fun srcReg ->
            lirRegToARM64Reg shift
            |> Result.map (fun shiftReg -> [ARM64Symbolic.ASR_reg (destReg, srcReg, shiftReg)])))

let internal emitLsl_imm (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (shift: int) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.LSL_imm (destReg, srcReg, shift)]))

let internal emitLsr_imm (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (shift: int) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.LSR_imm (destReg, srcReg, shift)]))

let internal emitAsr_imm (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) (shift: int) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.ASR_imm (destReg, srcReg, shift)]))

let internal emitNeg (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.NEG (destReg, srcReg)]))

let internal emitMvn (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.MVN (destReg, srcReg)]))

// Sign/zero extension for integer overflow truncation

let internal emitSxtb (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.SXTB (destReg, srcReg)]))

let internal emitSxth (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.SXTH (destReg, srcReg)]))

let internal emitSxtw (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.SXTW (destReg, srcReg)]))

let internal emitUxtb (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.UXTB (destReg, srcReg)]))

let internal emitUxth (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.UXTH (destReg, srcReg)]))

let internal emitUxtw (ctx: CodeGenContext) (dest: LIR.Reg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.UXTW (destReg, srcReg)]))

let internal emitClosureAlloc (ctx: CodeGenContext) (dest: LIR.Reg) (funcId: AST.FunctionId) (captures: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    let funcName = functionName ctx funcId
    // Allocate closure on heap: (func_ptr, cap1, cap2, ...)
    // Each slot is 8 bytes
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        let numSlots = 1 + List.length captures  // func_ptr + captures
        let sizeBytes = numSlots * 8
        // Total size includes 8 bytes for ref count, aligned to 8 bytes
        let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)

        // Allocate using bump allocator
        let allocInstrs = [
            ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)                      // dest = current heap pointer
            ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)                          // X15 = 1 (initial ref count)
            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X28, int16 sizeBytes)       // store ref count after payload
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, uint16 totalSize)  // bump pointer
        ]

        // Store function address at offset 0
        let storeFuncAddr = [
            ARM64Symbolic.ADR (ARM64Symbolic.X15, codeLabel funcName)               // X15 = function address
            ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, 0s)                      // [dest] = func_ptr
        ]

        // Store captures at subsequent offsets
        let storeCaptures =
            captures
            |> List.mapi (fun i cap -> (i, cap))
            |> List.collect (fun (i, cap) ->
                let offset = (i + 1) * 8
                match cap with
                | LIR.Imm value ->
                    loadImmediate ARM64Symbolic.X15 value @
                    [ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, int16 offset)]
                | LIR.Reg reg ->
                    match lirRegToARM64Reg reg with
                    | Ok srcReg ->
                        // Avoid storing dest into itself at offset
                        if srcReg = destReg then
                            [ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, srcReg); ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, int16 offset)]
                        else
                            [ARM64Symbolic.STR (srcReg, destReg, int16 offset)]
                    | Error msg -> Crash.crash $"ClosureAlloc: lirRegToARM64Reg failed: {msg}"
                | LIR.FuncAddr fname ->
                    [ARM64Symbolic.ADR (ARM64Symbolic.X15, codeLabel (functionName ctx fname)); ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, int16 offset)]
                | other -> Crash.crash $"ClosureAlloc: Unexpected capture operand type: {other}")

        Ok (allocInstrs @ generateLeakCounterInc ctx @ storeFuncAddr @ storeCaptures))

let internal emitArgMoves (ctx: CodeGenContext) (moves: (LIR.PhysReg * LIR.Operand) list) : Result<ARM64Symbolic.Instr list, string> =
    // Parallel move resolution for function arguments
    // After SaveRegs, X1-X10 are saved at [SP+0..SP+72]
    // If source is in X1-X7 and could be clobbered, load from stack instead
    //
    // Stack layout after SaveRegs: X1@[SP+0], X2@[SP+8], ..., X10@[SP+72]
    let saveRegsOffset (reg: LIR.PhysReg) : int option =
        match reg with
        | LIR.X1 -> Some 0
        | LIR.X2 -> Some 8
        | LIR.X3 -> Some 16
        | LIR.X4 -> Some 24
        | LIR.X5 -> Some 32
        | LIR.X6 -> Some 40
        | LIR.X7 -> Some 48
        | LIR.X8 -> Some 56
        | LIR.X9 -> Some 64
        | LIR.X10 -> Some 72
        | _ -> None

    // Find which destination registers (X0-X7) will be written
    let destRegs = moves |> List.map fst |> Set.ofList

    // For each move, determine how to execute it safely
    let generateMove (destReg: LIR.PhysReg, srcOp: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
        let destARM64 = lirPhysRegToARM64Reg destReg
        match srcOp with
        | LIR.Imm value ->
            Ok (loadImmediate destARM64 value)
        | LIR.Reg (LIR.Physical srcPhysReg) ->
            // If source equals destination, it's a no-op
            if srcPhysReg = destReg then
                Ok []
            else
                // Check if source register will be clobbered by an earlier move
                // A register is clobbered if it's a destination of a move to a LOWER index
                // (since we process X0, X1, X2, ... in order)
                let srcWillBeClobbered =
                    match srcPhysReg with
                    | LIR.X1 | LIR.X2 | LIR.X3 | LIR.X4 | LIR.X5 | LIR.X6 | LIR.X7 ->
                        Set.contains srcPhysReg destRegs
                    | _ -> false
                if srcWillBeClobbered then
                    // Load from SaveRegs stack instead of live register
                    match saveRegsOffset srcPhysReg with
                    | Some offset ->
                        Ok [ARM64Symbolic.LDR (destARM64, ARM64Symbolic.SP, int16 offset)]
                    | None ->
                        Error $"ArgMoves: Source register {srcPhysReg} will be clobbered but has no SaveRegs offset"
                else
                    let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
                    Ok [ARM64Symbolic.MOV_reg (destARM64, srcARM64)]
        | LIR.Reg (LIR.Virtual _) ->
            Error "Virtual register in ArgMoves - should have been allocated"
        | LIR.StackSlot offset ->
            loadStackSlot destARM64 offset
        | LIR.StringSymbol value ->
            Ok (loadStringLiteralPointer destARM64 value)
        | LIR.FuncAddr funcName ->
            Ok [ARM64Symbolic.ADR (destARM64, codeLabel (functionName ctx funcName))]
        | LIR.FloatImm _ | LIR.FloatSymbol _ ->
            Error "Float in ArgMoves not yet supported"

    // MIR lowering stores moves in ABI destination order (X0, X1, ...),
    // and register allocation changes only their source operands.
    let moveInstrs =
        moves
        |> ResultList.mapResults generateMove
        |> Result.map List.concat

    moveInstrs

let internal emitTailArgMoves (ctx: CodeGenContext) (moves: (LIR.PhysReg * LIR.Operand) list) : Result<ARM64Symbolic.Instr list, string> =
    // Parallel move resolution for TAIL CALL arguments
    // Unlike ArgMoves, there is NO SaveRegs, so we can't load from stack.
    // We use the shared ParallelMoves module with X16 as the temp register.

    // Helper to get source register if operand is a physical register
    let getSrcPhysReg (srcOp: LIR.Operand) : LIR.PhysReg option =
        match srcOp with
        | LIR.Reg (LIR.Physical srcPhysReg) -> Some srcPhysReg
        | _ -> None

    // Generate a single move instruction (for non-register sources)
    let generateMoveInstr (destReg: LIR.PhysReg, srcOp: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
        let destARM64 = lirPhysRegToARM64Reg destReg
        match srcOp with
        | LIR.Imm value ->
            Ok (loadImmediate destARM64 value)
        | LIR.Reg (LIR.Physical srcPhysReg) ->
            let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
            Ok [ARM64Symbolic.MOV_reg (destARM64, srcARM64)]
        | LIR.Reg (LIR.Virtual _) ->
            Error "Virtual register in TailArgMoves - should have been allocated"
        | LIR.StackSlot offset ->
            loadStackSlot destARM64 offset
        | LIR.FuncAddr funcName ->
            Ok [ARM64Symbolic.ADR (destARM64, codeLabel (functionName ctx funcName))]
        | LIR.StringSymbol value ->
            Ok (loadStringLiteralPointer destARM64 value)
        | LIR.FloatImm _ | LIR.FloatSymbol _ ->
            Error "Float in TailArgMoves not yet supported"

    // Use the shared parallel move resolution algorithm
    let actions = ParallelMoves.resolve moves getSrcPhysReg

    // Convert actions to ARM64 instructions
    actions
    |> ResultList.mapResults (function
        | ParallelMoves.SaveToTemp reg ->
            // Save register to X16 (temp)
            Ok [ARM64Symbolic.MOV_reg (ARM64Symbolic.X16, lirPhysRegToARM64Reg reg)]
        | ParallelMoves.Move (dest, src) ->
            generateMoveInstr (dest, src)
        | ParallelMoves.MoveFromTemp dest ->
            // Move from X16 (temp) to destination
            Ok [ARM64Symbolic.MOV_reg (lirPhysRegToARM64Reg dest, ARM64Symbolic.X16)])
    |> Result.map List.concat

let internal emitExit (ctx: CodeGenContext) : Result<ARM64Symbolic.Instr list, string> =
    // Exit program with code 0
    Ok (runtimeInstrs (ARM64PrintAndExit.generateExit ctx.Target))

let internal emitStdoutWrite (ctx: CodeGenContext) (effectId: int) (value: LIR.Operand) (appendNewline: bool) : Result<ARM64Symbolic.Instr list, string> =
    let syscalls = ARM64.targetSyscalls ctx.Target
    let label suffix = $"__presentation_{ctx.FunctionName}_{effectId}_{ctx.InstructionSite}_{suffix}"
    let writeLoop prefix =
        let loopLabel = label $"{prefix}_write"
        let retryLabel = label $"{prefix}_retry"
        let errorLabel = label $"{prefix}_error"
        let doneLabel = label $"{prefix}_done"
        let resultCheck =
            match ARM64.targetOS ctx.Target with
            | Platform.MacOS ->
                [ ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, errorLabel)
                  ARM64Symbolic.B_label $"{prefix}_success"
                  ARM64Symbolic.Label errorLabel
                  ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 4us)
                  ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, retryLabel)
                  ARM64Symbolic.B_label doneLabel
                  ARM64Symbolic.Label $"{prefix}_success" ]
            | Platform.Linux ->
                loadImmediate ARM64Symbolic.X12 -4L
                @ [ ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X12)
                    ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, retryLabel)
                    ARM64Symbolic.TBNZ_label (ARM64Symbolic.X0, 63, doneLabel) ]
        [ ARM64Symbolic.Label loopLabel
          ARM64Symbolic.CBZ (ARM64Symbolic.X2, doneLabel)
          ARM64Symbolic.Label retryLabel
          ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Write, 0)
          ARM64Symbolic.SVC syscalls.SvcImmediate ]
        @ resultCheck
        @ [ ARM64Symbolic.CBZ (ARM64Symbolic.X0, doneLabel)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X1, ARM64Symbolic.X1, ARM64Symbolic.X0)
            ARM64Symbolic.SUB_reg (ARM64Symbolic.X2, ARM64Symbolic.X2, ARM64Symbolic.X0)
            ARM64Symbolic.B_label loopLabel
            ARM64Symbolic.Label doneLabel ]

    let setupValue =
        match value with
        | LIR.Reg reg ->
            lirRegToARM64Reg reg
            |> Result.map (fun src ->
                [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, src)
                  ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X9, 8s)
                  ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X9, 16us) ])
        | LIR.StackSlot offset ->
            loadStackSlot ARM64Symbolic.X9 offset
            |> Result.map (fun load ->
                load
                @ [ ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X9, 8s)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X9, 16us) ])
        | LIR.StringSymbol text ->
            Ok (loadStringLiteralPointer ARM64Symbolic.X9 text
                @ [ ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X9, 8s)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X9, 16us) ])
        | _ -> Error "StdoutWrite requires a String operand"

    setupValue
    |> Result.map (fun setup ->
        let savedRegs =
            [ ARM64Symbolic.X0; ARM64Symbolic.X1; ARM64Symbolic.X2; ARM64Symbolic.X3
              ARM64Symbolic.X4; ARM64Symbolic.X5; ARM64Symbolic.X6; ARM64Symbolic.X7
              ARM64Symbolic.X8; ARM64Symbolic.X9; ARM64Symbolic.X10; ARM64Symbolic.X11
              ARM64Symbolic.X12; ARM64Symbolic.X13; ARM64Symbolic.X14; ARM64Symbolic.X15
              ARM64Symbolic.X16; ARM64Symbolic.X17 ]
        let save =
            [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 160us) ]
            @ (savedRegs |> List.mapi (fun i reg -> ARM64Symbolic.STR (reg, ARM64Symbolic.SP, int16 (i * 8))))
        let restore =
            (savedRegs |> List.mapi (fun i reg -> ARM64Symbolic.LDR (reg, ARM64Symbolic.SP, int16 (i * 8))))
            @ [ ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 160us) ]
        let newline =
            if not appendNewline then []
            else
                [ ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 10us, 0)
                  ARM64Symbolic.STRB (ARM64Symbolic.X9, ARM64Symbolic.SP, 144)
                  ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
                  ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.SP, 144us)
                  ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0) ]
                @ writeLoop "stdout_newline"
        save
        @ setup
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0) ]
        @ writeLoop "stdout"
        @ newline
        @ restore)

let internal emitStdinReadLine (ctx: CodeGenContext) (effectId: int) (dest: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        let syscalls = ARM64.targetSyscalls ctx.Target
        let label suffix = $"__presentation_{ctx.FunctionName}_{effectId}_{ctx.InstructionSite}_{suffix}"
        let readLabel = label "stdin_read"
        let retryLabel = label "stdin_retry"
        let gotByteLabel = label "stdin_byte"
        let finishLabel = label "stdin_finish"
        let noCrLabel = label "stdin_no_cr"
        let readResultCheck =
            match ARM64.targetOS ctx.Target with
            | Platform.MacOS ->
                let errorLabel = label "stdin_error"
                [ ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, errorLabel)
                  ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 1us)
                  ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, gotByteLabel)
                  ARM64Symbolic.B_label finishLabel
                  ARM64Symbolic.Label errorLabel
                  ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 4us)
                  ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, retryLabel)
                  ARM64Symbolic.B_label finishLabel ]
            | Platform.Linux ->
                loadImmediate ARM64Symbolic.X12 -4L
                @ [ ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X12)
                    ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, retryLabel)
                    ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 1us)
                    ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, gotByteLabel)
                    ARM64Symbolic.B_label finishLabel ]
        let savedRegs =
            [ ARM64Symbolic.X0; ARM64Symbolic.X1; ARM64Symbolic.X2; ARM64Symbolic.X3
              ARM64Symbolic.X4; ARM64Symbolic.X5; ARM64Symbolic.X6; ARM64Symbolic.X7
              ARM64Symbolic.X8; ARM64Symbolic.X9; ARM64Symbolic.X10; ARM64Symbolic.X11
              ARM64Symbolic.X12; ARM64Symbolic.X13; ARM64Symbolic.X14; ARM64Symbolic.X15
              ARM64Symbolic.X16; ARM64Symbolic.X17 ]
        [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 160us) ]
        @ (savedRegs |> List.mapi (fun i reg -> ARM64Symbolic.STR (reg, ARM64Symbolic.SP, int16 (i * 8))))
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.SP, 144s)
            ARM64Symbolic.Label readLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.SP, 144s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X28, 16us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X1, ARM64Symbolic.X1, ARM64Symbolic.X10)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
            ARM64Symbolic.Label retryLabel
            ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Read, 0)
            ARM64Symbolic.SVC syscalls.SvcImmediate ]
        @ readResultCheck
        @ [ ARM64Symbolic.Label gotByteLabel
            ARM64Symbolic.LDRB_imm (ARM64Symbolic.X11, ARM64Symbolic.X1, 0)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X11, 10us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, finishLabel)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.SP, 144s)
            ARM64Symbolic.B_label readLabel
            ARM64Symbolic.Label finishLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.SP, 144s)
            ARM64Symbolic.CBZ (ARM64Symbolic.X10, noCrLabel)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X28, 15us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X1, ARM64Symbolic.X1, ARM64Symbolic.X10)
            ARM64Symbolic.LDRB_imm (ARM64Symbolic.X11, ARM64Symbolic.X1, 0)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X11, 13us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, noCrLabel)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
            ARM64Symbolic.Label noCrLabel
            ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 1us, 0)
            ARM64Symbolic.STR (ARM64Symbolic.X0, ARM64Symbolic.X28, 0s)
            ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X28, 8s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X10, 7us)
            ARM64Symbolic.LSR_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 3)
            ARM64Symbolic.LSL_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 3)
            ARM64Symbolic.STR (ARM64Symbolic.X28, ARM64Symbolic.SP, 152s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 16us)
            ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X11) ]
        @ generateLeakCounterInc ctx
        @ (savedRegs |> List.mapi (fun i reg -> ARM64Symbolic.LDR (reg, ARM64Symbolic.SP, int16 (i * 8))))
        @ [ ARM64Symbolic.LDR (destReg, ARM64Symbolic.SP, 152s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 160us) ])

let internal emitRuntimeError (ctx: CodeGenContext) (message: string) : Result<ARM64Symbolic.Instr list, string> =
    Ok (
        loadStringLiteralPointer ARM64Symbolic.X0 message
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 0us, 0)
            ARM64Symbolic.B_label runtimeErrorHelperLabel ])

let internal emitRuntimeErrorString (ctx: CodeGenContext) (messageReg: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg messageReg
    |> Result.map (fun resolvedMessageReg ->
        (if resolvedMessageReg = ARM64Symbolic.X0 then
             []
         else
             [ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, resolvedMessageReg)])
        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 1us, 0)
            ARM64Symbolic.B_label runtimeErrorHelperLabel ])

// Floating-point instructions

let internal emitInt64ToFloat (ctx: CodeGenContext) (dest: LIR.FReg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.SCVTF (destReg, srcReg)]))

let internal emitGpToFp (ctx: CodeGenContext) (dest: LIR.FReg) (src: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Move bits from GP register to FP register (for floats loaded from heap)
    lirFRegToARM64FReg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [ARM64Symbolic.FMOV_from_gp (destReg, srcReg)]))
