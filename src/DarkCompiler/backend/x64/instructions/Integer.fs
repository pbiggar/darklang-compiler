// Integer.fs - Emit x64 instructions for integer operations.

module X64EmitInteger

open X64Operands
open X64CodeGenTypes
open X64FieldReferenceCounts
open X64InstructionContext

let internal emitMov (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Operand) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        match src with
        | LIR.Imm value ->
            Ok (loadImm64 destReg value)
        | LIR.Reg srcReg ->
            resolveReg srcReg
            |> Result.map (fun srcX86 ->
                if destReg = srcX86 then []
                else [X86_64.MOV_reg (destReg, srcX86)])
        | LIR.StackSlot offset ->
            Ok [X86_64.MOV_load (destReg, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
        | LIR.StringSymbol value ->
            Ok (emitStringLiteral destReg value)
        | LIR.FuncAddr funcName ->
            Ok [X86_64.LEA_rip (destReg, funcName)]
        | LIR.FloatImm value | LIR.FloatSymbol value ->
            // Store float bits in GP register
            let bits = System.BitConverter.DoubleToInt64Bits(value)
            Ok (loadImm64 destReg bits))

let internal emitStore (ctx: FuncCtx) (stackSlot: int) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    // Stack slots are byte offsets from FP, adjusted past callee-saved pushes
    resolveReg src
    |> Result.map (fun srcReg ->
        [X86_64.MOV_store (X86_64.RBP, int32 (adjustStackOffset ctx stackSlot), srcReg)])

let internal emitAdd
    (ctx: FuncCtx)
    (comparisonContext: ComparisonContext option)
    (dest: LIR.Reg)
    (left: LIR.Reg)
    (right: LIR.Operand)
    : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue ->
                if Option.isNone comparisonContext && destReg <> leftReg then
                    Ok [X86_64.LEA (destReg, leftReg, int32 value)]
                else
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ [X86_64.ADD_imm (destReg, int32 value)])
            | LIR.Imm value ->
                if destReg = scratch then
                    // dest is R11: can't use scratch for imm. Use PUSH/POP RCX.
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ loadImm64 X86_64.RCX value
                        @ [X86_64.ADD_reg (destReg, X86_64.RCX)
                           X86_64.POP X86_64.RCX])
                else
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ loadImm64 scratch value @ [X86_64.ADD_reg (destReg, scratch)])
            | LIR.Reg rightReg ->
                resolveReg rightReg
                |> Result.map (fun rightX86 ->
                    if destReg = rightX86 && destReg <> leftReg then
                        // dest is right operand: ADD is commutative, so just swap
                        [X86_64.ADD_reg (destReg, leftReg)]
                    elif Option.isNone comparisonContext
                         && destReg <> leftReg
                         && destReg <> rightX86
                         && rightX86 <> X86_64.RSP
                         && rightX86 <> X86_64.R12 then
                        [X86_64.LEA_index (destReg, leftReg, rightX86, 1, 0)]
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        setup @ [X86_64.ADD_reg (destReg, rightX86)])
            | LIR.StackSlot offset ->
                let adjOff = int32 (adjustStackOffset ctx offset)
                if destReg = scratch && leftReg = scratch then
                    // Both dest and left are R11: use PUSH/POP to avoid clobbering
                    Ok ([X86_64.PUSH X86_64.RCX
                         X86_64.MOV_load (X86_64.RCX, X86_64.RBP, adjOff)
                         X86_64.ADD_reg (destReg, X86_64.RCX)
                         X86_64.POP X86_64.RCX])
                else
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ [X86_64.ADD_load (destReg, X86_64.RBP, adjOff)])
            | _ -> Error $"Unsupported Add right operand: {right}"))

let internal emitSub (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Operand) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue ->
                let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                Ok (setup @ [X86_64.SUB_imm (destReg, int32 value)])
            | LIR.Imm value ->
                if destReg = scratch then
                    Ok ([X86_64.PUSH X86_64.RCX]
                        @ loadImm64 X86_64.RCX value
                        @ [X86_64.SUB_reg (destReg, X86_64.RCX)
                           X86_64.POP X86_64.RCX])
                else
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ loadImm64 scratch value @ [X86_64.SUB_reg (destReg, scratch)])
            | LIR.Reg rightReg ->
                resolveReg rightReg
                |> Result.map (fun rightX86 ->
                    if destReg = rightX86 && destReg <> leftReg then
                        if destReg = scratch then
                            // dest=right=R11, left is different: use PUSH/POP
                            [X86_64.PUSH X86_64.RCX
                             X86_64.MOV_reg (X86_64.RCX, leftReg)
                             X86_64.SUB_reg (X86_64.RCX, rightX86)
                             X86_64.MOV_reg (destReg, X86_64.RCX)
                             X86_64.POP X86_64.RCX]
                        else
                            [X86_64.MOV_reg (scratch, leftReg)
                             X86_64.SUB_reg (scratch, rightX86)
                             X86_64.MOV_reg (destReg, scratch)]
                    else
                        let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                        setup @ [X86_64.SUB_reg (destReg, rightX86)])
            | LIR.StackSlot offset ->
                let adjOff = int32 (adjustStackOffset ctx offset)
                if destReg = scratch && leftReg = scratch then
                    Ok ([X86_64.PUSH X86_64.RCX
                         X86_64.MOV_load (X86_64.RCX, X86_64.RBP, adjOff)
                         X86_64.SUB_reg (destReg, X86_64.RCX)
                         X86_64.POP X86_64.RCX])
                else
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    Ok (setup @ [X86_64.SUB_load (destReg, X86_64.RBP, adjOff)])
            | _ -> Error $"Unsupported Sub right operand: {right}"))

let internal emitMul (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<X86_64.Instr list, string> =
    // x86_64 IMUL r64, r/m64 — dest = dest * src
    // Must handle case where dest == right (would clobber right when setting up left)
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg left
        |> Result.bind (fun leftReg ->
            resolveReg right
            |> Result.map (fun rightReg ->
                if destReg = rightReg && destReg <> leftReg then
                    if destReg = scratch then
                        // dest=right=R11, left different: MUL is commutative, swap
                        [X86_64.IMUL_reg (destReg, leftReg)]
                    else
                        [X86_64.MOV_reg (scratch, leftReg)
                         X86_64.IMUL_reg (scratch, rightReg)
                         X86_64.MOV_reg (destReg, scratch)]
                else
                    let setup = if destReg <> leftReg then [X86_64.MOV_reg (destReg, leftReg)] else []
                    setup @ [X86_64.IMUL_reg (destReg, rightReg)])))

let internal emitSdiv (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<X86_64.Instr list, string> =
    // IDIV: RDX:RAX / src → RAX=quotient, RDX=remainder.
    // Clobbers both RAX and RDX. Save/restore RDX using the red zone
    // (below RSP) to avoid changing RSP.
    // Special case: INT64_MIN / -1 traps with #DE (SIGFPE). LIR.Sdiv
    // is defined to wrap to INT64_MIN, so detect the case and bypass.
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg left
        |> Result.bind (fun leftReg ->
            resolveReg right
            |> Result.map (fun rightReg ->
                let overflowLabel = freshLabel "idiv_overflow"
                let doneLabel = freshLabel "idiv_done"
                let divisor =
                    if rightReg = X86_64.RAX || rightReg = X86_64.RDX then scratch
                    else rightReg
                let saveDivisor =
                    if rightReg = X86_64.RAX || rightReg = X86_64.RDX then
                        [X86_64.MOV_reg (scratch, rightReg)]
                    else []
                let moveLeft =
                    if leftReg <> X86_64.RAX then [X86_64.MOV_reg (X86_64.RAX, leftReg)]
                    else []
                // Check for INT64_MIN / -1 overflow
                saveDivisor
                @ moveLeft
                @ [X86_64.CMP_imm (divisor, -1)]
                @ [X86_64.Jcc (X86_64.NE, doneLabel)]
                // divisor is -1, check if dividend is INT64_MIN
                @ loadImm64 scratch System.Int64.MinValue
                @ [X86_64.CMP_reg (X86_64.RAX, scratch)]
                @ [X86_64.Jcc (X86_64.EQ, overflowLabel)]
                // Normal IDIV path
                @ [X86_64.Label doneLabel]
                // Restore divisor if it was moved to scratch for the CMP
                @ (if rightReg = X86_64.RAX || rightReg = X86_64.RDX then
                       [X86_64.MOV_reg (scratch, divisor)]  // re-setup (was clobbered by INT64_MIN load)
                   else [])
                @ (if (rightReg = X86_64.RAX || rightReg = X86_64.RDX) then
                       [X86_64.MOV_reg (scratch, rightReg)]
                   else [])
                @ (if leftReg <> X86_64.RAX then [X86_64.MOV_reg (X86_64.RAX, leftReg)] else [])
                @ [X86_64.MOV_store (X86_64.RSP, -8, X86_64.RDX)]
                @ [X86_64.CQO; X86_64.IDIV divisor]
                @ (if destReg = X86_64.RDX then [X86_64.MOV_reg (scratch, X86_64.RAX)]
                   elif destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)]
                   else [])
                @ [X86_64.MOV_load (X86_64.RDX, X86_64.RSP, -8)]
                @ (if destReg = X86_64.RDX then [X86_64.MOV_reg (X86_64.RDX, scratch)] else [])
                @ [X86_64.JMP (overflowLabel + "_end")]
                // Overflow path: return INT64_MIN
                @ [X86_64.Label overflowLabel]
                @ loadImm64 destReg System.Int64.MinValue
                @ [X86_64.Label (overflowLabel + "_end")])))

let internal emitUdiv (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg left
        |> Result.bind (fun leftReg ->
            resolveReg right
            |> Result.map (fun rightReg ->
                let divisor =
                    if rightReg = X86_64.RAX || rightReg = X86_64.RDX then scratch
                    else rightReg
                let saveDivisor =
                    if rightReg = X86_64.RAX || rightReg = X86_64.RDX then
                        [X86_64.MOV_reg (scratch, rightReg)]
                    else []
                saveDivisor
                @ (if leftReg <> X86_64.RAX then [X86_64.MOV_reg (X86_64.RAX, leftReg)] else [])
                @ [X86_64.MOV_store (X86_64.RSP, -8, X86_64.RDX)
                   X86_64.XOR_reg (X86_64.RDX, X86_64.RDX)
                   X86_64.DIV divisor]
                @ (if destReg = X86_64.RDX then [X86_64.MOV_reg (scratch, X86_64.RAX)]
                   elif destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)]
                   else [])
                @ [X86_64.MOV_load (X86_64.RDX, X86_64.RSP, -8)]
                @ (if destReg = X86_64.RDX then [X86_64.MOV_reg (X86_64.RDX, scratch)] else []))))

let internal emitMsub (ctx: FuncCtx) (dest: LIR.Reg) (mulLeft: LIR.Reg) (mulRight: LIR.Reg) (sub: LIR.Reg) : Result<X86_64.Instr list, string> =
    // dest = sub - mulLeft * mulRight
    // No fused instruction on x86_64. Preserve a nonoperand temporary so
    // physical X11 remains valid as an allocated destination.
    resolveReg dest
    |> Result.bind (fun destReg ->
        resolveReg mulLeft
        |> Result.bind (fun mlReg ->
            resolveReg mulRight
            |> Result.bind (fun mrReg ->
                resolveReg sub
                |> Result.map (fun subReg ->
                    let temp = arithmeticTempExcluding [destReg; mlReg; mrReg; subReg]
                    [ X86_64.PUSH temp
                      X86_64.MOV_reg (temp, mlReg)
                      X86_64.IMUL_reg (temp, mrReg) ]
                    @ (if destReg <> subReg then [X86_64.MOV_reg (destReg, subReg)] else [])
                    @ [ X86_64.SUB_reg (destReg, temp)
                        X86_64.POP temp ]))))

let internal emitCmp (ctx: FuncCtx) (left: LIR.Reg) (right: LIR.Operand) : Result<X86_64.Instr list, string> =
    resolveReg left
    |> Result.bind (fun leftReg ->
        match right with
        | LIR.Imm value when value >= int64 System.Int32.MinValue && value <= int64 System.Int32.MaxValue ->
            Ok [X86_64.CMP_imm (leftReg, int32 value)]
        | LIR.Imm value ->
            if leftReg = scratch then
                Ok ([X86_64.PUSH X86_64.RCX]
                    @ loadImm64 X86_64.RCX value
                    @ [X86_64.CMP_reg (leftReg, X86_64.RCX)
                       X86_64.POP X86_64.RCX])
            else
                Ok (loadImm64 scratch value @ [X86_64.CMP_reg (leftReg, scratch)])
        | LIR.Reg rightReg ->
            resolveReg rightReg
            |> Result.map (fun rightX86 ->
                if leftReg = scratch && rightX86 = scratch then
                    // Both are R11 - always equal, just emit CMP R11, R11
                    [X86_64.CMP_reg (scratch, scratch)]
                else
                    [X86_64.CMP_reg (leftReg, rightX86)])
        | LIR.StackSlot offset ->
            let adjOff = int32 (adjustStackOffset ctx offset)
            if leftReg = scratch then
                Ok ([X86_64.PUSH X86_64.RCX
                     X86_64.MOV_load (X86_64.RCX, X86_64.RBP, adjOff)
                     X86_64.CMP_reg (leftReg, X86_64.RCX)
                     X86_64.POP X86_64.RCX])
            else
                Ok [X86_64.MOV_load (scratch, X86_64.RBP, adjOff); X86_64.CMP_reg (leftReg, scratch)]
        | _ -> Error $"Unsupported Cmp right operand: {right}")

let internal emitCset (ctx: FuncCtx) (comparisonContext: ComparisonContext option) (dest: LIR.Reg) (cond: LIR.Condition) : Result<X86_64.Instr list, string> =
    match comparisonContext with
    | None ->
        Error "x64 codegen: Cset without a preceding comparison in the same block"
    | Some comparisonContext ->
        resolveReg dest
        |> Result.map (fun destReg ->
            if comparisonContext = FloatComparison then
                match cond with
                | LIR.EQ ->
                    // Float EQ: ordered AND equal (ZF=1 AND PF=0)
                    // SETE + SETNP, then AND
                    [X86_64.SETcc (X86_64.EQ, destReg)
                     X86_64.MOVZX_byte (destReg, destReg)
                     X86_64.SETcc (X86_64.NP, scratch)
                     X86_64.MOVZX_byte (scratch, scratch)
                     X86_64.AND_reg (destReg, scratch)]
                | LIR.NE ->
                    // Float NE: unordered OR not equal (ZF=0 OR PF=1)
                    // SETNE + SETP, then OR
                    [X86_64.SETcc (X86_64.NE, destReg)
                     X86_64.MOVZX_byte (destReg, destReg)
                     X86_64.SETcc (X86_64.P, scratch)
                     X86_64.MOVZX_byte (scratch, scratch)
                     X86_64.OR_reg (destReg, scratch)]
                | LIR.LT | LIR.ULT -> [X86_64.SETcc (X86_64.B, destReg); X86_64.MOVZX_byte (destReg, destReg)]
                | LIR.GT | LIR.UGT -> [X86_64.SETcc (X86_64.A, destReg); X86_64.MOVZX_byte (destReg, destReg)]
                | LIR.LE | LIR.ULE -> [X86_64.SETcc (X86_64.BE, destReg); X86_64.MOVZX_byte (destReg, destReg)]
                | LIR.GE | LIR.UGE -> [X86_64.SETcc (X86_64.AE, destReg); X86_64.MOVZX_byte (destReg, destReg)]
            else
                let x86Cond =
                    match cond with
                    | LIR.EQ -> X86_64.EQ | LIR.NE -> X86_64.NE
                    | LIR.LT -> X86_64.LT | LIR.GT -> X86_64.GT
                    | LIR.LE -> X86_64.LE | LIR.GE -> X86_64.GE
                    | LIR.ULT -> X86_64.B | LIR.UGT -> X86_64.A
                    | LIR.ULE -> X86_64.BE | LIR.UGE -> X86_64.AE
                [X86_64.SETcc (x86Cond, destReg); X86_64.MOVZX_byte (destReg, destReg)])

let internal emitSelect
    (ctx: FuncCtx)
    (comparisonContext: ComparisonContext option)
    (dest: LIR.Reg)
    (whenTrue: LIR.Reg)
    (whenFalse: LIR.Reg)
    (cond: LIR.Condition)
    : Result<X86_64.Instr list, string> =
    match comparisonContext with
    | Some IntegerComparison ->
        resolveReg dest
        |> Result.bind (fun destReg ->
            resolveReg whenTrue
            |> Result.bind (fun trueReg ->
                resolveReg whenFalse
                |> Result.map (fun falseReg ->
                    let condition =
                        match cond with
                        | LIR.EQ -> X86_64.EQ | LIR.NE -> X86_64.NE
                        | LIR.LT -> X86_64.LT | LIR.GT -> X86_64.GT
                        | LIR.LE -> X86_64.LE | LIR.GE -> X86_64.GE
                        | LIR.ULT -> X86_64.B | LIR.UGT -> X86_64.A
                        | LIR.ULE -> X86_64.BE | LIR.UGE -> X86_64.AE
                    let inverse =
                        match condition with
                        | X86_64.EQ -> X86_64.NE | X86_64.NE -> X86_64.EQ
                        | X86_64.LT -> X86_64.GE | X86_64.GE -> X86_64.LT
                        | X86_64.GT -> X86_64.LE | X86_64.LE -> X86_64.GT
                        | X86_64.B -> X86_64.AE | X86_64.AE -> X86_64.B
                        | X86_64.A -> X86_64.BE | X86_64.BE -> X86_64.A
                        | X86_64.P -> X86_64.NP | X86_64.NP -> X86_64.P
                    if trueReg = falseReg then
                        if destReg = trueReg then [] else [X86_64.MOV_reg (destReg, trueReg)]
                    elif destReg = trueReg then
                        [X86_64.CMOVcc (inverse, destReg, falseReg)]
                    else
                        (if destReg = falseReg then [] else [X86_64.MOV_reg (destReg, falseReg)])
                        @ [X86_64.CMOVcc (condition, destReg, trueReg)])))
    | Some FloatComparison -> Error "x64 codegen: integer Select after floating-point comparison"
    | None -> Error "x64 codegen: Select without a preceding comparison in the same block"

let internal emitAnd (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg left |> Result.bind (fun l -> resolveReg right |> Result.map (fun r ->
        if d = r && d <> l then
            [X86_64.AND_reg (d, l)]  // AND is commutative
        else
            (if d <> l then [X86_64.MOV_reg (d, l)] else []) @ [X86_64.AND_reg (d, r)])))

let internal emitAnd_imm (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (imm: int64) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        let setup = if d <> s then [X86_64.MOV_reg (d, s)] else []
        if imm >= int64 System.Int32.MinValue && imm <= int64 System.Int32.MaxValue then
            setup @ [X86_64.AND_imm (d, int32 imm)]
        elif d = scratch then
            [X86_64.PUSH X86_64.RCX]
            @ setup
            @ loadImm64 X86_64.RCX imm
            @ [X86_64.AND_reg (d, X86_64.RCX); X86_64.POP X86_64.RCX]
        else
            setup @ loadImm64 scratch imm @ [X86_64.AND_reg (d, scratch)]))

let internal emitOrr (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg left |> Result.bind (fun l -> resolveReg right |> Result.map (fun r ->
        if d = r && d <> l then [X86_64.OR_reg (d, l)]  // OR is commutative
        else (if d <> l then [X86_64.MOV_reg (d, l)] else []) @ [X86_64.OR_reg (d, r)])))

let internal emitEor (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Reg) (right: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg left |> Result.bind (fun l -> resolveReg right |> Result.map (fun r ->
        if d = r && d <> l then [X86_64.XOR_reg (d, l)]  // XOR is commutative
        else (if d <> l then [X86_64.MOV_reg (d, l)] else []) @ [X86_64.XOR_reg (d, r)])))

let internal emitLsl_imm (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (shift: int) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.SHL_imm (d, shift)]))

let internal emitLsr_imm (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (shift: int) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.SHR_imm (d, shift)]))

let internal emitAsr_imm (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (shift: int) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.SAR_imm (d, shift)]))

let internal emitNeg (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.NEG d]))

let internal emitMvn (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        (if d <> s then [X86_64.MOV_reg (d, s)] else []) @ [X86_64.NOT d]))

let internal emitSxtb (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        [X86_64.MOVSX_byte (d, s)]))

let internal emitSxth (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        [X86_64.MOVSX_word (d, s)]))

let internal emitSxtw (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        [X86_64.MOVSXD (d, s)]))

let internal emitUxtb (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        [X86_64.MOVZX_byte (d, s)]))

let internal emitExit (ctx: FuncCtx) : Result<X86_64.Instr list, string> =
    // RDI should already contain the exit code
    Ok genExitSyscall

let internal emitStdoutWrite (ctx: FuncCtx) (value: LIR.Operand) (appendNewline: bool) : Result<X86_64.Instr list, string> =
    let writeLoop prefix =
        let loopLabel = freshLabel $"{prefix}_write"
        let retryLabel = freshLabel $"{prefix}_retry"
        let doneLabel = freshLabel $"{prefix}_done"
        [ X86_64.Label loopLabel
          X86_64.CMP_imm (X86_64.RDX, 0)
          X86_64.Jcc (X86_64.LE, doneLabel)
          X86_64.Label retryLabel ]
        @ genWriteSyscall
        @ [ X86_64.CMP_imm (X86_64.RAX, -4) // Linux EINTR
            X86_64.Jcc (X86_64.EQ, retryLabel)
            X86_64.CMP_imm (X86_64.RAX, 0)
            X86_64.Jcc (X86_64.LE, doneLabel)
            X86_64.ADD_reg (X86_64.RSI, X86_64.RAX)
            X86_64.SUB_reg (X86_64.RDX, X86_64.RAX)
            X86_64.JMP loopLabel
            X86_64.Label doneLabel ]

    let setupValue =
        match value with
        | LIR.Reg reg ->
            resolveReg reg
            |> Result.map (fun src ->
                [ X86_64.MOV_reg (X86_64.R10, src)
                  X86_64.MOV_load (X86_64.RDX, X86_64.R10, 8)
                  X86_64.LEA (X86_64.RSI, X86_64.R10, 16) ])
        | LIR.StackSlot offset ->
            Ok [ X86_64.MOV_load (X86_64.R10, X86_64.RBP, int32 (adjustStackOffset ctx offset))
                 X86_64.MOV_load (X86_64.RDX, X86_64.R10, 8)
                 X86_64.LEA (X86_64.RSI, X86_64.R10, 16) ]
        | LIR.StringSymbol text ->
            Ok (emitStringLiteralNoRefCount X86_64.R10 text
                @ [ X86_64.MOV_load (X86_64.RDX, X86_64.R10, 8)
                    X86_64.LEA (X86_64.RSI, X86_64.R10, 16) ])
        | _ -> Error "StdoutWrite requires a String operand"

    setupValue
    |> Result.map (fun setup ->
        let saved =
            [ X86_64.RAX; X86_64.RDI; X86_64.RSI; X86_64.RDX; X86_64.RCX
              X86_64.R8; X86_64.R9; X86_64.R10; X86_64.R11 ]
        let save = saved |> List.map X86_64.PUSH
        let restore = saved |> List.rev |> List.map X86_64.POP
        let newline =
            if not appendNewline then []
            else
                [ X86_64.SUB_imm (X86_64.RSP, 8) ]
                @ loadImm64 scratch 10L
                @ [ X86_64.MOV_store (X86_64.RSP, 0, scratch)
                    X86_64.MOV_imm32 (X86_64.RDI, 1)
                    X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
                    X86_64.MOV_imm32 (X86_64.RDX, 1) ]
                @ writeLoop "stdout_newline"
                @ [ X86_64.ADD_imm (X86_64.RSP, 8) ]
        save
        @ setup
        @ [ X86_64.MOV_imm32 (X86_64.RDI, 1) ]
        @ writeLoop "stdout"
        @ newline
        @ restore)

let internal emitStdinReadLine (ctx: FuncCtx) (dest: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.map (fun destReg ->
        let readLabel = freshLabel "stdin_read"
        let retryLabel = freshLabel "stdin_retry"
        let gotByteLabel = freshLabel "stdin_byte"
        let finishLabel = freshLabel "stdin_finish"
        let noCrLabel = freshLabel "stdin_no_cr"
        let saved =
            [ X86_64.RAX; X86_64.RDI; X86_64.RSI; X86_64.RDX; X86_64.RCX
              X86_64.R8; X86_64.R9; X86_64.R10; X86_64.R11 ]
        let save = saved |> List.map X86_64.PUSH
        let restore = saved |> List.rev |> List.map X86_64.POP
        let savedBytes = int32 (List.length saved * 8 + 8)
        save
        @ [ X86_64.SUB_imm (X86_64.RSP, 16) ]
        @ loadImm64 X86_64.R10 0L
        @ [ X86_64.MOV_store (X86_64.RSP, 0, X86_64.R10)
            X86_64.Label readLabel
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 0)
            X86_64.LEA (X86_64.RSI, heapPtr, 16)
            X86_64.ADD_reg (X86_64.RSI, X86_64.R10)
            X86_64.XOR_reg (X86_64.RDI, X86_64.RDI)
            X86_64.MOV_imm32 (X86_64.RDX, 1)
            X86_64.Label retryLabel ]
        @ loadImm64 X86_64.RAX (int64 syscalls.Read)
        @ [ X86_64.SYSCALL
            X86_64.CMP_imm (X86_64.RAX, -4)
            X86_64.Jcc (X86_64.EQ, retryLabel)
            X86_64.CMP_imm (X86_64.RAX, 1)
            X86_64.Jcc (X86_64.EQ, gotByteLabel)
            X86_64.JMP finishLabel
            X86_64.Label gotByteLabel
            X86_64.MOV_load_byte (X86_64.R11, X86_64.RSI, 0)
            X86_64.CMP_imm (X86_64.R11, 10)
            X86_64.Jcc (X86_64.EQ, finishLabel)
            X86_64.ADD_imm (X86_64.R10, 1)
            X86_64.MOV_store (X86_64.RSP, 0, X86_64.R10)
            X86_64.JMP readLabel
            X86_64.Label finishLabel
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 0)
            X86_64.CMP_imm (X86_64.R10, 0)
            X86_64.Jcc (X86_64.LE, noCrLabel)
            X86_64.LEA (X86_64.RSI, heapPtr, 15)
            X86_64.ADD_reg (X86_64.RSI, X86_64.R10)
            X86_64.MOV_load_byte (X86_64.R11, X86_64.RSI, 0)
            X86_64.CMP_imm (X86_64.R11, 13)
            X86_64.Jcc (X86_64.NE, noCrLabel)
            X86_64.SUB_imm (X86_64.R10, 1)
            X86_64.Label noCrLabel
            X86_64.MOV_imm32 (X86_64.RAX, 1)
            X86_64.MOV_store (heapPtr, 0, X86_64.RAX)
            X86_64.MOV_store (heapPtr, 8, X86_64.R10)
            X86_64.MOV_reg (X86_64.R11, X86_64.R10)
            X86_64.ADD_imm (X86_64.R11, 7)
            X86_64.AND_imm (X86_64.R11, -8) ]
        @ [
            X86_64.MOV_store (X86_64.RSP, 8, heapPtr)
            X86_64.ADD_imm (X86_64.R11, 16)
            X86_64.ADD_reg (heapPtr, X86_64.R11) ]
        @ genLeakCounterInc ctx
        @ [ X86_64.ADD_imm (X86_64.RSP, 16) ]
        @ restore
        @ [ X86_64.MOV_load (destReg, X86_64.RSP, -savedBytes) ])

let internal emitRuntimeError (ctx: FuncCtx) (msg: string) : Result<X86_64.Instr list, string> =
    Ok (emitStringLiteral X86_64.R8 msg
        @ [X86_64.JMP runtimeErrorHandlerLabel])

let internal emitRuntimeErrorString (ctx: FuncCtx) (messageReg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg messageReg
    |> Result.map (fun resolvedMessageReg ->
        [X86_64.MOV_reg (X86_64.R8, resolvedMessageReg)]
        @ [X86_64.MOV_load (X86_64.RDX, X86_64.R8, 8)
           X86_64.LEA (X86_64.RSI, X86_64.R8, 16)
           X86_64.MOV_imm32 (X86_64.RDI, 2)]
        @ genWriteSyscall
        @ loadImm64 X86_64.RDI 1L
        @ genExitSyscall)

let internal emitArgMoves (ctx: FuncCtx) (moves: (LIR.PhysReg * LIR.Operand) list) : Result<X86_64.Instr list, string> =
    // Parallel move resolution for function arguments.
    // Must handle case where a source register is also a destination of another
    // move (e.g., X1 <- X21; X4 <- X1 — second move must read ORIGINAL X1).
    //
    // Strategy: save all source registers that will be clobbered to scratch stack,
    // then perform all moves using saved values where needed.
    let generateMove (destPhys: LIR.PhysReg, srcOp: LIR.Operand) : Result<X86_64.Instr list, string> =
        let destX86 = lirRegToX86 destPhys
        match srcOp with
        | LIR.Imm value ->
            Ok (loadImm64 destX86 value)
        | LIR.Reg (LIR.Physical srcPhys) ->
            if srcPhys = destPhys then Ok []
            else
                // If source will be clobbered by an earlier move, we need the
                // saved value. For simplicity, we use a two-pass approach:
                // all moves from Reg sources that ARE destinations get saved first.
                Ok [X86_64.MOV_reg (destX86, lirRegToX86 srcPhys)]
        | LIR.Reg (LIR.Virtual _) ->
            Error "Virtual register in ArgMoves"
        | LIR.StackSlot offset ->
            Ok [X86_64.MOV_load (destX86, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
        | LIR.StringSymbol value ->
            Ok (emitStringLiteral destX86 value)
        | LIR.FloatSymbol value ->
            let bits = System.BitConverter.DoubleToInt64Bits(value)
            Ok (loadImm64 destX86 bits)  // Store float bits in GP register (for passing as arg)
        | LIR.FuncAddr funcName ->
            Ok [X86_64.LEA_rip (destX86, funcName)]
        | _ -> Error $"Unsupported ArgMoves operand: {srcOp}"
    // Two-pass approach to handle parallel move conflicts:
    // 1. Find source registers that are also destinations (will be clobbered)
    // 2. Save those to the red zone before any moves
    // 3. Do all moves, using red zone values for clobbered sources
    let destRegSet = moves |> List.map fst |> Set.ofList
    let clobberedSources =
        moves
        |> List.choose (fun (_, srcOp) ->
            match srcOp with
            | LIR.Reg (LIR.Physical srcPhys) ->
                if Set.contains srcPhys destRegSet then Some srcPhys
                else None
            | _ -> None)
        |> List.distinct

    // Save clobbered sources to red zone (below RSP, no RSP adjustment)
    // Use offsets -16, -24, -32, etc. (-8 is used by IDIV)
    let saveInstrs =
        clobberedSources
        |> List.mapi (fun i reg ->
            let offset = -16 - (i * 8)
            X86_64.MOV_store (X86_64.RSP, int32 offset, lirRegToX86 reg))

    // Build a map from clobbered source to red zone offset
    let clobberedOffsets =
        clobberedSources
        |> List.mapi (fun i reg -> (reg, -16 - (i * 8)))
        |> Map.ofList

    // Generate moves, using red zone for clobbered sources
    let generateMoveWithSave (destPhys: LIR.PhysReg, srcOp: LIR.Operand) : Result<X86_64.Instr list, string> =
        let destX86 = lirRegToX86 destPhys
        match srcOp with
        | LIR.Reg (LIR.Physical srcPhys) when Map.containsKey srcPhys clobberedOffsets ->
            if srcPhys = destPhys then Ok []
            else
                let offset = clobberedOffsets.[srcPhys]
                Ok [X86_64.MOV_load (destX86, X86_64.RSP, int32 offset)]
        | _ -> generateMove (destPhys, srcOp)

    let rec genMoves acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | m :: rest ->
            match generateMoveWithSave m with
            | Error e -> Error e
            | Ok instrs -> genMoves (instrs :: acc) rest
    genMoves [] moves
    |> Result.map (fun moveInstrs -> saveInstrs @ moveInstrs)

let internal emitTailArgMoves (ctx: FuncCtx) (moves: (LIR.PhysReg * LIR.Operand) list) : Result<X86_64.Instr list, string> =
    // Same parallel move resolution as ArgMoves
    let destRegSet = moves |> List.map fst |> Set.ofList
    let clobberedSources =
        moves |> List.choose (fun (_, srcOp) ->
            match srcOp with
            | LIR.Reg (LIR.Physical srcPhys) when Set.contains srcPhys destRegSet -> Some srcPhys
            | _ -> None)
        |> List.distinct
    let saveInstrs =
        clobberedSources |> List.mapi (fun i reg ->
            X86_64.MOV_store (X86_64.RSP, int32 (-16 - i * 8), lirRegToX86 reg))
    let clobberedOffsets =
        clobberedSources |> List.mapi (fun i reg -> (reg, -16 - i * 8)) |> Map.ofList
    let generateMove (destPhys: LIR.PhysReg, srcOp: LIR.Operand) : Result<X86_64.Instr list, string> =
        let destX86 = lirRegToX86 destPhys
        match srcOp with
        | LIR.Imm value -> Ok (loadImm64 destX86 value)
        | LIR.Reg (LIR.Physical srcPhys) when Map.containsKey srcPhys clobberedOffsets ->
            if srcPhys = destPhys then Ok []
            else Ok [X86_64.MOV_load (destX86, X86_64.RSP, int32 clobberedOffsets.[srcPhys])]
        | LIR.Reg (LIR.Physical srcPhys) ->
            if srcPhys = destPhys then Ok []
            else Ok [X86_64.MOV_reg (destX86, lirRegToX86 srcPhys)]
        | LIR.StackSlot offset ->
            Ok [X86_64.MOV_load (destX86, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
        | LIR.StringSymbol value ->
            Ok (emitStringLiteral destX86 value)
        | LIR.FloatSymbol value ->
            let bits = System.BitConverter.DoubleToInt64Bits(value)
            Ok (loadImm64 destX86 bits)
        | LIR.FuncAddr funcName ->
            Ok [X86_64.LEA_rip (destX86, funcName)]
        | _ -> Error $"Unsupported TailArgMoves operand: {srcOp}"
    let rec genMoves acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | m :: rest ->
            match generateMove m with
            | Error e -> Error e
            | Ok instrs -> genMoves (instrs :: acc) rest
    genMoves [] moves
    |> Result.map (fun moveInstrs -> saveInstrs @ moveInstrs)

let internal emitPhi (ctx: FuncCtx) (dest: LIR.Reg) : Result<X86_64.Instr list, string> =
    // Phi nodes should be eliminated before codegen (SSA destruction)
    // If we see one, it's a no-op — the parallel moves handle it
    Ok []

let internal emitInt64ToFloat (ctx: FuncCtx) (dest: LIR.FReg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    match dest with
    | LIR.FPhysical dp ->
        resolveReg src
        |> Result.map (fun srcReg -> [X86_64.CVTSI2SD (lirFRegToX86 dp, srcReg)])
    | _ -> Error "Int64ToFloat with virtual FP register"

let internal emitGpToFp (ctx: FuncCtx) (dest: LIR.FReg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    match dest with
    | LIR.FPhysical dp ->
        resolveReg src
        |> Result.map (fun srcReg -> [X86_64.MOVQ_from_gp (lirFRegToX86 dp, srcReg)])
    | _ -> Error "GpToFp with virtual FP register"

let internal emitLsl (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (shift: LIR.Reg) : Result<X86_64.Instr list, string> =
    // SHL by register: shift amount must be in CL (lower byte of RCX)
    // Save/restore RCX if it's not the shift operand or dest (clobber not modeled by regalloc)
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.bind (fun s ->
        resolveReg shift |> Result.map (fun shReg ->
            let needSaveRCX = shReg <> X86_64.RCX && d <> X86_64.RCX
            let save = if needSaveRCX then [X86_64.PUSH X86_64.RCX] else []
            let restore = if needSaveRCX then [X86_64.POP X86_64.RCX] else []
            if d = shReg && d <> s then
                // dest == shift register: moving src to dest would clobber shift.
                // Use scratch to save src, then move shift to RCX, then put src in dest.
                // This handles the case where s = RCX (which MOV RCX,shReg would clobber).
                save
                @ [X86_64.MOV_reg (scratch, s)]
                @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                @ [X86_64.MOV_reg (d, scratch)]
                @ [X86_64.SHL_cl d]
                @ restore
            elif d = X86_64.RCX && shReg <> X86_64.RCX then
                // dest is RCX: MOV d,s then MOV RCX,shReg would clobber src in d.
                // Use scratch to hold value, shift there, move result back.
                [X86_64.MOV_reg (scratch, s)
                 X86_64.MOV_reg (X86_64.RCX, shReg)
                 X86_64.SHL_cl scratch
                 X86_64.MOV_reg (X86_64.RCX, scratch)]
            else
                save
                @ (if d <> s then [X86_64.MOV_reg (d, s)] else [])
                @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                @ [X86_64.SHL_cl d]
                @ restore)))

let internal emitLsr (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (shift: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.bind (fun s ->
        resolveReg shift |> Result.map (fun shReg ->
            let needSaveRCX = shReg <> X86_64.RCX && d <> X86_64.RCX
            let save = if needSaveRCX then [X86_64.PUSH X86_64.RCX] else []
            let restore = if needSaveRCX then [X86_64.POP X86_64.RCX] else []
            if d = shReg && d <> s then
                // dest == shift: save src via scratch to avoid clobbering when s=RCX
                save
                @ [X86_64.MOV_reg (scratch, s)]
                @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                @ [X86_64.MOV_reg (d, scratch)]
                @ [X86_64.SHR_cl d]
                @ restore
            elif d = X86_64.RCX && shReg <> X86_64.RCX then
                // dest is RCX: use scratch to avoid clobbering
                [X86_64.MOV_reg (scratch, s)
                 X86_64.MOV_reg (X86_64.RCX, shReg)
                 X86_64.SHR_cl scratch
                 X86_64.MOV_reg (X86_64.RCX, scratch)]
            else
                save
                @ (if d <> s then [X86_64.MOV_reg (d, s)] else [])
                @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                @ [X86_64.SHR_cl d]
                @ restore)))

let internal emitAsr (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) (shift: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.bind (fun s ->
        resolveReg shift |> Result.map (fun shReg ->
            let needSaveRCX = shReg <> X86_64.RCX && d <> X86_64.RCX
            let save = if needSaveRCX then [X86_64.PUSH X86_64.RCX] else []
            let restore = if needSaveRCX then [X86_64.POP X86_64.RCX] else []
            if d = shReg && d <> s then
                save
                @ [X86_64.MOV_reg (scratch, s)]
                @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                @ [X86_64.MOV_reg (d, scratch); X86_64.SAR_cl d]
                @ restore
            elif d = X86_64.RCX && shReg <> X86_64.RCX then
                [X86_64.MOV_reg (scratch, s)
                 X86_64.MOV_reg (X86_64.RCX, shReg)
                 X86_64.SAR_cl scratch
                 X86_64.MOV_reg (X86_64.RCX, scratch)]
            else
                save
                @ (if d <> s then [X86_64.MOV_reg (d, s)] else [])
                @ (if shReg <> X86_64.RCX then [X86_64.MOV_reg (X86_64.RCX, shReg)] else [])
                @ [X86_64.SAR_cl d]
                @ restore)))

let internal emitUxth (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        [X86_64.MOVZX_word (d, s)]))

let internal emitUxtw (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.Reg) : Result<X86_64.Instr list, string> =
    // 32-bit MOV zero-extends to 64-bit on x86_64
    resolveReg dest |> Result.bind (fun d -> resolveReg src |> Result.map (fun s ->
        [X86_64.MOV_reg32 (d, s)]))

let internal emitClosureAlloc (ctx: FuncCtx) (dest: LIR.Reg) (funcName: string) (captures: LIR.Operand list) : Result<X86_64.Instr list, string> =
    // Allocate closure on heap: [func_ptr, cap1, cap2, ...][refcount]
    resolveReg dest
    |> Result.bind (fun destReg ->
        let numSlots = 1 + List.length captures
        let sizeBytes = numSlots * 8
        let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)  // + refcount, aligned
        let alloc = [
            X86_64.MOV_reg (destReg, heapPtr)
            X86_64.ADD_imm (heapPtr, int32 totalSize)
        ]
        // Store refcount = 1
        let storeRC =
            loadImm64 scratch 1L
            @ [X86_64.MOV_store (destReg, int32 sizeBytes, scratch)]
        // Store function address at offset 0
        let storeFunc = [
            X86_64.LEA_rip (scratch, funcName)
            X86_64.MOV_store (destReg, 0, scratch)
        ]
        // Store captures
        let storeCaptures =
            captures
            |> List.mapi (fun i cap -> (i, cap))
            |> List.collect (fun (i, cap) ->
                let offset = (i + 1) * 8
                match cap with
                | LIR.Imm value ->
                    loadImm64 scratch value
                    @ [X86_64.MOV_store (destReg, int32 offset, scratch)]
                | LIR.Reg reg ->
                    match resolveReg reg with
                    | Ok srcReg -> [X86_64.MOV_store (destReg, int32 offset, srcReg)]
                    | Error _ -> []
                | LIR.StackSlot stackOffset ->
                    let adjOff = adjustStackOffset ctx stackOffset
                    [X86_64.MOV_load (scratch, X86_64.RBP, int32 adjOff)
                     X86_64.MOV_store (destReg, int32 offset, scratch)]
                | _ -> [])
        Ok (alloc @ storeRC @ storeFunc @ storeCaptures @ genLeakCounterInc ctx))

let internal emitMadd (ctx: FuncCtx) (dest: LIR.Reg) (mulLeft: LIR.Reg) (mulRight: LIR.Reg) (add: LIR.Reg) : Result<X86_64.Instr list, string> =
    // dest = add + mulLeft * mulRight
    resolveReg dest |> Result.bind (fun d ->
        resolveReg mulLeft |> Result.bind (fun ml ->
            resolveReg mulRight |> Result.bind (fun mr ->
                resolveReg add |> Result.map (fun addReg ->
                    let temp = arithmeticTempExcluding [d; ml; mr; addReg]
                    [ X86_64.PUSH temp
                      X86_64.MOV_reg (temp, ml)
                      X86_64.IMUL_reg (temp, mr) ]
                    @ (if d <> addReg then [X86_64.MOV_reg (d, addReg)] else [])
                    @ [ X86_64.ADD_reg (d, temp)
                        X86_64.POP temp ]))))
