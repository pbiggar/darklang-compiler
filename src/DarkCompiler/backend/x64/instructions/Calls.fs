// Calls.fs - Emit x64 instructions for calls operations.

module X64EmitCalls

open X64Operands
open X64Frames
open X64CodeGenTypes
open X64FieldReferenceCounts

let internal emitSaveRegs (ctx: FuncCtx) (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) : Result<X86_64.Instr list, string> =
    // Save caller-saved registers that are live across a call.
    // PUSH each in order — first pushed is deepest on the stack.
    if List.isEmpty intRegs && List.isEmpty floatRegs then
        Ok []
    else
        let intSaves =
            intRegs |> List.map (fun reg -> X86_64.PUSH (lirRegToX86 reg))
        let floatSaves =
            floatRegs |> List.collect (fun freg ->
                let xmm = lirFRegToX86 freg
                // SUB RSP, 8; MOVSD [RSP], xmm
                [X86_64.SUB_imm (X86_64.RSP, 8)
                 X86_64.MOVSD_store (X86_64.RSP, 0, xmm)])
        // Track save area size for RestoreRegs/ArgMoves
        Ok (intSaves @ floatSaves)

let internal emitRestoreRegs (ctx: FuncCtx) (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) : Result<X86_64.Instr list, string> =
    if List.isEmpty intRegs && List.isEmpty floatRegs then
        Ok []
    else
        // Restore in reverse order of saves
        let floatRestores =
            floatRegs |> List.rev |> List.collect (fun freg ->
                let xmm = lirFRegToX86 freg
                [X86_64.MOVSD_load (xmm, X86_64.RSP, 0)
                 X86_64.ADD_imm (X86_64.RSP, 8)])
        let intRestores =
            intRegs |> List.rev |> List.map (fun reg -> X86_64.POP (lirRegToX86 reg))
        Ok (floatRestores @ intRestores)

let internal emitCall (ctx: FuncCtx) (dest: LIR.Reg) (funcId: AST.FunctionId) (_args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    // Arguments are already in place from ArgMoves
    resolveReg dest
    |> Result.map (fun destReg ->
        [X86_64.CALL (functionName ctx funcId)]
        @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else []))

let internal emitTailCall (ctx: FuncCtx) (funcId: AST.FunctionId) (_args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    // Restore stack frame before jumping (epilogue without RET)
    Ok (genEpilogue ctx.StackSize ctx.UsedCalleeSaved @ [X86_64.JMP (functionName ctx funcId)])

let internal emitIndirectCall (ctx: FuncCtx) (dest: LIR.Reg) (func: LIR.Reg) (_args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    resolveReg func
    |> Result.bind (fun funcReg ->
        resolveReg dest
        |> Result.map (fun destReg ->
            [X86_64.CALL_reg funcReg]
            @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else [])))

let internal emitIndirectTailCall (ctx: FuncCtx) (func: LIR.Reg) (_args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    resolveReg func
    |> Result.map (fun funcReg ->
        genEpilogue ctx.StackSize ctx.UsedCalleeSaved
        @ [X86_64.JMP_reg funcReg])

let internal emitLoadFuncAddr (ctx: FuncCtx) (dest: LIR.Reg) (funcId: AST.FunctionId) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.map (fun destReg -> [X86_64.LEA_rip (destReg, functionName ctx funcId)])

let internal emitClosureCall (ctx: FuncCtx) (dest: LIR.Reg) (closure: LIR.Reg) (_args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    // The closure register contains the function pointer
    // (LIR does HeapLoad to extract func_ptr before ClosureCall)
    resolveReg closure
    |> Result.bind (fun closureReg ->
        resolveReg dest
        |> Result.map (fun destReg ->
            // Move to R10 if in scratch (R11) to avoid conflicts
            let callReg = if closureReg = scratch then X86_64.R10 else closureReg
            let setup = if callReg <> closureReg then [X86_64.MOV_reg (callReg, closureReg)] else []
            setup
            @ [X86_64.CALL_reg callReg]
            @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else [])))

let internal emitClosureTailCall (ctx: FuncCtx) (closure: LIR.Reg) (_args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    resolveReg closure
    |> Result.map (fun closureReg ->
        let callReg = if closureReg = scratch then X86_64.R10 else closureReg
        let setup = if callReg <> closureReg then [X86_64.MOV_reg (callReg, closureReg)] else []
        setup
        @ genEpilogue ctx.StackSize ctx.UsedCalleeSaved
        @ [X86_64.JMP_reg callReg])
