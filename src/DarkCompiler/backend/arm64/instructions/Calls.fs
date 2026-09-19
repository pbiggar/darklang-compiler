// Calls.fs - Emit arm64 instructions for calls operations.

module ARM64EmitCalls

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64Operands
open ARM64Frames

let internal emitCall (ctx: CodeGenContext) (dest: LIR.Reg) (funcId: AST.FunctionId) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Function call: arguments already moved to X0-X7 by preceding MOVs
    // Caller-save is handled by SaveRegs/RestoreRegs instructions
    Ok [ARM64Symbolic.BL (functionName ctx funcId)]

let internal emitTailCall (ctx: CodeGenContext) (funcId: AST.FunctionId) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Tail call: restore stack frame, then branch (no link)
    // This is the same as the epilogue but with B instead of RET
    let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
    let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
    // Deallocate all stack at once
    let totalExtraStack = ctx.StackSize + calleeSavedSpace
    let deallocStack =
        if totalExtraStack > 0 then
            [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
        else
            []
    let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
    let branch = [ARM64Symbolic.B_label (functionName ctx funcId)]
    Ok (restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

let internal emitIndirectCall (ctx: CodeGenContext) (dest: LIR.Reg) (func: LIR.Reg) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Indirect call: call through function pointer in register
    // Use BLR instruction instead of BL
    lirRegToARM64Reg func
    |> Result.map (fun funcReg -> [ARM64Symbolic.BLR funcReg])

let internal emitIndirectTailCall (ctx: CodeGenContext) (func: LIR.Reg) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Indirect tail call: restore stack frame, then branch to register
    lirRegToARM64Reg func
    |> Result.map (fun funcReg ->
        let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
        let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
        let totalExtraStack = ctx.StackSize + calleeSavedSpace
        let deallocStack =
            if totalExtraStack > 0 then
                [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
            else
                []
        let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
        let branch = [ARM64Symbolic.BR funcReg]
        restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

let internal emitClosureCall (ctx: CodeGenContext) (dest: LIR.Reg) (funcPtr: LIR.Reg) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Call through closure - MIR_to_LIR already set up:
    // - X9: function pointer (loaded from closure[0])
    // - X0: closure
    // - X1-X7: args
    // Just do the BLR
    lirRegToARM64Reg funcPtr
    |> Result.map (fun funcPtrReg ->
        [ARM64Symbolic.BLR funcPtrReg])

let internal emitClosureTailCall (ctx: CodeGenContext) (funcPtr: LIR.Reg) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Closure tail call: restore stack frame, then branch to register
    lirRegToARM64Reg funcPtr
    |> Result.map (fun funcPtrReg ->
        let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
        let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
        let totalExtraStack = ctx.StackSize + calleeSavedSpace
        let deallocStack =
            if totalExtraStack > 0 then
                [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
            else
                []
        let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
        let branch = [ARM64Symbolic.BR funcPtrReg]
        restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

let internal emitSaveRegs (ctx: CodeGenContext) (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) : Result<ARM64Symbolic.Instr list, string> =
    // Save only the caller-saved registers that are live across this call
    // We maintain fixed offsets for ArgMoves compatibility:
    // Layout: X1-X10 at SP+0..SP+72 (fixed), D0-D7 at SP+80..SP+136
    // If no registers need saving, emit nothing (no stack allocation)
    if List.isEmpty intRegs && List.isEmpty floatRegs then
        Ok []  // Nothing to save - no stack allocation needed
    else
        // Determine stack size - we need fixed layout for ArgMoves compatibility
        // when any int registers are saved
        let hasIntRegs = not (List.isEmpty intRegs)
        let hasFloatRegs = not (List.isEmpty floatRegs)
        let intSlotSize = if hasIntRegs then 80 else 0  // X1-X10 (10 regs * 8 bytes)
        let floatSlotSize = if hasFloatRegs then 64 else 0  // D0-D7 (8 regs * 8 bytes)
        let totalSize = intSlotSize + floatSlotSize

        let allocStack = [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalSize)]

        // Save int registers using STP pairs where possible
        // Pairs: (X1,X2)@0, (X3,X4)@16, (X5,X6)@32, (X7,X8)@48, (X9,X10)@64
        let intPairs = [
            (LIR.X1, LIR.X2, 0s)
            (LIR.X3, LIR.X4, 16s)
            (LIR.X5, LIR.X6, 32s)
            (LIR.X7, LIR.X8, 48s)
            (LIR.X9, LIR.X10, 64s)
        ]

        let intSaves : ARM64Symbolic.Instr list =
            intPairs |> List.collect (fun (r1, r2, offset) ->
                let has1 = List.contains r1 intRegs
                let has2 = List.contains r2 intRegs
                match (has1, has2) with
                | (true, true) ->
                    // Both registers - use STP
                    [ARM64Symbolic.STP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset)]
                | (true, false) ->
                    // Only first register - use STR
                    [ARM64Symbolic.STR (lirPhysRegToARM64Reg r1, ARM64Symbolic.SP, offset)]
                | (false, true) ->
                    // Only second register - use STR
                    [ARM64Symbolic.STR (lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset + 8s)]
                | (false, false) ->
                    // Neither register
                    [])

        // Save float registers using STP_fp pairs where possible
        // Pairs: (D0,D1)@0, (D2,D3)@16, (D4,D5)@32, (D6,D7)@48
        let baseFloatOffset = if hasIntRegs then 80s else 0s
        let floatPairs = [
            (LIR.D0, LIR.D1, 0s)
            (LIR.D2, LIR.D3, 16s)
            (LIR.D4, LIR.D5, 32s)
            (LIR.D6, LIR.D7, 48s)
        ]

        let floatSaves : ARM64Symbolic.Instr list =
            floatPairs |> List.collect (fun (f1, f2, offset) ->
                let has1 = List.contains f1 floatRegs
                let has2 = List.contains f2 floatRegs
                match (has1, has2) with
                | (true, true) ->
                    // Both registers - use STP_fp
                    [ARM64Symbolic.STP_fp (lirPhysFPRegToARM64FReg f1, lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset)]
                | (true, false) ->
                    // Only first register - use STR_fp
                    [ARM64Symbolic.STR_fp (lirPhysFPRegToARM64FReg f1, ARM64Symbolic.SP, baseFloatOffset + offset)]
                | (false, true) ->
                    // Only second register - use STR_fp
                    [ARM64Symbolic.STR_fp (lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset + 8s)]
                | (false, false) ->
                    // Neither register
                    [])

        Ok (allocStack @ intSaves @ floatSaves)

let internal emitRestoreRegs (ctx: CodeGenContext) (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) : Result<ARM64Symbolic.Instr list, string> =
    // Restore only the caller-saved registers that are live across this call
    // Must match the layout from SaveRegs
    if List.isEmpty intRegs && List.isEmpty floatRegs then
        Ok []  // Nothing was saved - no stack deallocation needed
    else
        let hasIntRegs = not (List.isEmpty intRegs)
        let hasFloatRegs = not (List.isEmpty floatRegs)
        let intSlotSize = if hasIntRegs then 80 else 0
        let floatSlotSize = if hasFloatRegs then 64 else 0
        let totalSize = intSlotSize + floatSlotSize

        // Restore int registers using LDP pairs where possible
        // Pairs: (X1,X2)@0, (X3,X4)@16, (X5,X6)@32, (X7,X8)@48, (X9,X10)@64
        let intPairs = [
            (LIR.X1, LIR.X2, 0s)
            (LIR.X3, LIR.X4, 16s)
            (LIR.X5, LIR.X6, 32s)
            (LIR.X7, LIR.X8, 48s)
            (LIR.X9, LIR.X10, 64s)
        ]

        let intRestores : ARM64Symbolic.Instr list =
            intPairs |> List.collect (fun (r1, r2, offset) ->
                let has1 = List.contains r1 intRegs
                let has2 = List.contains r2 intRegs
                match (has1, has2) with
                | (true, true) ->
                    // Both registers - use LDP
                    [ARM64Symbolic.LDP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset)]
                | (true, false) ->
                    // Only first register - use LDR
                    [ARM64Symbolic.LDR (lirPhysRegToARM64Reg r1, ARM64Symbolic.SP, offset)]
                | (false, true) ->
                    // Only second register - use LDR
                    [ARM64Symbolic.LDR (lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, offset + 8s)]
                | (false, false) ->
                    // Neither register
                    [])

        // Restore float registers using LDP_fp pairs where possible
        // Pairs: (D0,D1)@0, (D2,D3)@16, (D4,D5)@32, (D6,D7)@48
        let baseFloatOffset = if hasIntRegs then 80s else 0s
        let floatPairs = [
            (LIR.D0, LIR.D1, 0s)
            (LIR.D2, LIR.D3, 16s)
            (LIR.D4, LIR.D5, 32s)
            (LIR.D6, LIR.D7, 48s)
        ]

        let floatRestores : ARM64Symbolic.Instr list =
            floatPairs |> List.collect (fun (f1, f2, offset) ->
                let has1 = List.contains f1 floatRegs
                let has2 = List.contains f2 floatRegs
                match (has1, has2) with
                | (true, true) ->
                    // Both registers - use LDP_fp
                    [ARM64Symbolic.LDP_fp (lirPhysFPRegToARM64FReg f1, lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset)]
                | (true, false) ->
                    // Only first register - use LDR_fp
                    [ARM64Symbolic.LDR_fp (lirPhysFPRegToARM64FReg f1, ARM64Symbolic.SP, baseFloatOffset + offset)]
                | (false, true) ->
                    // Only second register - use LDR_fp
                    [ARM64Symbolic.LDR_fp (lirPhysFPRegToARM64FReg f2, ARM64Symbolic.SP, baseFloatOffset + offset + 8s)]
                | (false, false) ->
                    // Neither register
                    [])

        let deallocStack = [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalSize)]

        Ok (intRestores @ floatRestores @ deallocStack)

let internal emitLoadFuncAddr (ctx: CodeGenContext) (dest: LIR.Reg) (funcId: AST.FunctionId) : Result<ARM64Symbolic.Instr list, string> =
    // Load the address of a function into the destination register using ADR
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        [ARM64Symbolic.ADR (destReg, codeLabel (functionName ctx funcId))])
