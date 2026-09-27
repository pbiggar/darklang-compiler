// Calls.fs - Emit arm64 instructions for calls operations.

module ARM64EmitCalls

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64Operands
open ARM64Frames

let private tailEpilogue (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    generateEpilogue ctx.UsedCalleeSaved ctx.UsedCalleeSavedF ctx.StackSize
    |> List.filter (function ARM64Symbolic.RET -> false | _ -> true)

let internal emitCall (ctx: CodeGenContext) (dest: LIR.Reg) (funcId: AST.FunctionId) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Function call: arguments already moved to X0-X7 by preceding MOVs
    // Caller-save is handled by SaveRegs/RestoreRegs instructions
    Ok [ARM64Symbolic.BL (functionName ctx funcId)]

let internal emitTailCall (ctx: CodeGenContext) (funcId: AST.FunctionId) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Tail call: restore stack frame, then branch (no link)
    Ok (tailEpilogue ctx @ [ARM64Symbolic.B_label (functionName ctx funcId)])

let internal emitIndirectCall (ctx: CodeGenContext) (dest: LIR.Reg) (func: LIR.Reg) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Indirect call: call through function pointer in register
    // Use BLR instruction instead of BL
    lirRegToARM64Reg func
    |> Result.map (fun funcReg -> [ARM64Symbolic.BLR funcReg])

let internal emitIndirectTailCall (ctx: CodeGenContext) (func: LIR.Reg) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    // Indirect tail call: restore stack frame, then branch to register
    lirRegToARM64Reg func
    |> Result.map (fun funcReg ->
        tailEpilogue ctx @ [ARM64Symbolic.BR funcReg])

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
        tailEpilogue ctx @ [ARM64Symbolic.BR funcPtrReg])

// Save only live caller-saved registers. Adjacent stack slots permit STP/LDP
// even when the physical register numbers are not adjacent.
let private callSaveLayout (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) =
    let ints = intRegs |> List.distinct |> List.sort
    let floats = floatRegs |> List.distinct |> List.sort
    let floatBase = List.length ints * 8
    let size = ((floatBase + List.length floats * 8 + 15) / 16) * 16
    (ints, floats, floatBase, size)

let internal emitSaveRegs (_ctx: CodeGenContext) (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) : Result<ARM64Symbolic.Instr list, string> =
    let (ints, floats, floatBase, size) = callSaveLayout intRegs floatRegs
    let rec saveInts offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [ARM64Symbolic.STR (lirPhysRegToARM64Reg reg, ARM64Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            ARM64Symbolic.STP
                (lirPhysRegToARM64Reg first, lirPhysRegToARM64Reg second,
                 ARM64Symbolic.SP, int16 offset)
            :: saveInts (offset + 16) rest
    let rec saveFloats offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [ARM64Symbolic.STR_fp (lirPhysFPRegToARM64FReg reg, ARM64Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            ARM64Symbolic.STP_fp
                (lirPhysFPRegToARM64FReg first, lirPhysFPRegToARM64FReg second,
                 ARM64Symbolic.SP, int16 offset)
            :: saveFloats (offset + 16) rest
    if size = 0 then Ok []
    else
        Ok ([ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 size)]
            @ saveInts 0 ints @ saveFloats floatBase floats)

let internal emitRestoreRegs (_ctx: CodeGenContext) (intRegs: LIR.PhysReg list) (floatRegs: LIR.PhysFPReg list) : Result<ARM64Symbolic.Instr list, string> =
    let (ints, floats, floatBase, size) = callSaveLayout intRegs floatRegs
    let rec restoreInts offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [ARM64Symbolic.LDR (lirPhysRegToARM64Reg reg, ARM64Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            ARM64Symbolic.LDP
                (lirPhysRegToARM64Reg first, lirPhysRegToARM64Reg second,
                 ARM64Symbolic.SP, int16 offset)
            :: restoreInts (offset + 16) rest
    let rec restoreFloats offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [ARM64Symbolic.LDR_fp (lirPhysFPRegToARM64FReg reg, ARM64Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            ARM64Symbolic.LDP_fp
                (lirPhysFPRegToARM64FReg first, lirPhysFPRegToARM64FReg second,
                 ARM64Symbolic.SP, int16 offset)
            :: restoreFloats (offset + 16) rest
    if size = 0 then Ok []
    else
        Ok (restoreInts 0 ints @ restoreFloats floatBase floats
            @ [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 size)])

let internal emitLoadFuncAddr (ctx: CodeGenContext) (dest: LIR.Reg) (funcId: AST.FunctionId) : Result<ARM64Symbolic.Instr list, string> =
    // Load the address of a function into the destination register using ADR
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        [ARM64Symbolic.ADR (destReg, codeLabel (functionName ctx funcId))])
