(*
   Calls.fs - Emit arm64 instructions for calls operations.
*)
[@@@warning "-4"]
let distinct xs=let _,ys=List.fold_left (fun (seen,ys) x -> if List.mem x seen then seen,ys else x::seen,x::ys) ([],[]) xs in List.rev ys
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low



open ARM64CodeGenTypes
open HeapAllocation
open ARM64Operands
open ARM64Frames

let tailEpilogue (ctx: codeGenContext) =
    generateEpilogue ctx.ARM64CodeGenTypes.usedCalleeSaved ctx.ARM64CodeGenTypes.usedCalleeSavedF ctx.ARM64CodeGenTypes.stackSize
    |> List.filter (function Symbolic.RET -> false | _ -> true)

(*
   Function call: arguments already moved to X0-X7 by preceding MOVs
   Caller-save is handled by SaveRegs/RestoreRegs instructions
*)
let emitCall (ctx: codeGenContext) (_dest: LIR.reg) (funcId: AST.functionId) (_args: LIR.operand list) =


    Ok [Symbolic.BL (functionName ctx funcId)]

(*
   Tail call: restore stack frame, then branch (no link)
*)
let emitTailCall (ctx: codeGenContext) (funcId: AST.functionId) (_args: LIR.operand list) =

    Ok (tailEpilogue ctx @ [Symbolic.B_label (functionName ctx funcId)])

(*
   Indirect call: call through function pointer in register
   Use BLR instruction instead of BL
*)
let emitIndirectCall (_ctx: codeGenContext) (_dest: LIR.reg) (func: LIR.reg) (_args: LIR.operand list) =


    lirRegToARM64Reg func
    |> Result.map (fun funcReg -> [Symbolic.BLR funcReg])

(*
   Indirect tail call: restore stack frame, then branch to register
*)
let emitIndirectTailCall (ctx: codeGenContext) (func: LIR.reg) (_args: LIR.operand list) =

    lirRegToARM64Reg func
    |> Result.map (fun funcReg ->
        tailEpilogue ctx @ [Symbolic.BR funcReg])

(*
   Call through closure - MIR_to_LIR already set up:
   - X9: function pointer (loaded from closure[0])
   - X0: closure
   - X1-X7: args
   Just do the BLR
*)
let emitClosureCall (_ctx: codeGenContext) (_dest: LIR.reg) (funcPtr: LIR.reg) (_args: LIR.operand list) =





    lirRegToARM64Reg funcPtr
    |> Result.map (fun funcPtrReg ->
        [Symbolic.BLR funcPtrReg])

(*
   Closure tail call: restore stack frame, then branch to register
*)
let emitClosureTailCall (ctx: codeGenContext) (funcPtr: LIR.reg) (_args: LIR.operand list) =

    lirRegToARM64Reg funcPtr
    |> Result.map (fun funcPtrReg ->
        tailEpilogue ctx @ [Symbolic.BR funcPtrReg])



(*
   Save only live caller-saved registers. Adjacent stack slots permit STP/LDP
   even when the physical register numbers are not adjacent.
*)
let callSaveLayout (intRegs: LIR.physReg list) (floatRegs: LIR.physFPReg list) =
    let ints = intRegs |> distinct |> List.sort Stdlib.compare
    in
    let floats = floatRegs |> distinct |> List.sort Stdlib.compare
    in
    let floatBase = mul (List.length ints) 8
    in
    let size = mul ((add (add floatBase (mul (List.length floats) 8)) 15) / 16) 16
    in
    (ints, floats, floatBase, size)

let emitSaveRegs (__ctx: codeGenContext) (intRegs: LIR.physReg list) (floatRegs: LIR.physFPReg list) =
    let (ints, floats, floatBase, size) = callSaveLayout intRegs floatRegs
    in
    let rec saveInts offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [Symbolic.STR (lirPhysRegToARM64Reg reg, Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            Symbolic.STP
                (lirPhysRegToARM64Reg first, lirPhysRegToARM64Reg second,
                 Symbolic.SP, int16 offset)
            :: saveInts (add offset 16) rest
    in
    let rec saveFloats offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [Symbolic.STR_fp (lirPhysFPRegToARM64FReg reg, Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            Symbolic.STP_fp
                (lirPhysFPRegToARM64FReg first, lirPhysFPRegToARM64FReg second,
                 Symbolic.SP, int16 offset)
            :: saveFloats (add offset 16) rest
    in
    if size = 0 then Ok []
    else
        Ok ([Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, (size land 65535))]
            @ saveInts 0 ints @ saveFloats floatBase floats)

let emitRestoreRegs (__ctx: codeGenContext) (intRegs: LIR.physReg list) (floatRegs: LIR.physFPReg list) =
    let (ints, floats, floatBase, size) = callSaveLayout intRegs floatRegs
    in
    let rec restoreInts offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [Symbolic.LDR (lirPhysRegToARM64Reg reg, Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            Symbolic.LDP
                (lirPhysRegToARM64Reg first, lirPhysRegToARM64Reg second,
                 Symbolic.SP, int16 offset)
            :: restoreInts (add offset 16) rest
    in
    let rec restoreFloats offset regs =
        match regs with
        | [] -> []
        | [reg] ->
            [Symbolic.LDR_fp (lirPhysFPRegToARM64FReg reg, Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            Symbolic.LDP_fp
                (lirPhysFPRegToARM64FReg first, lirPhysFPRegToARM64FReg second,
                 Symbolic.SP, int16 offset)
            :: restoreFloats (add offset 16) rest
    in
    if size = 0 then Ok []
    else
        Ok (restoreInts 0 ints @ restoreFloats floatBase floats
            @ [Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, (size land 65535))])

(*
   Load the address of a function into the destination register using ADR
*)
let emitLoadFuncAddr (ctx: codeGenContext) (dest: LIR.reg) (funcId: AST.functionId) =

    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        [Symbolic.ADR (destReg, codeLabel (functionName ctx funcId))])
