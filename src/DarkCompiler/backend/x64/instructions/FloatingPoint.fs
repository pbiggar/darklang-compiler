// FloatingPoint.fs - Emit x64 instructions for floatingpoint operations.

module X64EmitFloatingPoint

open X64Operands
open X64CodeGenTypes
open X64FieldReferenceCounts
open X64InstructionContext

let internal emitFArgMoves (ctx: FuncCtx) (moves: (LIR.PhysFPReg * LIR.FReg) list) : Result<X86_64.Instr list, string> =
    // Float arguments are parallel moves: a source may be overwritten by an
    // earlier destination, so cycles are broken through a stack slot.
    let resolvedMoves =
        moves
        |> List.map (fun (destPhys, srcFreg) ->
            match srcFreg with
            | LIR.FPhysical srcPhys ->
                (lirFRegToX86 destPhys, lirFRegToX86 srcPhys)
            | LIR.FVirtual id ->
                Crash.crash $"Unresolved virtual float register f{id} in FArgMoves")

    let getSrcReg (srcReg: X86_64.FReg) : X86_64.FReg option = Some srcReg
    ParallelMoves.resolve resolvedMoves getSrcReg
    |> List.collect (function
        | ParallelMoves.SaveToTemp src ->
            [ X86_64.SUB_imm (X86_64.RSP, 16)
              X86_64.MOVSD_store (X86_64.RSP, 0, src) ]
        | ParallelMoves.Move (dest, src) ->
            [X86_64.MOVSD_reg (dest, src)]
        | ParallelMoves.MoveFromTemp dest ->
            [ X86_64.MOVSD_load (dest, X86_64.RSP, 0)
              X86_64.ADD_imm (X86_64.RSP, 16) ])
    |> Ok

let internal emitFPhi (ctx: FuncCtx) : Result<X86_64.Instr list, string> =
    Ok []

let internal emitFMov (ctx: FuncCtx) (dest: LIR.FReg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    // Register allocation represents a parallel-move cycle temp as f2000.
    // Keep that value on the stack so every XMM register remains allocatable.
    match dest, src with
    | LIR.FVirtual 2000, LIR.FPhysical srcPhys ->
        let s = lirFRegToX86 srcPhys
        Ok [ X86_64.SUB_imm (X86_64.RSP, 16)
             X86_64.MOVSD_store (X86_64.RSP, 0, s) ]
    | LIR.FPhysical destPhys, LIR.FVirtual 2000 ->
        let d = lirFRegToX86 destPhys
        Ok [ X86_64.MOVSD_load (d, X86_64.RSP, 0)
             X86_64.ADD_imm (X86_64.RSP, 16) ]
    | LIR.FPhysical destPhys, LIR.FPhysical srcPhys ->
        let d = lirFRegToX86 destPhys
        let s = lirFRegToX86 srcPhys
        Ok (if d = s then [] else [X86_64.MOVSD_reg (d, s)])
    | _ -> Error "FMov with unresolved virtual FP register"

let internal emitFLoad (ctx: FuncCtx) (dest: LIR.FReg) (value: float) : Result<X86_64.Instr list, string> =
    match dest with
    | LIR.FPhysical dp ->
        let d = lirFRegToX86 dp
        // Load float immediate via GP register
        let bits = System.BitConverter.DoubleToInt64Bits(value)
        Ok (loadImm64 scratch bits @ [X86_64.MOVQ_from_gp (d, scratch)])
    | _ -> Error "FLoad with virtual FP register"

let internal emitFSpillLoad (ctx: FuncCtx) (dest: LIR.FReg) (stackSlot: int) : Result<X86_64.Instr list, string> =
    match dest with
    | LIR.FPhysical destPhys ->
        Ok [X86_64.MOVSD_load (lirFRegToX86 destPhys, X86_64.RBP, int32 (adjustStackOffset ctx stackSlot))]
    | _ -> Error "FSpillLoad with virtual FP register"

let internal emitFSpillStore (ctx: FuncCtx) (stackSlot: int) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match src with
    | LIR.FPhysical srcPhys ->
        Ok [X86_64.MOVSD_store (X86_64.RBP, int32 (adjustStackOffset ctx stackSlot), lirFRegToX86 srcPhys)]
    | _ -> Error "FSpillStore with virtual FP register"

let internal emitFAdd (ctx: FuncCtx) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, left, right with
    | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
        let d = lirFRegToX86 dp
        let l = lirFRegToX86 lp
        let r = lirFRegToX86 rp
        if d = r && d <> l then
            Ok [X86_64.ADDSD (d, l)]  // commutative: swap operands
        else
            let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
            Ok (setup @ [X86_64.ADDSD (d, r)])
    | _ -> Error "FAdd with virtual FP register"

let internal emitFSub (ctx: FuncCtx) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, left, right with
    | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
        let d = lirFRegToX86 dp
        let l = lirFRegToX86 lp
        let r = lirFRegToX86 rp
        if d = r && d <> l then
            Ok (
                withPreservedFloatScratch [d; l; r] (fun temp ->
                    [ X86_64.MOVSD_reg (temp, l)
                      X86_64.SUBSD (temp, r)
                      X86_64.MOVSD_reg (d, temp) ]))
        else
            let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
            Ok (setup @ [X86_64.SUBSD (d, r)])
    | _ -> Error "FSub with virtual FP register"

let internal emitFMul (ctx: FuncCtx) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, left, right with
    | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
        let d = lirFRegToX86 dp
        let l = lirFRegToX86 lp
        let r = lirFRegToX86 rp
        if d = r && d <> l then
            Ok [X86_64.MULSD (d, l)]  // commutative: swap operands
        else
            let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
            Ok (setup @ [X86_64.MULSD (d, r)])
    | _ -> Error "FMul with virtual FP register"

let internal emitFDiv (ctx: FuncCtx) (dest: LIR.FReg) (left: LIR.FReg) (right: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, left, right with
    | LIR.FPhysical dp, LIR.FPhysical lp, LIR.FPhysical rp ->
        let d = lirFRegToX86 dp
        let l = lirFRegToX86 lp
        let r = lirFRegToX86 rp
        if d = r && d <> l then
            Ok (
                withPreservedFloatScratch [d; l; r] (fun temp ->
                    [ X86_64.MOVSD_reg (temp, l)
                      X86_64.DIVSD (temp, r)
                      X86_64.MOVSD_reg (d, temp) ]))
        else
            let setup = if d <> l then [X86_64.MOVSD_reg (d, l)] else []
            Ok (setup @ [X86_64.DIVSD (d, r)])
    | _ -> Error "FDiv with virtual FP register"

let internal emitFNeg (ctx: FuncCtx) (dest: LIR.FReg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, src with
    | LIR.FPhysical dp, LIR.FPhysical sp ->
        let d = lirFRegToX86 dp
        let s = lirFRegToX86 sp
        Ok (
            withPreservedFloatScratch [d; s] (fun temp ->
                loadImm64 scratch System.Int64.MinValue
                @ [ X86_64.MOVQ_from_gp (temp, scratch)
                    X86_64.MOVSD_reg (d, s)
                    X86_64.XORPD (d, temp) ]))
    | _ -> Error "FNeg with virtual FP register"

let internal emitFAbs (ctx: FuncCtx) (dest: LIR.FReg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, src with
    | LIR.FPhysical dp, LIR.FPhysical sp ->
        let d = lirFRegToX86 dp
        let s = lirFRegToX86 sp
        // Abs: clear sign bit using ANDPD with 0x7FFFFFFFFFFFFFFF mask.
        // We don't have ANDPD in our ISA, but we can use the GP trick:
        // 1. Move float to GP register
        // 2. AND with 0x7FFFFFFFFFFFFFFF
        // 3. Move back to float register
        // Move float bits to GP, AND with mask to clear sign bit, move back
        Ok ([X86_64.MOVQ_to_gp (scratch, s)]
            @ loadImm64 X86_64.RCX 0x7FFFFFFFFFFFFFFFL
            @ [X86_64.AND_reg (scratch, X86_64.RCX)
               X86_64.MOVQ_from_gp (d, scratch)])
    | _ -> Error "FAbs with virtual FP register"

let internal emitFSqrt (ctx: FuncCtx) (dest: LIR.FReg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match dest, src with
    | LIR.FPhysical dp, LIR.FPhysical sp ->
        Ok [X86_64.SQRTSD (lirFRegToX86 dp, lirFRegToX86 sp)]
    | _ -> Error "FSqrt with virtual FP register"

let internal emitFCmp (ctx: FuncCtx) (left: LIR.FReg) (right: LIR.FReg) : Result<X86_64.Instr list, string> =
    match left, right with
    | LIR.FPhysical lp, LIR.FPhysical rp ->
        Ok [X86_64.UCOMISD (lirFRegToX86 lp, lirFRegToX86 rp)]
    | _ -> Error "FCmp with virtual FP register"

let internal emitFloatToInt64 (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match src with
    | LIR.FPhysical sp ->
        resolveReg dest
        |> Result.map (fun destReg -> [X86_64.CVTTSD2SI (destReg, lirFRegToX86 sp)])
    | _ -> Error "FloatToInt64 with virtual FP register"

let internal emitFpToGp (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match src with
    | LIR.FPhysical sp ->
        resolveReg dest
        |> Result.map (fun destReg -> [X86_64.MOVQ_to_gp (destReg, lirFRegToX86 sp)])
    | _ -> Error "FpToGp with virtual FP register"

let internal emitFloatToBits (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    match src with
    | LIR.FPhysical sp ->
        resolveReg dest
        |> Result.map (fun destReg -> [X86_64.MOVQ_to_gp (destReg, lirFRegToX86 sp)])
    | _ -> Error "FloatToBits with virtual FP register"

let internal emitFloatToString (ctx: FuncCtx) (dest: LIR.Reg) (src: LIR.FReg) : Result<X86_64.Instr list, string> =
    // Call Stdlib.Float.toString(D0)
    match src with
    | LIR.FPhysical fp ->
        let xmm = lirFRegToX86 fp
        resolveReg dest
        |> Result.map (fun destReg ->
            (if xmm <> X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0, xmm)] else [])
            @ [X86_64.CALL "Stdlib.Float.toString"]
            @ (if destReg <> X86_64.RAX then [X86_64.MOV_reg (destReg, X86_64.RAX)] else []))
    | _ -> Error "FloatToString with virtual FP register"
