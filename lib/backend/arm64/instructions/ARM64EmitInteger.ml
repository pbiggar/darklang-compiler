(*
   Integer.fs - Emit arm64 instructions for integer operations.
*)
[@@@warning "-4"]
let bind f value=Result.bind value f
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low



open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(*
   Phi nodes should be eliminated before code generation (by register allocation)
*)
let emitPhi (_ctx: codeGenContext) =

    Error "Phi nodes should be eliminated before code generation"

(*
   Skip self-moves (can happen after register allocation coalesces VRegs)
   Load from stack slot into destination register
   Load function address using ADR instruction
*)
let emitMov (ctx: codeGenContext) (dest: LIR.reg) (src: LIR.operand) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        match src with
        | LIR.Imm value ->
            Ok (loadImmediate destReg value)
        | LIR.FloatImm _ ->
            Error "Float code generation not yet implemented"
        | LIR.Reg srcReg ->
            lirRegToARM64Reg srcReg
            |> Result.map (fun srcARM64 ->

                if destReg = srcARM64 then []
                else [Symbolic.MOV_reg (destReg, srcARM64)])
        | LIR.StackSlot offset ->

            loadStackSlot destReg offset
        | LIR.StringSymbol value ->
            Ok (loadStringLiteralPointer destReg value)
        | LIR.FloatSymbol _ ->
            Error "Cannot MOV float reference - use FLoad instruction"
        | LIR.FuncAddr funcName ->

            Ok [Symbolic.ADR (destReg, codeLabel (functionName ctx funcName))])

(*
   Store register to stack slot
*)
let emitStore (_ctx: codeGenContext) (offset: int) (src: LIR.reg) =

    lirRegToARM64Reg src
    |> bind (fun srcReg -> storeStackSlot srcReg offset)

(*
   Can use immediate ADD
   Need to load immediate into register first
   Use X9 as temp
   Load stack slot into temp register, then add
*)
let emitAdd (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.operand) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->

                Ok [Symbolic.ADD_imm (destReg, leftReg, (Int64.to_int value land 65535))]
            | LIR.Imm value ->

                let tempReg = Symbolic.X9
                in
                Ok (loadImmediate tempReg value @ [Symbolic.ADD_reg (destReg, leftReg, tempReg)])
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [Symbolic.ADD_reg (destReg, leftReg, rightARM64)])
            | LIR.StackSlot offset ->

                let tempReg = Symbolic.X9
                in
                loadStackSlot tempReg offset
                |> Result.map (fun loadInstrs -> loadInstrs @ [Symbolic.ADD_reg (destReg, leftReg, tempReg)])
            | LIR.StringSymbol _ ->
                Error "Cannot use string reference in arithmetic operation"
            | LIR.FloatSymbol _ ->
                Error "Cannot use float reference in integer arithmetic"
            | LIR.FuncAddr _ ->
                Error "Cannot use function address in arithmetic operation"))

(*
   Load stack slot into temp register, then subtract
*)
let emitSub (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.operand) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->
                Ok [Symbolic.SUB_imm (destReg, leftReg, (Int64.to_int value land 65535))]
            | LIR.Imm value ->
                let tempReg = Symbolic.X9
                in
                Ok (loadImmediate tempReg value @ [Symbolic.SUB_reg (destReg, leftReg, tempReg)])
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [Symbolic.SUB_reg (destReg, leftReg, rightARM64)])
            | LIR.StackSlot offset ->

                let tempReg = Symbolic.X9
                in
                loadStackSlot tempReg offset
                |> Result.map (fun loadInstrs -> loadInstrs @ [Symbolic.SUB_reg (destReg, leftReg, tempReg)])
            | LIR.StringSymbol _ ->
                Error "Cannot use string reference in arithmetic operation"
            | LIR.FloatSymbol _ ->
                Error "Cannot use float reference in integer arithmetic"
            | LIR.FuncAddr _ ->
                Error "Cannot use function address in arithmetic operation"))

let emitMul (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [Symbolic.MUL (destReg, leftReg, rightReg)])))

let emitSdiv (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [Symbolic.SDIV (destReg, leftReg, rightReg)])))

let emitUdiv (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [Symbolic.UDIV (destReg, leftReg, rightReg)])))

(*
   MSUB: dest = sub - mulLeft * mulRight
*)
let emitMsub (_ctx: codeGenContext) (dest: LIR.reg) (mulLeft: LIR.reg) (mulRight: LIR.reg) (sub: LIR.reg) =

    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg mulLeft
        |> bind (fun mulLeftReg ->
            lirRegToARM64Reg mulRight
            |> bind (fun mulRightReg ->
                lirRegToARM64Reg sub
                |> Result.map (fun subReg ->
                    [Symbolic.MSUB (destReg, mulLeftReg, mulRightReg, subReg)]))))

(*
   MADD: dest = add + mulLeft * mulRight
*)
let emitMadd (_ctx: codeGenContext) (dest: LIR.reg) (mulLeft: LIR.reg) (mulRight: LIR.reg) (add: LIR.reg) =

    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg mulLeft
        |> bind (fun mulLeftReg ->
            lirRegToARM64Reg mulRight
            |> bind (fun mulRightReg ->
                lirRegToARM64Reg add
                |> Result.map (fun addReg ->
                    [Symbolic.MADD (destReg, mulLeftReg, mulRightReg, addReg)]))))

(*
   Load stack slot into temp register, then compare
*)
let emitCmp (_ctx: codeGenContext) (left: LIR.reg) (right: LIR.operand) =
    lirRegToARM64Reg left
    |> bind (fun leftReg ->
        match right with
        | LIR.Imm value when value >= 0L && value < 4096L ->
            Ok [Symbolic.CMP_imm (leftReg, (Int64.to_int value land 65535))]
        | LIR.Imm value ->
            let tempReg = Symbolic.X9
            in
            Ok (loadImmediate tempReg value @ [Symbolic.CMP_reg (leftReg, tempReg)])
        | LIR.FloatImm _ ->
            Error "Float code generation not yet implemented"
        | LIR.Reg rightReg ->
            lirRegToARM64Reg rightReg
            |> Result.map (fun rightARM64 -> [Symbolic.CMP_reg (leftReg, rightARM64)])
        | LIR.StackSlot offset ->

            let tempReg = Symbolic.X9
            in
            loadStackSlot tempReg offset
            |> Result.map (fun loadInstrs -> loadInstrs @ [Symbolic.CMP_reg (leftReg, tempReg)])
        | LIR.StringSymbol _ ->
            Error "Cannot compare string references directly"
        | LIR.FloatSymbol _ ->
            Error "Cannot compare float references directly - use FCmp"
        | LIR.FuncAddr _ ->
            Error "Cannot compare function addresses directly")

let emitCset (_ctx: codeGenContext) (dest: LIR.reg) (cond: LIR.condition) =
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        let arm64Cond =
            match cond with
            | LIR.EQ -> Symbolic.EQ
            | LIR.NE -> Symbolic.NE
            | LIR.LT -> Symbolic.LT
            | LIR.GT -> Symbolic.GT
            | LIR.LE -> Symbolic.LE
            | LIR.GE -> Symbolic.GE
            | LIR.ULT -> Symbolic.LO
            | LIR.UGT -> Symbolic.HI
            | LIR.ULE -> Symbolic.LS
            | LIR.UGE -> Symbolic.HS
        in
        [Symbolic.CSET (destReg, arm64Cond)])

let emitSelect (_ctx: codeGenContext) (dest: LIR.reg) (whenTrue: LIR.reg) (whenFalse: LIR.reg) (cond: LIR.condition) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg whenTrue
        |> bind (fun trueReg ->
            lirRegToARM64Reg whenFalse
            |> Result.map (fun falseReg ->
                let arm64Cond =
                    match cond with
                    | LIR.EQ -> Symbolic.EQ | LIR.NE -> Symbolic.NE
                    | LIR.LT -> Symbolic.LT | LIR.GT -> Symbolic.GT
                    | LIR.LE -> Symbolic.LE | LIR.GE -> Symbolic.GE
                    | LIR.ULT -> Symbolic.LO | LIR.UGT -> Symbolic.HI
                    | LIR.ULE -> Symbolic.LS | LIR.UGE -> Symbolic.HS
                in
                [Symbolic.CSEL (destReg, trueReg, falseReg, arm64Cond)])))

let emitAnd (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [Symbolic.AND_reg (destReg, leftReg, rightReg)])))

let emitAnd_imm (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (imm: int64) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.AND_imm (destReg, srcReg, imm)]))

let emitOrr (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [Symbolic.ORR_reg (destReg, leftReg, rightReg)])))

let emitEor (_ctx: codeGenContext) (dest: LIR.reg) (left: LIR.reg) (right: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg left
        |> bind (fun leftReg ->
            lirRegToARM64Reg right
            |> Result.map (fun rightReg -> [Symbolic.EOR_reg (destReg, leftReg, rightReg)])))

let emitLsl (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (shift: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> bind (fun srcReg ->
            lirRegToARM64Reg shift
            |> Result.map (fun shiftReg -> [Symbolic.LSL_reg (destReg, srcReg, shiftReg)])))

let emitLsr (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (shift: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> bind (fun srcReg ->
            lirRegToARM64Reg shift
            |> Result.map (fun shiftReg -> [Symbolic.LSR_reg (destReg, srcReg, shiftReg)])))

let emitAsr (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (shift: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> bind (fun srcReg ->
            lirRegToARM64Reg shift
            |> Result.map (fun shiftReg -> [Symbolic.ASR_reg (destReg, srcReg, shiftReg)])))

let emitLsl_imm (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (shift: int) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.LSL_imm (destReg, srcReg, shift)]))

let emitLsr_imm (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (shift: int) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.LSR_imm (destReg, srcReg, shift)]))

let emitAsr_imm (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) (shift: int) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.ASR_imm (destReg, srcReg, shift)]))

let emitNeg (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.NEG (destReg, srcReg)]))

let emitMvn (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.MVN (destReg, srcReg)]))



(*
   Sign/zero extension for integer overflow truncation
*)
let emitSxtb (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.SXTB (destReg, srcReg)]))

let emitSxth (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.SXTH (destReg, srcReg)]))

let emitSxtw (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.SXTW (destReg, srcReg)]))

let emitUxtb (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.UXTB (destReg, srcReg)]))

let emitUxth (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.UXTH (destReg, srcReg)]))

let emitUxtw (_ctx: codeGenContext) (dest: LIR.reg) (src: LIR.reg) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.UXTW (destReg, srcReg)]))

(*
   Allocate closure on heap: (func_ptr, cap1, cap2, ...)
   Each slot is 8 bytes
   func_ptr + captures
   Total size includes 8 bytes for ref count, aligned to 8 bytes
   Allocate using bump allocator
   dest = current heap pointer
   X15 = 1 (initial ref count)
   store ref count after payload
   bump pointer
   Store function address at offset 0
   X15 = function address
   [dest] = func_ptr
   Store captures at subsequent offsets
   Avoid storing dest into itself at offset
*)
let emitClosureAlloc (ctx: codeGenContext) (dest: LIR.reg) (funcId: AST.functionId) (captures: LIR.operand list) =
    let funcName = functionName ctx funcId


    in
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        let numSlots = add 1 (List.length captures)
        in
        let sizeBytes = mul numSlots 8

        in
        let totalSize = (add (add sizeBytes 8) 7) land (lnot 7)


        in
        let allocInstrs = [
            Symbolic.MOV_reg (destReg, Symbolic.X28);
            Symbolic.MOVZ (Symbolic.X15, 1, 0);
            Symbolic.STR (Symbolic.X15, Symbolic.X28, int16 sizeBytes);
            Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, (totalSize land 65535))
        ]
        in


        let storeFuncAddr = [
            Symbolic.ADR (Symbolic.X15, codeLabel funcName);
            Symbolic.STR (Symbolic.X15, destReg, 0)
        ]
        in


        let storeCaptures =
            captures
            |> List.mapi (fun i cap -> (i, cap))
            |> List.concat_map (fun (i, cap) ->
                let offset = mul (add i 1) 8
                in
                match cap with
                | LIR.Imm value ->
                    loadImmediate Symbolic.X15 value @
                    [Symbolic.STR (Symbolic.X15, destReg, int16 offset)]
                | LIR.Reg reg ->
                    (match lirRegToARM64Reg reg with
                    | Ok srcReg ->

                        if srcReg = destReg then
                            [Symbolic.MOV_reg (Symbolic.X15, srcReg); Symbolic.STR (Symbolic.X15, destReg, int16 offset)]
                        else
                            [Symbolic.STR (srcReg, destReg, int16 offset)]
                    | Error msg -> Crash.crash ("ClosureAlloc: lirRegToARM64Reg failed: " ^ msg))
                | LIR.FuncAddr fname ->
                    [Symbolic.ADR (Symbolic.X15, codeLabel (functionName ctx fname)); Symbolic.STR (Symbolic.X15, destReg, int16 offset)]
                | _ -> Crash.crash "ClosureAlloc: Unexpected capture operand type")

        in
        Ok (allocInstrs @ generateLeakCounterInc ctx @ storeFuncAddr @ storeCaptures))

(*
   Resolve call arguments without relying on caller-save stack offsets.
   X16 is reserved from allocation and breaks register cycles.
   Helper to get source register if operand is a physical register
   Generate a single move instruction (for non-register sources)
   Use the shared parallel move resolution algorithm
   Convert actions to ARM64 instructions
   Save register to X16 (temp)
   Move from X16 (temp) to destination
*)
let emitTailArgMoves (ctx: codeGenContext) (moves: (LIR.physReg * LIR.operand) list) =




    let getSrcPhysReg (srcOp: LIR.operand) =
        match srcOp with
        | LIR.Reg (LIR.Physical srcPhysReg) -> Some srcPhysReg
        | _ -> None


    in
    let generateMoveInstr ((destReg, srcOp): LIR.physReg * LIR.operand) =
        let destARM64 = lirPhysRegToARM64Reg destReg
        in
        match srcOp with
        | LIR.Imm value ->
            Ok (loadImmediate destARM64 value)
        | LIR.Reg (LIR.Physical srcPhysReg) ->
            let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
            in
            Ok [Symbolic.MOV_reg (destARM64, srcARM64)]
        | LIR.Reg (LIR.Virtual _) ->
            Error "Virtual register in TailArgMoves - should have been allocated"
        | LIR.StackSlot offset ->
            loadStackSlot destARM64 offset
        | LIR.FuncAddr funcName ->
            Ok [Symbolic.ADR (destARM64, codeLabel (functionName ctx funcName))]
        | LIR.StringSymbol value ->
            Ok (loadStringLiteralPointer destARM64 value)
        | LIR.FloatImm _ | LIR.FloatSymbol _ ->
            Error "Float in TailArgMoves not yet supported"


    in
    let actions = ParallelMoves.resolve moves getSrcPhysReg


    in
    actions
    |> ResultList.mapResults (function
        | ParallelMoves.SaveToTemp reg ->

            Ok [Symbolic.MOV_reg (Symbolic.X16, lirPhysRegToARM64Reg reg)]
        | ParallelMoves.Move (dest, src) ->
            generateMoveInstr (dest, src)
        | ParallelMoves.MoveFromTemp dest ->

            Ok [Symbolic.MOV_reg (lirPhysRegToARM64Reg dest, Symbolic.X16)])
    |> Result.map List.concat

let emitArgMoves (ctx: codeGenContext) (moves: (LIR.physReg * LIR.operand) list) =
    emitTailArgMoves ctx moves

(*
   Exit program with code 0
*)
let emitExit (ctx: codeGenContext) =

    Ok (runtimeInstrs (PrintAndExit.generateExit ctx.target))

let emitStdoutWrite (ctx: codeGenContext) (effectId: int) (value: LIR.operand) (appendNewline: bool) =
    let syscalls = ARM64.targetSyscalls ctx.target
    in
    let label suffix = Printf.sprintf "__presentation_%s_%d_%s_%s" ctx.functionName effectId ctx.instructionSite suffix
    in
    let writeLoop prefix =
        let loopLabel = label (prefix ^ "_write")
        in
        let retryLabel = label (prefix ^ "_retry")
        in
        let errorLabel = label (prefix ^ "_error")
        in
        let doneLabel = label (prefix ^ "_done")
        in
        let resultCheck =
            match ARM64.targetOS ctx.target with
            | Platform.MacOS ->
                [ Symbolic.B_cond_label (Symbolic.HS, errorLabel);
                  Symbolic.B_label (prefix ^ "_success");
                  Symbolic.Label errorLabel;
                  Symbolic.CMP_imm (Symbolic.X0, 4);
                  Symbolic.B_cond_label (Symbolic.EQ, retryLabel);
                  Symbolic.B_label doneLabel;
                  Symbolic.Label (prefix ^ "_success") ]
            | Platform.Linux ->
                loadImmediate Symbolic.X12 (-4L)
                @ [ Symbolic.CMP_reg (Symbolic.X0, Symbolic.X12);
                    Symbolic.B_cond_label (Symbolic.EQ, retryLabel);
                    Symbolic.TBNZ_label (Symbolic.X0, 63, doneLabel) ]
        in
        [ Symbolic.Label loopLabel;
          Symbolic.CBZ (Symbolic.X2, doneLabel);
          Symbolic.Label retryLabel;
          Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.write, 0);
          Symbolic.SVC syscalls.ARM64.svcImmediate ]
        @ resultCheck
        @ [ Symbolic.CBZ (Symbolic.X0, doneLabel);
            Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, Symbolic.X0);
            Symbolic.SUB_reg (Symbolic.X2, Symbolic.X2, Symbolic.X0);
            Symbolic.B_label loopLabel;
            Symbolic.Label doneLabel ]

    in
    let setupValue =
        match value with
        | LIR.Reg reg ->
            lirRegToARM64Reg reg
            |> Result.map (fun src ->
                [ Symbolic.MOV_reg (Symbolic.X9, src);
                  Symbolic.LDR (Symbolic.X2, Symbolic.X9, 8);
                  Symbolic.ADD_imm (Symbolic.X1, Symbolic.X9, 16) ])
        | LIR.StackSlot offset ->
            loadStackSlot Symbolic.X9 offset
            |> Result.map (fun load ->
                load
                @ [ Symbolic.LDR (Symbolic.X2, Symbolic.X9, 8);
                    Symbolic.ADD_imm (Symbolic.X1, Symbolic.X9, 16) ])
        | LIR.StringSymbol text ->
            Ok (loadStringLiteralPointer Symbolic.X9 text
                @ [ Symbolic.LDR (Symbolic.X2, Symbolic.X9, 8);
                    Symbolic.ADD_imm (Symbolic.X1, Symbolic.X9, 16) ])
        | _ -> Error "StdoutWrite requires a String operand"

    in
    setupValue
    |> Result.map (fun setup ->
        let savedRegs =
            [ Symbolic.X0; Symbolic.X1; Symbolic.X2; Symbolic.X3;
              Symbolic.X4; Symbolic.X5; Symbolic.X6; Symbolic.X7;
              Symbolic.X8; Symbolic.X9; Symbolic.X10; Symbolic.X11;
              Symbolic.X12; Symbolic.X13; Symbolic.X14; Symbolic.X15;
              Symbolic.X16; Symbolic.X17 ]
        in
        let save =
            [ Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 160) ]
            @ (savedRegs |> List.mapi (fun i reg -> Symbolic.STR (reg, Symbolic.SP, int16 (mul i 8))))
        in
        let restore =
            (savedRegs |> List.mapi (fun i reg -> Symbolic.LDR (reg, Symbolic.SP, int16 (mul i 8))))
            @ [ Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 160) ]
        in
        let newline =
            if not appendNewline then []
            else
                [ Symbolic.MOVZ (Symbolic.X9, 10, 0);
                  Symbolic.STRB (Symbolic.X9, Symbolic.SP, 144);
                  Symbolic.MOVZ (Symbolic.X0, 1, 0);
                  Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 144);
                  Symbolic.MOVZ (Symbolic.X2, 1, 0) ]
                @ writeLoop "stdout_newline"
        in
        save
        @ setup
        @ [ Symbolic.MOVZ (Symbolic.X0, 1, 0) ]
        @ writeLoop "stdout"
        @ newline
        @ restore)

let emitStdinReadLine (ctx: codeGenContext) (effectId: int) (dest: LIR.reg) =
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        let syscalls = ARM64.targetSyscalls ctx.target
        in
        let label suffix = Printf.sprintf "__presentation_%s_%d_%s_%s" ctx.functionName effectId ctx.instructionSite suffix
        in
        let readLabel = label "stdin_read"
        in
        let retryLabel = label "stdin_retry"
        in
        let gotByteLabel = label "stdin_byte"
        in
        let finishLabel = label "stdin_finish"
        in
        let noCrLabel = label "stdin_no_cr"
        in
        let readResultCheck =
            match ARM64.targetOS ctx.target with
            | Platform.MacOS ->
                let errorLabel = label "stdin_error"
                in
                [ Symbolic.B_cond_label (Symbolic.HS, errorLabel);
                  Symbolic.CMP_imm (Symbolic.X0, 1);
                  Symbolic.B_cond_label (Symbolic.EQ, gotByteLabel);
                  Symbolic.B_label finishLabel;
                  Symbolic.Label errorLabel;
                  Symbolic.CMP_imm (Symbolic.X0, 4);
                  Symbolic.B_cond_label (Symbolic.EQ, retryLabel);
                  Symbolic.B_label finishLabel ]
            | Platform.Linux ->
                loadImmediate Symbolic.X12 (-4L)
                @ [ Symbolic.CMP_reg (Symbolic.X0, Symbolic.X12);
                    Symbolic.B_cond_label (Symbolic.EQ, retryLabel);
                    Symbolic.CMP_imm (Symbolic.X0, 1);
                    Symbolic.B_cond_label (Symbolic.EQ, gotByteLabel);
                    Symbolic.B_label finishLabel ]
        in
        let savedRegs =
            [ Symbolic.X0; Symbolic.X1; Symbolic.X2; Symbolic.X3;
              Symbolic.X4; Symbolic.X5; Symbolic.X6; Symbolic.X7;
              Symbolic.X8; Symbolic.X9; Symbolic.X10; Symbolic.X11;
              Symbolic.X12; Symbolic.X13; Symbolic.X14; Symbolic.X15;
              Symbolic.X16; Symbolic.X17 ]
        in
        [ Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 160) ]
        @ (savedRegs |> List.mapi (fun i reg -> Symbolic.STR (reg, Symbolic.SP, int16 (mul i 8))))
        @ [ Symbolic.MOVZ (Symbolic.X10, 0, 0);
            Symbolic.STR (Symbolic.X10, Symbolic.SP, 144);
            Symbolic.Label readLabel;
            Symbolic.LDR (Symbolic.X10, Symbolic.SP, 144);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.X28, 16);
            Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, Symbolic.X10);
            Symbolic.MOVZ (Symbolic.X0, 0, 0);
            Symbolic.MOVZ (Symbolic.X2, 1, 0);
            Symbolic.Label retryLabel;
            Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.read, 0);
            Symbolic.SVC syscalls.ARM64.svcImmediate ]
        @ readResultCheck
        @ [ Symbolic.Label gotByteLabel;
            Symbolic.LDRB_imm (Symbolic.X11, Symbolic.X1, 0);
            Symbolic.CMP_imm (Symbolic.X11, 10);
            Symbolic.B_cond_label (Symbolic.EQ, finishLabel);
            Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
            Symbolic.STR (Symbolic.X10, Symbolic.SP, 144);
            Symbolic.B_label readLabel;
            Symbolic.Label finishLabel;
            Symbolic.LDR (Symbolic.X10, Symbolic.SP, 144);
            Symbolic.CBZ (Symbolic.X10, noCrLabel);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.X28, 15);
            Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, Symbolic.X10);
            Symbolic.LDRB_imm (Symbolic.X11, Symbolic.X1, 0);
            Symbolic.CMP_imm (Symbolic.X11, 13);
            Symbolic.B_cond_label (Symbolic.NE, noCrLabel);
            Symbolic.SUB_imm (Symbolic.X10, Symbolic.X10, 1);
            Symbolic.Label noCrLabel;
            Symbolic.MOVZ (Symbolic.X0, 1, 0);
            Symbolic.STR (Symbolic.X0, Symbolic.X28, 0);
            Symbolic.STR (Symbolic.X10, Symbolic.X28, 8);
            Symbolic.ADD_imm (Symbolic.X11, Symbolic.X10, 7);
            Symbolic.LSR_imm (Symbolic.X11, Symbolic.X11, 3);
            Symbolic.LSL_imm (Symbolic.X11, Symbolic.X11, 3);
            Symbolic.STR (Symbolic.X28, Symbolic.SP, 152);
            Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 16);
            Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X11) ]
        @ generateLeakCounterInc ctx
        @ (savedRegs |> List.mapi (fun i reg -> Symbolic.LDR (reg, Symbolic.SP, int16 (mul i 8))))
        @ [ Symbolic.LDR (destReg, Symbolic.SP, 152);
            Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 160) ])

let emitRuntimeError (_ctx: codeGenContext) (message: string) =
    Ok (
        loadStringLiteralPointer Symbolic.X0 message
        @ [ Symbolic.MOVZ (Symbolic.X3, 0, 0);
            Symbolic.B_label runtimeErrorHelperLabel ])

let emitRuntimeErrorString (_ctx: codeGenContext) (messageReg: LIR.reg) =
    lirRegToARM64Reg messageReg
    |> Result.map (fun resolvedMessageReg ->
        (if resolvedMessageReg = Symbolic.X0 then
             []
         else
             [Symbolic.MOV_reg (Symbolic.X0, resolvedMessageReg)])
        @ [ Symbolic.MOVZ (Symbolic.X3, 1, 0);
            Symbolic.B_label runtimeErrorHelperLabel ])



(*
   Floating-point instructions
*)
let emitInt64ToFloat (_ctx: codeGenContext) (dest: LIR.fReg) (src: LIR.reg) =
    lirFRegToARM64FReg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.SCVTF (destReg, srcReg)]))

(*
   Move bits from GP register to FP register (for floats loaded from heap)
*)
let emitGpToFp (_ctx: codeGenContext) (dest: LIR.fReg) (src: LIR.reg) =

    lirFRegToARM64FReg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> [Symbolic.FMOV_from_gp (destReg, srcReg)]))
