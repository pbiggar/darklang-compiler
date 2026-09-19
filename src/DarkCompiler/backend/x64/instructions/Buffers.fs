// Buffers.fs - Emit x64 instructions for buffers operations.

module X64EmitBuffers

open X64Operands
open X64CodeGenTypes
open X64FieldReferenceCounts
open X64InstructionContext

let internal emitCanonicalBufferEq (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Operand) (right: LIR.Operand) : Result<X86_64.Instr list, string> =
    // Canonical buffers share [refcount:8][length:8][data:N]. Compare the
    // representation directly without allocating or calling stdlib code.
    resolveReg dest
    |> Result.bind (fun destReg ->
        let leftReg = X86_64.RDI
        let rightReg = X86_64.RSI
        let remainingReg = X86_64.RCX
        let leftWordReg = X86_64.R8
        let rightWordReg = X86_64.R9
        let byteReg = X86_64.R10
        let savedRegs = [leftReg; rightReg; remainingReg; leftWordReg; rightWordReg; byteReg]
        let saveInstrs = savedRegs |> List.map X86_64.PUSH
        let restoreInstrs = savedRegs |> List.rev |> List.map X86_64.POP

        let materializeLiteral target value =
            emitStringLiteralNoRefCount target value

        let prepareOperands =
            match left, right with
            | LIR.Reg left, LIR.Reg right ->
                resolveReg left
                |> Result.bind (fun sourceLeft ->
                    resolveReg right
                    |> Result.map (fun sourceRight ->
                        [X86_64.PUSH sourceLeft
                         X86_64.PUSH sourceRight
                         X86_64.POP rightReg
                         X86_64.POP leftReg]))
            | LIR.Reg left, LIR.StringSymbol right ->
                resolveReg left
                |> Result.map (fun sourceLeft ->
                    [X86_64.PUSH sourceLeft]
                    @ materializeLiteral rightReg right
                    @ [X86_64.POP leftReg])
            | LIR.StringSymbol left, LIR.Reg right ->
                resolveReg right
                |> Result.map (fun sourceRight ->
                    [X86_64.PUSH sourceRight]
                    @ materializeLiteral leftReg left
                    @ [X86_64.POP rightReg])
            | LIR.StringSymbol left, LIR.StringSymbol right ->
                Ok (materializeLiteral leftReg left @ materializeLiteral rightReg right)
            | _ -> Error "CanonicalBufferEq requires StringSymbol or Reg operands"

        let wordLoop = freshLabel "canonical_eq_words"
        let byteLoop = freshLabel "canonical_eq_bytes"
        let equalLabel = freshLabel "canonical_eq_equal"
        let unequalLabel = freshLabel "canonical_eq_unequal"
        let doneLabel = freshLabel "canonical_eq_done"

        prepareOperands
        |> Result.map (fun operandInstrs ->
            saveInstrs
            @ operandInstrs
            @ [X86_64.CMP_reg (leftReg, rightReg)
               X86_64.Jcc (X86_64.EQ, equalLabel)
               X86_64.MOV_load (remainingReg, leftReg, 8)
               X86_64.MOV_load (rightWordReg, rightReg, 8)
               X86_64.CMP_reg (remainingReg, rightWordReg)
               X86_64.Jcc (X86_64.NE, unequalLabel)
               X86_64.ADD_imm (leftReg, 16)
               X86_64.ADD_imm (rightReg, 16)
               X86_64.Label wordLoop
               X86_64.CMP_imm (remainingReg, 8)
               X86_64.Jcc (X86_64.LT, byteLoop)
               X86_64.MOV_load (leftWordReg, leftReg, 0)
               X86_64.MOV_load (rightWordReg, rightReg, 0)
               X86_64.CMP_reg (leftWordReg, rightWordReg)
               X86_64.Jcc (X86_64.NE, unequalLabel)
               X86_64.ADD_imm (leftReg, 8)
               X86_64.ADD_imm (rightReg, 8)
               X86_64.SUB_imm (remainingReg, 8)
               X86_64.JMP wordLoop
               X86_64.Label byteLoop
               X86_64.CMP_imm (remainingReg, 0)
               X86_64.Jcc (X86_64.EQ, equalLabel)
               X86_64.MOV_load_byte (leftWordReg, leftReg, 0)
               X86_64.MOV_load_byte (byteReg, rightReg, 0)
               X86_64.CMP_reg (leftWordReg, byteReg)
               X86_64.Jcc (X86_64.NE, unequalLabel)
               X86_64.ADD_imm (leftReg, 1)
               X86_64.ADD_imm (rightReg, 1)
               X86_64.SUB_imm (remainingReg, 1)
               X86_64.JMP byteLoop
               X86_64.Label equalLabel
               X86_64.MOV_imm32 (scratch, 1)
               X86_64.JMP doneLabel
               X86_64.Label unequalLabel
               X86_64.MOV_imm32 (scratch, 0)
               X86_64.Label doneLabel]
            @ restoreInstrs
            @ (if destReg = scratch then [] else [X86_64.MOV_reg (destReg, scratch)])))

let private emitStringConcatBinary (ctx: FuncCtx) (dest: LIR.Reg) (left: LIR.Operand) (right: LIR.Operand) : Result<X86_64.Instr list, string> =
    // String concat: dest = left ++ right
    // Dynamic and literal strings share [refcount:8][length:8][data:N].
    // Strategy: load both strings' info, allocate result, copy bytes with loops.
    // Register plan (no PUSH/POP in loops):
    //   RDI = left data ptr, RSI = left len
    //   R8  = right data ptr, R9 = right len
    //   R10 = loop counter, R11(scratch) = temp byte
    //   destReg = result ptr, RCX = dest write ptr
    //
    // IMPORTANT: This operation clobbers RDI, RSI, RCX, R8, R9, R10.
    // Save/restore all caller-saved registers except those used as operands,
    // since the register allocator doesn't model these clobbers.
    resolveReg dest
    |> Result.bind (fun destReg ->
        // Clobbered registers (RDI=X1, RSI=X2, RCX=X3, R8=X4, R9=X5, R10=X6)
        // Save all except the dest reg (caller may still need operand regs after this)
        let clobbered = [X86_64.RDI; X86_64.RSI; X86_64.RCX; X86_64.R8; X86_64.R9; X86_64.R10]
        let toSave = clobbered |> List.filter (fun r -> r <> destReg)
        let saveInstrs = toSave |> List.map (fun r -> X86_64.PUSH r)
        let restoreInstrs = toSave |> List.rev |> List.map (fun r -> X86_64.POP r)

        let loadInfo (op: LIR.Operand) (addrDest: X86_64.Reg) (lenDest: X86_64.Reg) : Result<X86_64.Instr list, string> =
            match op with
            | LIR.Reg reg ->
                resolveReg reg
                |> Result.map (fun srcReg ->
                    if srcReg = lenDest then
                        // srcReg == lenDest: LEA first so MOV_load doesn't clobber pointer
                        [X86_64.LEA (addrDest, srcReg, 16)
                         X86_64.MOV_load (lenDest, srcReg, 8)]
                    elif srcReg = addrDest then
                        // srcReg == addrDest: save pointer in scratch before LEA clobbers it
                        [X86_64.MOV_reg (scratch, srcReg)
                         X86_64.MOV_load (lenDest, srcReg, 8)
                         X86_64.LEA (addrDest, scratch, 16)]
                    else
                        [X86_64.MOV_load (lenDest, srcReg, 8)
                         X86_64.LEA (addrDest, srcReg, 16)])
            | LIR.StringSymbol value ->
                let len = System.Text.Encoding.UTF8.GetByteCount(value)
                let instrs = emitStringLiteralNoRefCount addrDest value
                let setResults = loadImm64 lenDest (int64 len) @ [X86_64.LEA (addrDest, addrDest, 16)]
                Ok (instrs @ setResults)
            | LIR.StackSlot stackOffset ->
                let adjustedOffset = int32 (adjustStackOffset ctx stackOffset)
                Ok [ X86_64.MOV_load (scratch, X86_64.RBP, adjustedOffset)
                     X86_64.MOV_load (lenDest, scratch, 8)
                     X86_64.LEA (addrDest, scratch, 16) ]
            | _ -> Ok (loadImm64 lenDest 0L @ loadImm64 addrDest 0L)

        let copy1 = freshLabel "strcat_c1"
        let done1 = freshLabel "strcat_d1"
        let copy2 = freshLabel "strcat_c2"
        let done2 = freshLabel "strcat_d2"
        let doneAllocation = freshLabel "strcat_alloc_ok"

        // Load RIGHT first (if Reg, no allocation needed), then LEFT
        // (which might allocate for StringSymbol). This avoids clobbering
        // the right source register during left's heap allocation.
        //
        // Loading the right operand owns R8/R9 and may also use scratch for
        // a stack slot, literal, or aliased R8 source. Preserve a left
        // pointer held in any of those registers before that setup.
        let leftConflictReg =
            match left with
            | LIR.Reg reg ->
                match resolveReg reg with
                | Ok r when r = X86_64.R8 || r = X86_64.R9 || r = scratch -> Some r
                | _ -> None
            | _ -> None

        loadInfo right X86_64.R8 X86_64.R9
        |> Result.bind (fun rightInstrs ->
            // Save right info before loading left (left might clobber R8/R9)
            let saveRight = [X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9]

            let preserveLeft =
                match leftConflictReg with
                | Some r -> [X86_64.PUSH r]
                | None -> []

            let loadLeft =
                match leftConflictReg with
                | Some _ ->
                    // R8 and R9 were pushed after the preserved left pointer.
                    Ok [X86_64.MOV_load (scratch, X86_64.RSP, 16)
                        X86_64.MOV_load (X86_64.RSI, scratch, 8)
                        X86_64.LEA (X86_64.RDI, scratch, 16)]
                | None ->
                    loadInfo left X86_64.RDI X86_64.RSI

            let discardPreservedLeft =
                match leftConflictReg with
                | Some _ -> [X86_64.ADD_imm (X86_64.RSP, 8)]
                | None -> []

            loadLeft
            |> Result.map (fun leftInstrs ->
                saveInstrs
                @ preserveLeft
                @ rightInstrs @ saveRight @ leftInstrs
                // Restore right info
                @ [X86_64.POP X86_64.R9; X86_64.POP X86_64.R8]
                @ discardPreservedLeft

                // Total length in RCX
                @ [X86_64.MOV_reg (X86_64.RCX, X86_64.RSI)
                   X86_64.ADD_reg (X86_64.RCX, X86_64.R9)]

                // Allocate: use RBX to hold result ptr (callee-saved, safe across loops)
                // Save RBX first
                @ [X86_64.PUSH X86_64.RBX]
                @ [X86_64.MOV_reg (X86_64.RBX, heapPtr)
                   X86_64.MOV_reg (X86_64.R10, X86_64.RCX)
                   X86_64.ADD_imm (X86_64.R10, 23)
                   X86_64.AND_imm (X86_64.R10, -8)
                   X86_64.ADD_reg (heapPtr, X86_64.R10)
                   X86_64.MOV_reg (scratch, heapPtr)
                   X86_64.SUB_reg (scratch, freeListBase)
                   X86_64.CMP_imm (scratch, int32 heapMmapSizeBytes)
                   X86_64.Jcc (X86_64.LE, doneAllocation)]
                @ genOomJump ()
                @ [X86_64.Label doneAllocation]

                // Store the fixed header.
                @ loadImm64 scratch 1L
                @ [X86_64.MOV_store (X86_64.RBX, 0, scratch)
                   X86_64.MOV_store (X86_64.RBX, 8, X86_64.RCX)]

                // Copy left bytes: RBX[8+i] = left[i]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.Label copy1
                   X86_64.CMP_reg (X86_64.R10, X86_64.RSI)
                   X86_64.Jcc (X86_64.GE, done1)
                   X86_64.MOV_reg (scratch, X86_64.RDI)
                   X86_64.ADD_reg (scratch, X86_64.R10)
                   X86_64.MOV_load_byte (scratch, scratch, 0)
                   X86_64.LEA (X86_64.RCX, X86_64.RBX, 16)
                   X86_64.ADD_reg (X86_64.RCX, X86_64.R10)
                   X86_64.MOV_store_byte (X86_64.RCX, 0, scratch)
                   X86_64.ADD_imm (X86_64.R10, 1)
                   X86_64.JMP copy1
                   X86_64.Label done1]

                // Copy right bytes: RBX[8+leftLen+i] = right[i]
                @ [X86_64.LEA (X86_64.RCX, X86_64.RBX, 16)
                   X86_64.ADD_reg (X86_64.RCX, X86_64.RSI)]
                @ loadImm64 X86_64.R10 0L
                @ [X86_64.Label copy2
                   X86_64.CMP_reg (X86_64.R10, X86_64.R9)
                   X86_64.Jcc (X86_64.GE, done2)
                   X86_64.MOV_reg (scratch, X86_64.R8)
                   X86_64.ADD_reg (scratch, X86_64.R10)
                   X86_64.MOV_load_byte (scratch, scratch, 0)
                   X86_64.MOV_reg (X86_64.RDI, X86_64.RCX)
                   X86_64.ADD_reg (X86_64.RDI, X86_64.R10)
                   X86_64.MOV_store_byte (X86_64.RDI, 0, scratch)
                   X86_64.ADD_imm (X86_64.R10, 1)
                   X86_64.JMP copy2
                   X86_64.Label done2]

                // Leak counter increment for string allocation
                @ genLeakCounterInc ctx
                // Move result to destReg, restore RBX
                // If destReg IS RBX, we need to save result elsewhere first
                @ (if destReg = X86_64.RBX then
                       // Result is already in RBX. Pop saved RBX to scratch, keep result.
                       [X86_64.ADD_imm (X86_64.RSP, 8)]  // discard saved RBX
                   else
                       [X86_64.MOV_reg (destReg, X86_64.RBX)
                        X86_64.POP X86_64.RBX])
                @ restoreInstrs)))

/// Lower a concat tree as one length pass, one allocation, and one ordered copy pass.
let private emitStringConcatMany
    (ctx: FuncCtx)
    (dest: LIR.Reg)
    (first: LIR.Operand)
    (second: LIR.Operand)
    (remaining: LIR.Operand list)
    : Result<X86_64.Instr list, string> =
    let operands = first :: second :: remaining
    let savedRegs =
        [ X86_64.RAX; X86_64.RDI; X86_64.RSI; X86_64.RCX
          X86_64.R8; X86_64.R9; X86_64.R10; X86_64.RBX ]

    let snapshotOperand (operand: LIR.Operand) : Result<X86_64.Instr list, string> =
        match operand with
        | LIR.Reg reg ->
            resolveReg reg
            |> Result.map (fun source ->
                [ X86_64.PUSH source
                  X86_64.MOV_load (scratch, source, 8)
                  X86_64.PUSH scratch ])
        | LIR.StringSymbol value ->
            Ok (emitStringLiteralNoRefCount scratch value
                @ [ X86_64.PUSH scratch ]
                @ loadImm64 scratch (int64 (System.Text.Encoding.UTF8.GetByteCount value))
                @ [ X86_64.PUSH scratch ])
        | LIR.StackSlot stackOffset ->
            let adjustedOffset = int32 (adjustStackOffset ctx stackOffset)
            Ok [ X86_64.MOV_load (scratch, X86_64.RBP, adjustedOffset)
                 X86_64.PUSH scratch
                 X86_64.MOV_load (scratch, scratch, 8)
                 X86_64.PUSH scratch ]
        | other -> Error $"StringConcat requires string operands, got: {other}"

    resolveReg dest
    |> Result.bind (fun destReg ->
        operands
        |> ResultList.mapResults snapshotOperand
        |> Result.map (fun snapshots ->
            let save = savedRegs |> List.map X86_64.PUSH
            let restore = savedRegs |> List.rev |> List.map X86_64.POP
            let operandCount = List.length operands
            let stackOffset index fieldOffset =
                int32 (((2 * (operandCount - index - 1)) + fieldOffset) * 8)

            let measure =
                loadImm64 X86_64.RCX 0L
                @ ([0 .. operandCount - 1]
                   |> List.collect (fun index ->
                       [ X86_64.MOV_load (scratch, X86_64.RSP, stackOffset index 0)
                         X86_64.ADD_reg (X86_64.RCX, scratch) ]))

            let allocationDone = freshLabel "strcat_many_alloc_ok"
            let allocate =
                [ X86_64.MOV_reg (X86_64.RBX, heapPtr)
                  X86_64.MOV_reg (X86_64.R10, X86_64.RCX)
                  X86_64.ADD_imm (X86_64.R10, 23)
                  X86_64.AND_imm (X86_64.R10, -8)
                  X86_64.ADD_reg (heapPtr, X86_64.R10)
                  X86_64.MOV_reg (scratch, heapPtr)
                  X86_64.SUB_reg (scratch, freeListBase)
                  X86_64.CMP_imm (scratch, int32 heapMmapSizeBytes)
                  X86_64.Jcc (X86_64.LE, allocationDone) ]
                @ genOomJump ()
                @ [ X86_64.Label allocationDone ]
                @ loadImm64 scratch 1L
                @ [ X86_64.MOV_store (X86_64.RBX, 0, scratch)
                    X86_64.MOV_store (X86_64.RBX, 8, X86_64.RCX)
                    X86_64.LEA (X86_64.RDI, X86_64.RBX, 16) ]

            let copy index =
                let loop = freshLabel $"strcat_many_copy_{index}"
                let doneLabel = freshLabel $"strcat_many_done_{index}"
                [ X86_64.MOV_load (X86_64.RSI, X86_64.RSP, stackOffset index 1)
                  X86_64.ADD_imm (X86_64.RSI, 16)
                  X86_64.MOV_load (X86_64.R10, X86_64.RSP, stackOffset index 0)
                  X86_64.Label loop
                  X86_64.CMP_imm (X86_64.R10, 0)
                  X86_64.Jcc (X86_64.LE, doneLabel)
                  X86_64.MOV_load_byte (scratch, X86_64.RSI, 0)
                  X86_64.MOV_store_byte (X86_64.RDI, 0, scratch)
                  X86_64.ADD_imm (X86_64.RSI, 1)
                  X86_64.ADD_imm (X86_64.RDI, 1)
                  X86_64.SUB_imm (X86_64.R10, 1)
                  X86_64.JMP loop
                  X86_64.Label doneLabel ]

            save
            @ (snapshots |> List.concat)
            @ measure
            @ allocate
            @ ([0 .. operandCount - 1] |> List.collect copy)
            @ genLeakCounterInc ctx
            @ [ X86_64.MOV_reg (scratch, X86_64.RBX)
                X86_64.ADD_imm (X86_64.RSP, operandCount * 16) ]
            @ restore
            @ (if destReg = scratch then [] else [ X86_64.MOV_reg (destReg, scratch) ])))

let internal emitStringConcat ctx dest first second remaining =
    match remaining with
    | [] -> emitStringConcatBinary ctx dest first second
    | _ -> emitStringConcatMany ctx dest first second remaining
