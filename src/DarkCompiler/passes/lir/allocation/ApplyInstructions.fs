// ApplyInstructions.fs - Rewrite instruction operands through the allocation and spill plan.

module ApplyRegisterAllocation

open AllocationModel
open SpillOperands

/// Apply allocation to an instruction
let applyToInstr (arch: Platform.Arch) (mapping: AllocationResult) (instr: LIR.Instr) : LIR.Instr list =
    match instr with
    | LIR.Phi _ ->
        // Phi nodes are handled specially by resolvePhiNodes after allocation.
        // Skip them here - they will be removed and converted to moves at predecessor exits.
        []

    | LIR.FPhi _ ->
        // Float phi nodes are handled specially by resolvePhiNodes after allocation.
        // Skip them here - they will be removed and converted to FMov at predecessor exits.
        []

    | LIR.FSpillLoad _
    | LIR.FSpillStore _ ->
        [instr]

    | LIR.Mov (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcOp, srcLoads) = applyToOperand mapping src LIR.X12
        let movInstr = LIR.Mov (destReg, srcOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [movInstr] @ storeInstrs

    | LIR.Store (offset, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.Store (offset, srcReg)]

    | LIR.Add (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        let addInstr = LIR.Add (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [addInstr] @ storeInstrs

    | LIR.Sub (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        let subInstr = LIR.Sub (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [subInstr] @ storeInstrs

    | LIR.Mul (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let mulInstr = LIR.Mul (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [mulInstr] @ storeInstrs

    | LIR.Sdiv (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let divInstr = LIR.Sdiv (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [divInstr] @ storeInstrs

    | LIR.Udiv (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let divInstr = LIR.Udiv (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [divInstr] @ storeInstrs

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        if isX86_64 arch then
            // On x86_64, X12/X13/X14 all alias R11. Use loadSpilledPair for mul
            // operands (uses dest as a safe temp), and preserve a distinct physical
            // register for a spilled third operand when either multiplicand uses R11.
            let ((mulLeftReg, mulLeftLoads), (mulRightReg, mulRightLoads)) =
                loadSpilledPair arch mapping mulLeft mulRight destReg
            let subIsSpilled =
                match sub with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let mulOperandAliasesScratch =
                [mulLeftReg; mulRightReg]
                |> List.exists (function
                    | LIR.Physical reg -> aliasesX86ScratchReg reg
                    | LIR.Virtual _ -> false)
            let preservedTemp = x86SpillTempExcluding [destReg; mulLeftReg; mulRightReg]
            let subTemp = if subIsSpilled && mulOperandAliasesScratch then preservedTemp else LIR.X12
            let (subReg, subLoads) = loadSpilled mapping sub subTemp
            let msubInstr = LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            let preserveTemp = if subIsSpilled && mulOperandAliasesScratch then [LIR.SaveRegs ([preservedTemp], [])] else []
            let restoreTemp = if subIsSpilled && mulOperandAliasesScratch then [LIR.RestoreRegs ([preservedTemp], [])] else []
            preserveTemp @ mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs @ restoreTemp
        else
            let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
            let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
            let (subReg, subLoads) = loadSpilled mapping sub LIR.X14
            let msubInstr = LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs

    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        if isX86_64 arch then
            let ((mulLeftReg, mulLeftLoads), (mulRightReg, mulRightLoads)) =
                loadSpilledPair arch mapping mulLeft mulRight destReg
            let addIsSpilled =
                match add with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let mulOperandAliasesScratch =
                [mulLeftReg; mulRightReg]
                |> List.exists (function
                    | LIR.Physical reg -> aliasesX86ScratchReg reg
                    | LIR.Virtual _ -> false)
            let preservedTemp = x86SpillTempExcluding [destReg; mulLeftReg; mulRightReg]
            let addTemp = if addIsSpilled && mulOperandAliasesScratch then preservedTemp else LIR.X12
            let (addReg, addLoads) = loadSpilled mapping add addTemp
            let maddInstr = LIR.Madd (destReg, mulLeftReg, mulRightReg, addReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            let preserveTemp = if addIsSpilled && mulOperandAliasesScratch then [LIR.SaveRegs ([preservedTemp], [])] else []
            let restoreTemp = if addIsSpilled && mulOperandAliasesScratch then [LIR.RestoreRegs ([preservedTemp], [])] else []
            preserveTemp @ mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs @ restoreTemp
        else
            let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
            let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
            let (addReg, addLoads) = loadSpilled mapping add LIR.X14
            let maddInstr = LIR.Madd (destReg, mulLeftReg, mulRightReg, addReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs

    | LIR.Cmp (left, right) ->
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        leftLoads @ rightLoads @ [LIR.Cmp (leftReg, rightOp)]

    | LIR.Cset (dest, cond) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let csetInstr = LIR.Cset (destReg, cond)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [csetInstr] @ storeInstrs

    | LIR.And (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let andInstr = LIR.And (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [andInstr] @ storeInstrs

    | LIR.And_imm (dest, src, imm) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let andInstr = LIR.And_imm (destReg, srcReg, imm)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [andInstr] @ storeInstrs

    | LIR.Orr (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let orrInstr = LIR.Orr (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [orrInstr] @ storeInstrs

    | LIR.Eor (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftReg, leftLoads), (rightReg, rightLoads)) = loadSpilledPair arch mapping left right destReg
        let eorInstr = LIR.Eor (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [eorInstr] @ storeInstrs

    | LIR.Lsl (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = loadSpilledPair arch mapping src shift destReg
        let lslInstr = LIR.Lsl (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lslInstr] @ storeInstrs

    | LIR.Lsr (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = loadSpilledPair arch mapping src shift destReg
        let lsrInstr = LIR.Lsr (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lsrInstr] @ storeInstrs

    | LIR.Asr (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = loadSpilledPair arch mapping src shift destReg
        let asrInstr = LIR.Asr (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [asrInstr] @ storeInstrs

    | LIR.Lsl_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let lslInstr = LIR.Lsl_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [lslInstr] @ storeInstrs

    | LIR.Lsr_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let lsrInstr = LIR.Lsr_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [lsrInstr] @ storeInstrs

    | LIR.Asr_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let asrInstr = LIR.Asr_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [asrInstr] @ storeInstrs

    | LIR.Neg (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let negInstr = LIR.Neg (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [negInstr] @ storeInstrs

    | LIR.Mvn (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let mvnInstr = LIR.Mvn (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [mvnInstr] @ storeInstrs

    // Sign/zero extension instructions (for integer overflow)
    | LIR.Sxtb (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Sxtb (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Sxth (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Sxth (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Sxtw (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Sxtw (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Uxtb (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Uxtb (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Uxth (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Uxth (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Uxtw (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIR.Uxtw (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIR.Call (dest, funcName, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then
                    // On x86_64, X12/X13 both map to R11. Use applyToOperandNoLoad
                    // to keep spilled args as StackSlots - ArgMoves handles loading them.
                    (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.Call (destReg, funcName, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        argLoads @ [callInstr] @ storeInstrs

    | LIR.TailCall (funcName, args) ->
        // Tail calls have no destination - just apply allocation to args
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.TailCall (funcName, argOps)
        argLoads @ [callInstr]

    | LIR.IndirectCall (dest, func, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.IndirectCall (destReg, funcReg, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        funcLoads @ argLoads @ [callInstr] @ storeInstrs

    | LIR.IndirectTailCall (func, args) ->
        // Indirect tail calls have no destination
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.IndirectTailCall (funcReg, argOps)
        funcLoads @ argLoads @ [callInstr]

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedCaptures =
            captures |> List.mapi (fun i cap ->
                if isX86_64 arch then (applyToOperandNoLoad mapping cap, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping cap tempReg
            )
        let capLoads = allocatedCaptures |> List.collect snd
        let capOps = allocatedCaptures |> List.map fst
        let allocInstr = LIR.ClosureAlloc (destReg, funcName, capOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        capLoads @ [allocInstr] @ storeInstrs

    | LIR.ClosureCall (dest, closure, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (closureReg, closureLoads) = loadSpilled mapping closure LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.ClosureCall (destReg, closureReg, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        closureLoads @ argLoads @ [callInstr] @ storeInstrs

    | LIR.ClosureTailCall (closure, args) ->
        // Closure tail calls have no destination
        let (closureReg, closureLoads) = loadSpilled mapping closure LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                if isX86_64 arch then (applyToOperandNoLoad mapping arg, [])
                else
                    let tempReg = if i = 0 then LIR.X12 else LIR.X13
                    applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIR.ClosureTailCall (closureReg, argOps)
        closureLoads @ argLoads @ [callInstr]

    // SaveRegs/RestoreRegs are handled specially in applyToBlockWithLiveness
    // These patterns handle the case where they've already been populated
    | LIR.SaveRegs (intRegs, floatRegs) -> [LIR.SaveRegs (intRegs, floatRegs)]
    | LIR.RestoreRegs (intRegs, floatRegs) -> [LIR.RestoreRegs (intRegs, floatRegs)]

    | LIR.ArgMoves moves ->
        // ArgMoves must preserve distinct sources for each argument.
        // Use no-load allocation so spilled values remain StackSlot and are
        // loaded per-move in CodeGen (avoids reusing a single temp).
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                let allocatedOp = applyToOperandNoLoad mapping srcOp
                (destReg, allocatedOp))
        [LIR.ArgMoves allocatedMoves]

    | LIR.TailArgMoves moves ->
        // Apply allocation WITHOUT loading spilled values into a temp register.
        // This is different from ArgMoves: for tail calls, we can't use a shared temp
        // because there's no SaveRegs to preserve values. CodeGen will handle StackSlots
        // by loading them directly into the destination register.
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                (destReg, applyToOperandNoLoad mapping srcOp))
        [LIR.TailArgMoves allocatedMoves]

    | LIR.FArgMoves moves ->
        // Pass through unchanged for now - float argument moves use physical registers only
        [LIR.FArgMoves moves]

    | LIR.PrintInt64 reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintInt64 regFinal]

    | LIR.PrintUInt64 reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintUInt64 regFinal]

    | LIR.PrintBool reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBool regFinal]

    | LIR.PrintInt64NoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintInt64NoNewline regFinal]

    | LIR.PrintUInt64NoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintUInt64NoNewline regFinal]

    | LIR.PrintBoolNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBoolNoNewline regFinal]

    | LIR.PrintFloatNoNewline freg -> [LIR.PrintFloatNoNewline freg]

    | LIR.PrintHeapStringNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintHeapStringNoNewline regFinal]

    | LIR.PrintList (listPtr, elemType) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping listPtr LIR.X12
        ptrLoads @ [LIR.PrintList (ptrFinal, elemType)]

    | LIR.PrintSum (sumPtr, variants) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping sumPtr LIR.X12
        ptrLoads @ [LIR.PrintSum (ptrFinal, variants)]

    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping recordPtr LIR.X12
        ptrLoads @ [LIR.PrintRecord (ptrFinal, typeName, fields)]

    | LIR.PrintFloat freg -> [LIR.PrintFloat freg]
    | LIR.PrintString value -> [LIR.PrintString value]
    | LIR.StdoutWrite (effectId, value, appendNewline) ->
        let (valueOp, valueLoads) = applyToOperand mapping value LIR.X12
        valueLoads @ [LIR.StdoutWrite (effectId, valueOp, appendNewline)]
    | LIR.StdinReadLine (effectId, dest) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [LIR.StdinReadLine (effectId, destReg)] @ storeInstrs
    | LIR.RuntimeError message -> [LIR.RuntimeError message]
    | LIR.RuntimeErrorString reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.RuntimeErrorString regFinal]
    | LIR.PrintChars chars -> [LIR.PrintChars chars]
    | LIR.PrintBlob reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBlob regFinal]

    // FP instructions pass through unchanged
    | LIR.FMov (dest, src) -> [LIR.FMov (dest, src)]
    | LIR.FLoad (dest, value) -> [LIR.FLoad (dest, value)]
    | LIR.FAdd (dest, left, right) -> [LIR.FAdd (dest, left, right)]
    | LIR.FSub (dest, left, right) -> [LIR.FSub (dest, left, right)]
    | LIR.FMul (dest, left, right) -> [LIR.FMul (dest, left, right)]
    | LIR.FDiv (dest, left, right) -> [LIR.FDiv (dest, left, right)]
    | LIR.FNeg (dest, src) -> [LIR.FNeg (dest, src)]
    | LIR.FAbs (dest, src) -> [LIR.FAbs (dest, src)]
    | LIR.FSqrt (dest, src) -> [LIR.FSqrt (dest, src)]
    | LIR.FCmp (left, right) -> [LIR.FCmp (left, right)]
    // Int64ToFloat: src is integer register, dest is FP register
    | LIR.Int64ToFloat (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.Int64ToFloat (dest, srcReg)]
    // GpToFp: move bits from GP register to FP register (src is integer, dest is FP)
    | LIR.GpToFp (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.GpToFp (dest, srcReg)]
    // FloatToInt64: src is FP register, dest is integer register
    | LIR.FloatToInt64 (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FloatToInt64 (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // FpToGp: src is FP register, dest is integer register
    | LIR.FpToGp (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FpToGp (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // FloatToBits: src is FP register, dest is integer register (bit copy)
    | LIR.FloatToBits (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FloatToBits (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // Heap operations
    | LIR.HeapAlloc (dest, size) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocInstr = LIR.HeapAlloc (destReg, size)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [allocInstr] @ storeInstrs

    | LIR.HeapStore (addr, offset, src, vt) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let (srcOp, srcLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping src, [])
            else applyToOperand mapping src LIR.X13
        addrLoads @ srcLoads @ [LIR.HeapStore (addrReg, offset, srcOp, vt)]

    | LIR.HeapLoad (dest, addr, offset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let loadInstr = LIR.HeapLoad (destReg, addrReg, offset)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        addrLoads @ [loadInstr] @ storeInstrs

    | LIR.RefCountInc (addr, payloadSize, kind, sourceType) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIR.RefCountInc (addrReg, payloadSize, kind, sourceType)]

    | LIR.RefCountDec (addr, payloadSize, kind, sourceType) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIR.RefCountDec (addrReg, payloadSize, kind, sourceType)]

    | LIR.StringConcat (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftOp, leftLoads) = applyToOperand mapping left LIR.X12
        let (rightOp, rightLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping right, [])
            else applyToOperand mapping right LIR.X13
        let concatInstr = LIR.StringConcat (destReg, leftOp, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [concatInstr] @ storeInstrs

    | LIR.CanonicalBufferEq (dest, kind, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((leftOp, leftLoads), (rightOp, rightLoads)) =
            match left, right with
            | LIR.Reg leftReg, LIR.Reg rightReg ->
                let ((allocatedLeft, leftLoads), (allocatedRight, rightLoads)) =
                    loadSpilledPair arch mapping leftReg rightReg destReg
                ((LIR.Reg allocatedLeft, leftLoads), (LIR.Reg allocatedRight, rightLoads))
            | _ ->
                (applyToOperand mapping left LIR.X12,
                 applyToOperand mapping right LIR.X13)
        let eqInstr = LIR.CanonicalBufferEq (destReg, kind, leftOp, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [eqInstr] @ storeInstrs

    | LIR.PrintHeapString reg ->
        let (regPhys, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintHeapString regPhys]

    | LIR.LoadFuncAddr (dest, funcName) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let loadInstr = LIR.LoadFuncAddr (destReg, funcName)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [loadInstr] @ storeInstrs

    | LIR.FileReadText (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileReadText (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileExists (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileExists (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileWriteText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping content, [])
            else applyToOperand mapping content LIR.X13
        let fileInstr = LIR.FileWriteText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIR.FileAppendText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) =
            if isX86_64 arch then (applyToOperandNoLoad mapping content, [])
            else applyToOperand mapping content LIR.X13
        let fileInstr = LIR.FileAppendText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIR.FileDelete (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileDelete (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileSetExecutable (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIR.FileSetExecutable (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        if isX86_64 arch then
            let ((ptrReg, ptrLoads), (lengthReg, lengthLoads)) =
                loadSpilledPair arch mapping ptr length destReg
            let fileInstr = LIR.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X13
            let (lengthReg, lengthLoads) = loadSpilled mapping length LIR.X14
            let fileInstr = LIR.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)
            let storeInstrs =
                match destAlloc with
                | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
                | _ -> []
            pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs

    | LIR.RawAlloc (dest, numBytes) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (numBytesReg, numBytesLoads) = loadSpilled mapping numBytes LIR.X12
        let allocInstr = LIR.RawAlloc (destReg, numBytesReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        numBytesLoads @ [allocInstr] @ storeInstrs

    | LIR.MappedAlloc (dest, numBytes) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (numBytesReg, numBytesLoads) = loadSpilled mapping numBytes LIR.X12
        let allocInstr = LIR.MappedAlloc (destReg, numBytesReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        numBytesLoads @ [allocInstr] @ storeInstrs

    | LIR.RawFree ptr ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        ptrLoads @ [LIR.RawFree ptrReg]

    | LIR.MappedFree ptr ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        ptrLoads @ [LIR.MappedFree ptrReg]

    | LIR.RawGet (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((ptrReg, ptrLoads), (offsetReg, offsetLoads)) = loadSpilledPair arch mapping ptr byteOffset destReg
        let getInstr = LIR.RawGet (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let ((ptrReg, ptrLoads), (offsetReg, offsetLoads)) = loadSpilledPair arch mapping ptr byteOffset destReg
        let getInstr = LIR.RawGetByte (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIR.RawWriteWord (ptr, byteOffset, value) ->
        if isX86_64 arch then
            // On x86_64, X12/X13/X14 all alias R11. When both ptr and value are
            // spilled, loading both into R11 clobbers one. Save X3 (RCX) via
            // push/pop and use it as a non-R11 temp for ptr. The codegen already
            // handles ptr=RCX when value=R11(scratch).
            let ptrSpilled =
                match ptr with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let valueSpilled =
                match value with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            if ptrSpilled && valueSpilled then
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X3
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                [LIR.SaveRegs ([LIR.X3], [])]
                @ ptrLoads @ offsetLoads @ valueLoads
                @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)]
                @ [LIR.RestoreRegs ([LIR.X3], [])]
            else
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)]
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
            let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
            let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
            ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)]

    | LIR.RawWriteByte (ptr, byteOffset, value) ->
        if isX86_64 arch then
            let ptrSpilled =
                match ptr with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let valueSpilled =
                match value with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            if ptrSpilled && valueSpilled then
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X3
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                [LIR.SaveRegs ([LIR.X3], [])]
                @ ptrLoads @ offsetLoads @ valueLoads
                @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)]
                @ [LIR.RestoreRegs ([LIR.X3], [])]
            else
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)]
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
            let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
            let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
            ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)]

    | LIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
        if isX86_64 arch then
            let ptrSpilled =
                match ptr with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            let valueSpilled =
                match value with
                | LIR.Virtual id -> match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false
                | _ -> false
            if ptrSpilled && valueSpilled then
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X3
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                [LIR.SaveRegs ([LIR.X3], [])]
                @ ptrLoads @ offsetLoads @ valueLoads
                @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)]
                @ [LIR.RestoreRegs ([LIR.X3], [])]
            else
                let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
                let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X12
                let (valueReg, valueLoads) = loadSpilled mapping value LIR.X12
                ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)]
        else
            let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
            let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
            let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
            ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)]

    | LIR.RefCountIncString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIR.RefCountIncString strOp]

    | LIR.RefCountDecString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIR.RefCountDecString strOp]

    | LIR.RefCountIncBlob bytes ->
        let (bytesOp, bytesLoads) = applyToOperand mapping bytes LIR.X12
        bytesLoads @ [LIR.RefCountIncBlob bytesOp]

    | LIR.RefCountDecBlob bytes ->
        let (bytesOp, bytesLoads) = applyToOperand mapping bytes LIR.X12
        bytesLoads @ [LIR.RefCountDecBlob bytesOp]

    | LIR.RandomInt64 dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let randomInstr = LIR.RandomInt64 destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [randomInstr] @ storeInstrs

    | LIR.DateTimeNow dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let dateInstr = LIR.DateTimeNow destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [dateInstr] @ storeInstrs

    | LIR.Sleep _ ->
        [instr]

    | LIR.CliNative (dest, operation, args) ->
        let scratchRegs = [LIR.X12; LIR.X13; LIR.X14]
        let resolvedArgs =
            args
            |> List.mapi (fun index arg ->
                let scratch = List.item (min index 2) scratchRegs
                applyToOperand mapping arg scratch)
        let argLoads = resolvedArgs |> List.collect snd
        let args' = resolvedArgs |> List.map fst
        let (destReg, destAlloc) = applyToReg mapping dest
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        argLoads @ [LIR.CliNative (destReg, operation, args')] @ storeInstrs

    | LIR.FloatToString (dest, value) ->
        // FP register value is already physical after float allocation
        let (destReg, destAlloc) = applyToReg mapping dest
        let floatToStrInstr = LIR.FloatToString (destReg, value)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [floatToStrInstr] @ storeInstrs

    | LIR.CoverageHit exprId ->
        [LIR.CoverageHit exprId]  // No registers to allocate

    | LIR.Exit -> [LIR.Exit]
