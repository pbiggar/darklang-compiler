(* ApplyRegisterAllocation.ml - Rewrite instruction operands through the allocation and spill plan. *)
[@@@warning "-4"]
open AllocationModel
open SpillOperands
(*
   Apply allocation to an instruction
   Phi nodes are handled specially by resolvePhiNodes after allocation.
   Skip them here - they will be removed and converted to moves at predecessor exits.
   Float phi nodes are handled specially by resolvePhiNodes after allocation.
   Skip them here - they will be removed and converted to FMov at predecessor exits.
   On x86_64, X12/X13/X14 all alias R11. Use loadSpilledPair for mul
   operands (uses dest as a safe temp), and preserve a distinct physical
   register for a spilled third operand when either multiplicand uses R11.
   Sign/zero extension instructions (for integer overflow)
   On x86_64, X12/X13 both map to R11. Use applyToOperandNoLoad
   to keep spilled args as StackSlots - ArgMoves handles loading them.
   Tail calls have no destination - just apply allocation to args
   Indirect tail calls have no destination
   Closure tail calls have no destination
   SaveRegs/RestoreRegs are handled specially in applyToBlockWithLiveness
   These patterns handle the case where they've already been populated
   ArgMoves must preserve distinct sources for each argument.
   Use no-load allocation so spilled values remain StackSlot and are
   loaded per-move in CodeGen (avoids reusing a single temp).
   Apply allocation WITHOUT loading spilled values into a temp register.
   This is different from ArgMoves: for tail calls, we can't use a shared temp
   because there's no SaveRegs to preserve values. CodeGen will handle StackSlots
   by loading them directly into the destination register.
   Pass through unchanged for now - float argument moves use physical registers only
   FP instructions pass through unchanged
   Int64ToFloat: src is integer register, dest is FP register
   GpToFp: move bits from GP register to FP register (src is integer, dest is FP)
   FloatToInt64: src is FP register, dest is integer register
   FpToGp: src is FP register, dest is integer register
   FloatToBits: src is FP register, dest is integer register (bit copy)
   Heap operations
   Variadic concat consumes operands sequentially, so spilled values
   stay in frame slots instead of competing for scratch registers.
   On x86_64, X12/X13/X14 all alias R11. When both ptr and value are
   spilled, loading both into R11 clobbers one. Save X3 (RCX) via
   push/pop and use it as a non-R11 temp for ptr. The codegen already
   handles ptr=RCX when value=R11(scratch).
   FP register value is already physical after float allocation
   No registers to allocate
*)
let applyToInstr arch mapping instr =
(match instr with
| LIR.Phi _ -> ([])
| LIR.FPhi _ -> ([])
| LIR.FSpillLoad _ | LIR.FSpillStore _ -> ([instr])
| LIR.Mov (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcOp, srcLoads) = (applyToOperand mapping src LIR.X12) in
let movInstr = (LIR.Mov (destReg, srcOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [movInstr] @ storeInstrs)
| LIR.Store (offset, src) -> (let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
srcLoads @ [LIR.Store (offset, srcReg)])
| LIR.Add (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (leftReg, leftLoads) = (loadSpilled mapping left LIR.X12) in
let (rightOp, rightLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping right, [])) else (applyToOperand mapping right LIR.X13))) in
let addInstr = (LIR.Add (destReg, leftReg, rightOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [addInstr] @ storeInstrs)
| LIR.Sub (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (leftReg, leftLoads) = (loadSpilled mapping left LIR.X12) in
let (rightOp, rightLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping right, [])) else (applyToOperand mapping right LIR.X13))) in
let subInstr = (LIR.Sub (destReg, leftReg, rightOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [subInstr] @ storeInstrs)
| LIR.Mul (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftReg, leftLoads), (rightReg, rightLoads)) = (loadSpilledPair arch mapping left right destReg) in
let mulInstr = (LIR.Mul (destReg, leftReg, rightReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [mulInstr] @ storeInstrs)
| LIR.Sdiv (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftReg, leftLoads), (rightReg, rightLoads)) = (loadSpilledPair arch mapping left right destReg) in
let divInstr = (LIR.Sdiv (destReg, leftReg, rightReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [divInstr] @ storeInstrs)
| LIR.Udiv (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftReg, leftLoads), (rightReg, rightLoads)) = (loadSpilledPair arch mapping left right destReg) in
let divInstr = (LIR.Udiv (destReg, leftReg, rightReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [divInstr] @ storeInstrs)
| LIR.Msub (dest, mulLeft, mulRight, sub) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
(if isX86_64 arch then (let ((mulLeftReg, mulLeftLoads), (mulRightReg, mulRightLoads)) = (loadSpilledPair arch mapping mulLeft mulRight destReg) in
let subIsSpilled = ((match sub with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
let mulOperandAliasesScratch = ([mulLeftReg; mulRightReg] |> List.exists (function
| LIR.Physical reg -> (aliasesX86ScratchReg reg)
| LIR.Virtual _ -> (false))) in
let preservedTemp = (x86SpillTempExcluding [destReg; mulLeftReg; mulRightReg]) in
let subTemp = (if subIsSpilled && mulOperandAliasesScratch then preservedTemp else LIR.X12) in
let (subReg, subLoads) = (loadSpilled mapping sub subTemp) in
let msubInstr = (LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
let preserveTemp = (if subIsSpilled && mulOperandAliasesScratch then [LIR.SaveRegs ([preservedTemp], [])] else []) in
let restoreTemp = (if subIsSpilled && mulOperandAliasesScratch then [LIR.RestoreRegs ([preservedTemp], [])] else []) in
preserveTemp @ mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs @ restoreTemp) else (let (mulLeftReg, mulLeftLoads) = (loadSpilled mapping mulLeft LIR.X12) in
let (mulRightReg, mulRightLoads) = (loadSpilled mapping mulRight LIR.X13) in
let (subReg, subLoads) = (loadSpilled mapping sub LIR.X14) in
let msubInstr = (LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs)))
| LIR.Madd (dest, mulLeft, mulRight, add) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
(if isX86_64 arch then (let ((mulLeftReg, mulLeftLoads), (mulRightReg, mulRightLoads)) = (loadSpilledPair arch mapping mulLeft mulRight destReg) in
let addIsSpilled = ((match add with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
let mulOperandAliasesScratch = ([mulLeftReg; mulRightReg] |> List.exists (function
| LIR.Physical reg -> (aliasesX86ScratchReg reg)
| LIR.Virtual _ -> (false))) in
let preservedTemp = (x86SpillTempExcluding [destReg; mulLeftReg; mulRightReg]) in
let addTemp = (if addIsSpilled && mulOperandAliasesScratch then preservedTemp else LIR.X12) in
let (addReg, addLoads) = (loadSpilled mapping add addTemp) in
let maddInstr = (LIR.Madd (destReg, mulLeftReg, mulRightReg, addReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
let preserveTemp = (if addIsSpilled && mulOperandAliasesScratch then [LIR.SaveRegs ([preservedTemp], [])] else []) in
let restoreTemp = (if addIsSpilled && mulOperandAliasesScratch then [LIR.RestoreRegs ([preservedTemp], [])] else []) in
preserveTemp @ mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs @ restoreTemp) else (let (mulLeftReg, mulLeftLoads) = (loadSpilled mapping mulLeft LIR.X12) in
let (mulRightReg, mulRightLoads) = (loadSpilled mapping mulRight LIR.X13) in
let (addReg, addLoads) = (loadSpilled mapping add LIR.X14) in
let maddInstr = (LIR.Madd (destReg, mulLeftReg, mulRightReg, addReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs)))
| LIR.Cmp (left, right) -> (let (leftReg, leftLoads) = (loadSpilled mapping left LIR.X12) in
let (rightOp, rightLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping right, [])) else (applyToOperand mapping right LIR.X13))) in
leftLoads @ rightLoads @ [LIR.Cmp (leftReg, rightOp)])
| LIR.Cset (dest, cond) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let csetInstr = (LIR.Cset (destReg, cond)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[csetInstr] @ storeInstrs)
| LIR.Select (dest, whenTrue, whenFalse, cond) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((trueReg, trueLoads), (falseReg, falseLoads)) = (loadSpilledPair arch mapping whenTrue whenFalse destReg) in
let selectInstr = (LIR.Select (destReg, trueReg, falseReg, cond)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
trueLoads @ falseLoads @ [selectInstr] @ storeInstrs)
| LIR.And (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftReg, leftLoads), (rightReg, rightLoads)) = (loadSpilledPair arch mapping left right destReg) in
let andInstr = (LIR.And (destReg, leftReg, rightReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [andInstr] @ storeInstrs)
| LIR.And_imm (dest, src, imm) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let andInstr = (LIR.And_imm (destReg, srcReg, imm)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [andInstr] @ storeInstrs)
| LIR.Orr (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftReg, leftLoads), (rightReg, rightLoads)) = (loadSpilledPair arch mapping left right destReg) in
let orrInstr = (LIR.Orr (destReg, leftReg, rightReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [orrInstr] @ storeInstrs)
| LIR.Eor (dest, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftReg, leftLoads), (rightReg, rightLoads)) = (loadSpilledPair arch mapping left right destReg) in
let eorInstr = (LIR.Eor (destReg, leftReg, rightReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [eorInstr] @ storeInstrs)
| LIR.Lsl (dest, src, shift) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = (loadSpilledPair arch mapping src shift destReg) in
let lslInstr = (LIR.Lsl (destReg, srcReg, shiftReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ shiftLoads @ [lslInstr] @ storeInstrs)
| LIR.Lsr (dest, src, shift) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = (loadSpilledPair arch mapping src shift destReg) in
let lsrInstr = (LIR.Lsr (destReg, srcReg, shiftReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ shiftLoads @ [lsrInstr] @ storeInstrs)
| LIR.Asr (dest, src, shift) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((srcReg, srcLoads), (shiftReg, shiftLoads)) = (loadSpilledPair arch mapping src shift destReg) in
let asrInstr = (LIR.Asr (destReg, srcReg, shiftReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ shiftLoads @ [asrInstr] @ storeInstrs)
| LIR.Lsl_imm (dest, src, shift) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let lslInstr = (LIR.Lsl_imm (destReg, srcReg, shift)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [lslInstr] @ storeInstrs)
| LIR.Lsr_imm (dest, src, shift) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let lsrInstr = (LIR.Lsr_imm (destReg, srcReg, shift)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [lsrInstr] @ storeInstrs)
| LIR.Asr_imm (dest, src, shift) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let asrInstr = (LIR.Asr_imm (destReg, srcReg, shift)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [asrInstr] @ storeInstrs)
| LIR.Neg (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let negInstr = (LIR.Neg (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [negInstr] @ storeInstrs)
| LIR.Mvn (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let mvnInstr = (LIR.Mvn (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [mvnInstr] @ storeInstrs)
| LIR.Sxtb (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let extInstr = (LIR.Sxtb (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [extInstr] @ storeInstrs)
| LIR.Sxth (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let extInstr = (LIR.Sxth (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [extInstr] @ storeInstrs)
| LIR.Sxtw (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let extInstr = (LIR.Sxtw (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [extInstr] @ storeInstrs)
| LIR.Uxtb (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let extInstr = (LIR.Uxtb (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [extInstr] @ storeInstrs)
| LIR.Uxth (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let extInstr = (LIR.Uxth (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [extInstr] @ storeInstrs)
| LIR.Uxtw (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
let extInstr = (LIR.Uxtw (destReg, srcReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
srcLoads @ [extInstr] @ storeInstrs)
| LIR.Call (dest, funcName, args) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let allocatedArgs = (args |> List.mapi (fun i arg ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping arg, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping arg tempReg)))) in
let argLoads = (allocatedArgs |> List.concat_map snd) in
let argOps = (allocatedArgs |> List.map fst) in
let callInstr = (LIR.Call (destReg, funcName, argOps)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
argLoads @ [callInstr] @ storeInstrs)
| LIR.TailCall (funcName, args) -> (let allocatedArgs = (args |> List.mapi (fun i arg ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping arg, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping arg tempReg)))) in
let argLoads = (allocatedArgs |> List.concat_map snd) in
let argOps = (allocatedArgs |> List.map fst) in
let callInstr = (LIR.TailCall (funcName, argOps)) in
argLoads @ [callInstr])
| LIR.IndirectCall (dest, func, args) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (funcReg, funcLoads) = (loadSpilled mapping func LIR.X14) in
let allocatedArgs = (args |> List.mapi (fun i arg ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping arg, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping arg tempReg)))) in
let argLoads = (allocatedArgs |> List.concat_map snd) in
let argOps = (allocatedArgs |> List.map fst) in
let callInstr = (LIR.IndirectCall (destReg, funcReg, argOps)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
funcLoads @ argLoads @ [callInstr] @ storeInstrs)
| LIR.IndirectTailCall (func, args) -> (let (funcReg, funcLoads) = (loadSpilled mapping func LIR.X14) in
let allocatedArgs = (args |> List.mapi (fun i arg ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping arg, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping arg tempReg)))) in
let argLoads = (allocatedArgs |> List.concat_map snd) in
let argOps = (allocatedArgs |> List.map fst) in
let callInstr = (LIR.IndirectTailCall (funcReg, argOps)) in
funcLoads @ argLoads @ [callInstr])
| LIR.ClosureAlloc (dest, funcName, captures) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let allocatedCaptures = (captures |> List.mapi (fun i cap ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping cap, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping cap tempReg)))) in
let capLoads = (allocatedCaptures |> List.concat_map snd) in
let capOps = (allocatedCaptures |> List.map fst) in
let allocInstr = (LIR.ClosureAlloc (destReg, funcName, capOps)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
capLoads @ [allocInstr] @ storeInstrs)
| LIR.ClosureCall (dest, closure, args) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (closureReg, closureLoads) = (loadSpilled mapping closure LIR.X14) in
let allocatedArgs = (args |> List.mapi (fun i arg ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping arg, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping arg tempReg)))) in
let argLoads = (allocatedArgs |> List.concat_map snd) in
let argOps = (allocatedArgs |> List.map fst) in
let callInstr = (LIR.ClosureCall (destReg, closureReg, argOps)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
closureLoads @ argLoads @ [callInstr] @ storeInstrs)
| LIR.ClosureTailCall (closure, args) -> (let (closureReg, closureLoads) = (loadSpilled mapping closure LIR.X14) in
let allocatedArgs = (args |> List.mapi (fun i arg ->
(if isX86_64 arch then ((applyToOperandNoLoad mapping arg, [])) else (let tempReg = (if i = 0 then LIR.X12 else LIR.X13) in
applyToOperand mapping arg tempReg)))) in
let argLoads = (allocatedArgs |> List.concat_map snd) in
let argOps = (allocatedArgs |> List.map fst) in
let callInstr = (LIR.ClosureTailCall (closureReg, argOps)) in
closureLoads @ argLoads @ [callInstr])
| LIR.SaveRegs (intRegs, floatRegs) -> ([LIR.SaveRegs (intRegs, floatRegs)])
| LIR.RestoreRegs (intRegs, floatRegs) -> ([LIR.RestoreRegs (intRegs, floatRegs)])
| LIR.ArgMoves moves -> (let allocatedMoves = (moves |> List.map (fun (destReg, srcOp) ->
let allocatedOp = (applyToOperandNoLoad mapping srcOp) in
(destReg, allocatedOp))) in
[LIR.ArgMoves allocatedMoves])
| LIR.TailArgMoves moves -> (let allocatedMoves = (moves |> List.map (fun (destReg, srcOp) ->
(destReg, applyToOperandNoLoad mapping srcOp))) in
[LIR.TailArgMoves allocatedMoves])
| LIR.FArgMoves moves -> ([LIR.FArgMoves moves])
| LIR.PrintInt64 reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintInt64 regFinal])
| LIR.PrintUInt64 reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintUInt64 regFinal])
| LIR.PrintBool reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintBool regFinal])
| LIR.PrintInt64NoNewline reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintInt64NoNewline regFinal])
| LIR.PrintUInt64NoNewline reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintUInt64NoNewline regFinal])
| LIR.PrintBoolNoNewline reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintBoolNoNewline regFinal])
| LIR.PrintFloatNoNewline freg -> ([LIR.PrintFloatNoNewline freg])
| LIR.PrintHeapStringNoNewline reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintHeapStringNoNewline regFinal])
| LIR.PrintList (listPtr, elemType) -> (let (ptrFinal, ptrLoads) = (loadSpilled mapping listPtr LIR.X12) in
ptrLoads @ [LIR.PrintList (ptrFinal, elemType)])
| LIR.PrintSum (sumPtr, variants, transparentPayload) -> (let (ptrFinal, ptrLoads) = (loadSpilled mapping sumPtr LIR.X12) in
ptrLoads @ [LIR.PrintSum (ptrFinal, variants, transparentPayload)])
| LIR.PrintRecord (recordPtr, typeName, fields) -> (let (ptrFinal, ptrLoads) = (loadSpilled mapping recordPtr LIR.X12) in
ptrLoads @ [LIR.PrintRecord (ptrFinal, typeName, fields)])
| LIR.PrintFloat freg -> ([LIR.PrintFloat freg])
| LIR.PrintString value -> ([LIR.PrintString value])
| LIR.StdoutWrite (effectId, value, appendNewline) -> (let (valueOp, valueLoads) = (applyToOperand mapping value LIR.X12) in
valueLoads @ [LIR.StdoutWrite (effectId, valueOp, appendNewline)])
| LIR.StdinReadLine (effectId, dest) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[LIR.StdinReadLine (effectId, destReg)] @ storeInstrs)
| LIR.RuntimeError message -> ([LIR.RuntimeError message])
| LIR.RuntimeErrorString reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.RuntimeErrorString regFinal])
| LIR.PrintChars chars -> ([LIR.PrintChars chars])
| LIR.PrintBlob reg -> (let (regFinal, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintBlob regFinal])
| LIR.FMov (dest, src) -> ([LIR.FMov (dest, src)])
| LIR.FLoad (dest, value) -> ([LIR.FLoad (dest, value)])
| LIR.FAdd (dest, left, right) -> ([LIR.FAdd (dest, left, right)])
| LIR.FSub (dest, left, right) -> ([LIR.FSub (dest, left, right)])
| LIR.FMul (dest, left, right) -> ([LIR.FMul (dest, left, right)])
| LIR.FMadd (dest, left, right, addend) -> ([LIR.FMadd (dest, left, right, addend)])
| LIR.FDiv (dest, left, right) -> ([LIR.FDiv (dest, left, right)])
| LIR.FNeg (dest, src) -> ([LIR.FNeg (dest, src)])
| LIR.FAbs (dest, src) -> ([LIR.FAbs (dest, src)])
| LIR.FSqrt (dest, src) -> ([LIR.FSqrt (dest, src)])
| LIR.FCmp (left, right) -> ([LIR.FCmp (left, right)])
| LIR.Int64ToFloat (dest, src) -> (let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
srcLoads @ [LIR.Int64ToFloat (dest, srcReg)])
| LIR.GpToFp (dest, src) -> (let (srcReg, srcLoads) = (loadSpilled mapping src LIR.X12) in
srcLoads @ [LIR.GpToFp (dest, srcReg)])
| LIR.FloatToInt64 (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let instr = (LIR.FloatToInt64 (destReg, src)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[instr] @ storeInstrs)
| LIR.FpToGp (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let instr = (LIR.FpToGp (destReg, src)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[instr] @ storeInstrs)
| LIR.FloatToBits (dest, src) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let instr = (LIR.FloatToBits (destReg, src)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[instr] @ storeInstrs)
| LIR.HeapAlloc (dest, size) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let allocInstr = (LIR.HeapAlloc (destReg, size)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[allocInstr] @ storeInstrs)
| LIR.HeapStore (addr, offset, src, vt) -> (let (addrReg, addrLoads) = (loadSpilled mapping addr LIR.X12) in
let (srcOp, srcLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping src, [])) else (applyToOperand mapping src LIR.X13))) in
addrLoads @ srcLoads @ [LIR.HeapStore (addrReg, offset, srcOp, vt)])
| LIR.HeapLoad (dest, addr, offset) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (addrReg, addrLoads) = (loadSpilled mapping addr LIR.X12) in
let loadInstr = (LIR.HeapLoad (destReg, addrReg, offset)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
addrLoads @ [loadInstr] @ storeInstrs)
| LIR.RefCountInc (addr, payloadSize, kind, sourceType) -> (let (addrReg, addrLoads) = (loadSpilled mapping addr LIR.X12) in
addrLoads @ [LIR.RefCountInc (addrReg, payloadSize, kind, sourceType)])
| LIR.RefCountDec (addr, payloadSize, kind, sourceType) -> (let (addrReg, addrLoads) = (loadSpilled mapping addr LIR.X12) in
addrLoads @ [LIR.RefCountDec (addrReg, payloadSize, kind, sourceType)])
| LIR.StringConcat (dest, first, second, remaining) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (concatInstr, operandLoads) = ((match remaining with
| [] -> (let (firstOp, firstLoads) = (applyToOperand mapping first LIR.X12) in
let (secondOp, secondLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping second, [])) else (applyToOperand mapping second LIR.X13))) in
(LIR.StringConcat (destReg, firstOp, secondOp, []), firstLoads @ secondLoads))
| _ -> (let operands = (first :: second :: remaining |> List.map (applyToOperandNoLoad mapping)) in
(match operands with
| firstOp :: secondOp :: remainingOps -> ((LIR.StringConcat (destReg, firstOp, secondOp, remainingOps), []))
| _ -> (Crash.crash "StringConcat lost its required operands"))))) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
operandLoads @ (concatInstr :: storeInstrs))
| LIR.CanonicalBufferEq (dest, kind, left, right) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((leftOp, leftLoads), (rightOp, rightLoads)) = ((match left, right with
| LIR.Reg leftReg, LIR.Reg rightReg -> (let ((allocatedLeft, leftLoads), (allocatedRight, rightLoads)) = (loadSpilledPair arch mapping leftReg rightReg destReg) in
((LIR.Reg allocatedLeft, leftLoads), (LIR.Reg allocatedRight, rightLoads)))
| _ -> ((applyToOperand mapping left LIR.X12, applyToOperand mapping right LIR.X13)))) in
let eqInstr = (LIR.CanonicalBufferEq (destReg, kind, leftOp, rightOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
leftLoads @ rightLoads @ [eqInstr] @ storeInstrs)
| LIR.PrintHeapString reg -> (let (regPhys, regLoads) = (loadSpilled mapping reg LIR.X12) in
regLoads @ [LIR.PrintHeapString regPhys])
| LIR.LoadFuncAddr (dest, funcName) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let loadInstr = (LIR.LoadFuncAddr (destReg, funcName)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[loadInstr] @ storeInstrs)
| LIR.FileReadBlob (dest, path) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let fileInstr = (LIR.FileReadBlob (destReg, pathOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ [fileInstr] @ storeInstrs)
| LIR.FileExists (dest, path) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let fileInstr = (LIR.FileExists (destReg, pathOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ [fileInstr] @ storeInstrs)
| LIR.FileWriteBlob (dest, path, content) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let (contentOp, contentLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping content, [])) else (applyToOperand mapping content LIR.X13))) in
let fileInstr = (LIR.FileWriteBlob (destReg, pathOp, contentOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ contentLoads @ [fileInstr] @ storeInstrs)
| LIR.FileAppendText (dest, path, content) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let (contentOp, contentLoads) = ((if isX86_64 arch then ((applyToOperandNoLoad mapping content, [])) else (applyToOperand mapping content LIR.X13))) in
let fileInstr = (LIR.FileAppendText (destReg, pathOp, contentOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ contentLoads @ [fileInstr] @ storeInstrs)
| LIR.FileDelete (dest, path) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let fileInstr = (LIR.FileDelete (destReg, pathOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ [fileInstr] @ storeInstrs)
| LIR.FileCreateDirectory (dest, path) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let fileInstr = (LIR.FileCreateDirectory (destReg, pathOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ [fileInstr] @ storeInstrs)
| LIR.FileSetExecutable (dest, path) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
let fileInstr = (LIR.FileSetExecutable (destReg, pathOp)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ [fileInstr] @ storeInstrs)
| LIR.FileWriteFromPtr (dest, path, ptr, length) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (pathOp, pathLoads) = (applyToOperand mapping path LIR.X12) in
(if isX86_64 arch then (let ((ptrReg, ptrLoads), (lengthReg, lengthLoads)) = (loadSpilledPair arch mapping ptr length destReg) in
let fileInstr = (LIR.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X13) in
let (lengthReg, lengthLoads) = (loadSpilled mapping length LIR.X14) in
let fileInstr = (LIR.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs)))
| LIR.RawAlloc (dest, numBytes) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (numBytesReg, numBytesLoads) = (loadSpilled mapping numBytes LIR.X12) in
let allocInstr = (LIR.RawAlloc (destReg, numBytesReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
numBytesLoads @ [allocInstr] @ storeInstrs)
| LIR.MappedAlloc (dest, numBytes) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let (numBytesReg, numBytesLoads) = (loadSpilled mapping numBytes LIR.X12) in
let allocInstr = (LIR.MappedAlloc (destReg, numBytesReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
numBytesLoads @ [allocInstr] @ storeInstrs)
| LIR.RawFree ptr -> (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
ptrLoads @ [LIR.RawFree ptrReg])
| LIR.MappedFree ptr -> (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
ptrLoads @ [LIR.MappedFree ptrReg])
| LIR.RawGet (dest, ptr, byteOffset) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((ptrReg, ptrLoads), (offsetReg, offsetLoads)) = (loadSpilledPair arch mapping ptr byteOffset destReg) in
let getInstr = (LIR.RawGet (destReg, ptrReg, offsetReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs)
| LIR.RawGetByte (dest, ptr, byteOffset) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let ((ptrReg, ptrLoads), (offsetReg, offsetLoads)) = (loadSpilledPair arch mapping ptr byteOffset destReg) in
let getInstr = (LIR.RawGetByte (destReg, ptrReg, offsetReg)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs)
| LIR.RawWriteWord (ptr, byteOffset, value) -> ((if isX86_64 arch then (let ptrSpilled = ((match ptr with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
let valueSpilled = ((match value with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
(if ptrSpilled && valueSpilled then (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X3) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X12) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X12) in
[LIR.SaveRegs ([LIR.X3], [])] @ ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)] @ [LIR.RestoreRegs ([LIR.X3], [])]) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X12) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X12) in
ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)]))) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X13) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X14) in
ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteWord (ptrReg, offsetReg, valueReg)])))
| LIR.RawWriteByte (ptr, byteOffset, value) -> ((if isX86_64 arch then (let ptrSpilled = ((match ptr with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
let valueSpilled = ((match value with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
(if ptrSpilled && valueSpilled then (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X3) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X12) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X12) in
[LIR.SaveRegs ([LIR.X3], [])] @ ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)] @ [LIR.RestoreRegs ([LIR.X3], [])]) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X12) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X12) in
ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)]))) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X13) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X14) in
ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawWriteByte (ptrReg, offsetReg, valueReg)])))
| LIR.RawSlotInit (ptr, byteOffset, value, valueType) -> ((if isX86_64 arch then (let ptrSpilled = ((match ptr with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
let valueSpilled = ((match value with
| LIR.Virtual id -> ((match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false))
| _ -> (false))) in
(if ptrSpilled && valueSpilled then (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X3) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X12) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X12) in
[LIR.SaveRegs ([LIR.X3], [])] @ ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)] @ [LIR.RestoreRegs ([LIR.X3], [])]) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X12) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X12) in
ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)]))) else (let (ptrReg, ptrLoads) = (loadSpilled mapping ptr LIR.X12) in
let (offsetReg, offsetLoads) = (loadSpilled mapping byteOffset LIR.X13) in
let (valueReg, valueLoads) = (loadSpilled mapping value LIR.X14) in
ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSlotInit (ptrReg, offsetReg, valueReg, valueType)])))
| LIR.RefCountIncString str -> (let (strOp, strLoads) = (applyToOperand mapping str LIR.X12) in
strLoads @ [LIR.RefCountIncString strOp])
| LIR.RefCountDecString str -> (let (strOp, strLoads) = (applyToOperand mapping str LIR.X12) in
strLoads @ [LIR.RefCountDecString strOp])
| LIR.RefCountIncBlob bytes -> (let (bytesOp, bytesLoads) = (applyToOperand mapping bytes LIR.X12) in
bytesLoads @ [LIR.RefCountIncBlob bytesOp])
| LIR.RefCountDecBlob bytes -> (let (bytesOp, bytesLoads) = (applyToOperand mapping bytes LIR.X12) in
bytesLoads @ [LIR.RefCountDecBlob bytesOp])
| LIR.RefCountIncInt value -> (let (valueOp, valueLoads) = (applyToOperand mapping value LIR.X12) in
valueLoads @ [LIR.RefCountIncInt valueOp])
| LIR.RefCountDecInt value -> (let (valueOp, valueLoads) = (applyToOperand mapping value LIR.X12) in
valueLoads @ [LIR.RefCountDecInt valueOp])
| LIR.RandomInt64 dest -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let randomInstr = (LIR.RandomInt64 destReg) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[randomInstr] @ storeInstrs)
| LIR.DateTimeNow dest -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let dateInstr = (LIR.DateTimeNow destReg) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[dateInstr] @ storeInstrs)
| LIR.Sleep _ -> ([instr])
| LIR.CliNative (dest, operation, args) -> (let scratchRegs = ([LIR.X12; LIR.X13; LIR.X14]) in
let resolvedArgs = (args |> List.mapi (fun index arg ->
let scratch = (List.nth scratchRegs (min index 2)) in
applyToOperand mapping arg scratch)) in
let argLoads = (resolvedArgs |> List.concat_map snd) in
let args' = (resolvedArgs |> List.map fst) in
let (destReg, destAlloc) = (applyToReg mapping dest) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
argLoads @ [LIR.CliNative (destReg, operation, args')] @ storeInstrs)
| LIR.FloatToString (dest, value) -> (let (destReg, destAlloc) = (applyToReg mapping dest) in
let floatToStrInstr = (LIR.FloatToString (destReg, value)) in
let storeInstrs = ((match destAlloc with
| Some (StackSlot offset) -> ([LIR.Store (offset, LIR.Physical LIR.X11)])
| _ -> ([]))) in
[floatToStrInstr] @ storeInstrs)
| LIR.CoverageHit exprId -> ([LIR.CoverageHit exprId])
| LIR.Exit -> ([LIR.Exit]))
