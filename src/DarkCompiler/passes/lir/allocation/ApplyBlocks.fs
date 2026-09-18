// ApplyBlocks.fs - Apply allocation and caller-save plans across CFG blocks.

module ApplyBlockAllocation

open AllocationModel
open RegisterFacts
open RegisterLiveness
open FloatAllocation
open SpillOperands
open ApplyRegisterAllocation

/// Apply allocation to terminator
let applyToTerminator (mapping: AllocationResult) (term: LIR.Terminator)
    : LIR.Instr list * LIR.Terminator =
    match term with
    | LIR.Ret -> ([], LIR.Ret)
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        match cond with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.Branch (LIR.Physical physReg, trueLabel, falseLabel))
            | Some (StackSlot offset) ->
                // Load condition from stack before branching
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel))
            | None ->
                ([], LIR.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel))
        | LIR.Physical p ->
            ([], LIR.Branch (LIR.Physical p, trueLabel, falseLabel))
    | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) ->
        match cond with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.BranchZero (LIR.Physical physReg, zeroLabel, nonZeroLabel))
            | Some (StackSlot offset) ->
                // Load condition from stack before branching
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel))
            | None ->
                ([], LIR.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel))
        | LIR.Physical p ->
            ([], LIR.BranchZero (LIR.Physical p, zeroLabel, nonZeroLabel))
    | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) ->
        match reg with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.BranchBitZero (LIR.Physical physReg, bit, zeroLabel, nonZeroLabel))
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.BranchBitZero (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel))
            | None ->
                ([], LIR.BranchBitZero (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel))
        | LIR.Physical p ->
            ([], LIR.BranchBitZero (LIR.Physical p, bit, zeroLabel, nonZeroLabel))
    | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) ->
        match reg with
        | LIR.Virtual id ->
            match tryAllocation mapping id with
            | Some (PhysReg physReg) ->
                ([], LIR.BranchBitNonZero (LIR.Physical physReg, bit, nonZeroLabel, zeroLabel))
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
                ([loadInstr], LIR.BranchBitNonZero (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel))
            | None ->
                ([], LIR.BranchBitNonZero (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel))
        | LIR.Physical p ->
            ([], LIR.BranchBitNonZero (LIR.Physical p, bit, nonZeroLabel, zeroLabel))
    | LIR.Jump label -> ([], LIR.Jump label)
    | LIR.CondBranch (cond, trueLabel, falseLabel) ->
        // CondBranch uses condition flags, not a register - pass through unchanged
        ([], LIR.CondBranch (cond, trueLabel, falseLabel))

type internal BlockAllocationPreparation = {
    SaveRegsLiveness: (BitSet * BitSet) list
    ArgMoveBackingRegs: LIR.PhysReg list list
}

let private prepareBlockAllocation
    (arch: Platform.Arch)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (liveOut: BitSet)
    (floatLiveOut: BitSet)
    (block: LIR.BasicBlock)
    (instrFacts: InstrRegisterFacts array)
    : BlockAllocationPreparation =
    let hasEmptySaveRegs = List.exists isEmptySaveRegs block.Instrs
    let (saveRegsLiveness, argMoveBackingRegs) =
        if hasEmptySaveRegs then
            computeSaveRegsPreparation
                (arch = Platform.ARM64)
                mapping.Domain
                floatAllocation.Domain
                mapping
                block
                instrFacts
                liveOut
                floatLiveOut
        else
            ([], [])

    { SaveRegsLiveness = saveRegsLiveness
      ArgMoveBackingRegs = argMoveBackingRegs }

/// Apply allocation to a basic block with precomputed SaveRegs/RestoreRegs data.
let private applyToPreparedBlock
    (arch: Platform.Arch)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (preparation: BlockAllocationPreparation)
    (block: LIR.BasicBlock)
    : LIR.BasicBlock =

    // Find SaveRegs/RestoreRegs pairs and compute the registers to save while
    // emitting allocated instructions directly. The old mapFold produced one
    // temporary list per input instruction, concatenated all of those lists,
    // and then traversed the result again for float allocation.
    let allocatedInstrs = ResizeArray<LIR.Instr>()
    let mutable savedRegsStack : (LIR.PhysReg list * LIR.PhysFPReg list) list = []
    let mutable remainingLiveness = preparation.SaveRegsLiveness
    let mutable remainingArgMoveBacking = preparation.ArgMoveBackingRegs

    let appendOneAllocated (instr: LIR.Instr) : unit =
        for allocated in applyFloatAllocationToInstrs floatAllocation instr do
            allocatedInstrs.Add(allocated)

    let appendAllocated (instrs: LIR.Instr list) : unit =
        for instr in instrs do
            appendOneAllocated instr

    for instr in block.Instrs do
        match instr with
        | LIR.SaveRegs ([], []) ->
            match remainingLiveness, remainingArgMoveBacking with
            | (liveAfter, floatLiveAfter) :: restLiveness,
              argMoveBacking :: restArgMoveBacking ->
                let liveCallerSaved = getLiveCallerSavedRegs mapping liveAfter
                let intRegs =
                    liveCallerSaved @ argMoveBacking
                    |> List.distinct
                    |> List.sort
                let liveCallerSavedFloat =
                    getLiveCallerSavedFloatRegs arch floatLiveAfter floatAllocation
                let regs = (intRegs, liveCallerSavedFloat)
                appendOneAllocated (LIR.SaveRegs regs)
                savedRegsStack <- regs :: savedRegsStack
                remainingLiveness <- restLiveness
                remainingArgMoveBacking <- restArgMoveBacking
            | [], _ ->
                Crash.crash "Missing liveness snapshot for SaveRegs"
            | _, [] ->
                Crash.crash "Missing argument-move backing for SaveRegs"
        | LIR.RestoreRegs ([], []) ->
            match savedRegsStack with
            | regs :: restSavedRegs ->
                appendOneAllocated (LIR.RestoreRegs regs)
                savedRegsStack <- restSavedRegs
            | [] ->
                Crash.crash "Unmatched RestoreRegs: SaveRegs stack is empty"
        | _ ->
            appendAllocated (applyToInstr arch mapping instr)

    if not (List.isEmpty remainingLiveness) then
        Crash.crash "Unused liveness snapshot for SaveRegs"

    if not (List.isEmpty remainingArgMoveBacking) then
        Crash.crash "Unused argument-move backing for SaveRegs"

    let (termLoads, allocatedTerm) = applyToTerminator mapping block.Terminator
    appendAllocated termLoads
    { Label = block.Label
      Instrs = allocatedInstrs |> Seq.toList
      Terminator = allocatedTerm }

/// Apply allocation to a basic block with liveness-aware SaveRegs/RestoreRegs population
let applyToBlockWithLiveness
    (arch: Platform.Arch)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (liveOut: BitSet)
    (floatLiveOut: BitSet)
    (block: LIR.BasicBlock)
    : LIR.BasicBlock =
    let instrFacts =
        (classifyBlocks [| block |]).[0].InstrFacts
    let preparation =
        prepareBlockAllocation arch mapping floatAllocation liveOut floatLiveOut block instrFacts
    applyToPreparedBlock arch mapping floatAllocation preparation block

let internal prepareCFGAllocation
    (arch: Platform.Arch)
    (blocks: LIR.BasicBlock array)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (liveness: BlockLiveness array)
    (floatLiveness: BlockLiveness array)
    (classifiedBlocks: ClassifiedBlock array)
    : BlockAllocationPreparation array =
    let emptyFloat = Bitset.empty floatAllocation.Domain.WordCount
    Array.init blocks.Length (fun idx ->
        let blockLiveness = liveness.[idx]
        let floatBlockLiveness =
            if idx < floatLiveness.Length then floatLiveness.[idx]
            else { LiveIn = emptyFloat; LiveOut = emptyFloat }
        prepareBlockAllocation
            arch
            mapping
            floatAllocation
            blockLiveness.LiveOut
            floatBlockLiveness.LiveOut
            blocks.[idx]
            classifiedBlocks.[idx].InstrFacts)

let internal applyPreparedCFGAllocation
    (arch: Platform.Arch)
    (blocks: LIR.BasicBlock array)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (preparations: BlockAllocationPreparation array)
    : LIR.BasicBlock array =
    Array.init blocks.Length (fun idx ->
        applyToPreparedBlock arch mapping floatAllocation preparations.[idx] blocks.[idx])

/// Apply allocation to CFG with liveness info
let applyToCFGWithLiveness
    (arch: Platform.Arch)
    (blocks: LIR.BasicBlock array)
    (mapping: AllocationResult)
    (floatAllocation: FAllocationResult)
    (liveness: BlockLiveness array)
    (floatLiveness: BlockLiveness array)
    : LIR.BasicBlock array =
    let classifiedBlocks = classifyBlocks blocks
    let preparations =
        prepareCFGAllocation
            arch
            blocks
            mapping
            floatAllocation
            liveness
            floatLiveness
            classifiedBlocks
    applyPreparedCFGAllocation arch blocks mapping floatAllocation preparations
