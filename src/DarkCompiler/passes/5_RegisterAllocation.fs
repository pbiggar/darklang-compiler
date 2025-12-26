// 5_RegisterAllocation.fs - Register Allocation with Liveness Analysis (Pass 5)
//
// Allocates physical ARM64 registers to virtual registers in LIR CFG.
//
// Algorithm:
// 1. Compute liveness information using backward dataflow analysis
// 2. Build live intervals for each virtual register
// 3. Use linear scan algorithm to allocate registers
// 4. Spill to stack when registers are exhausted
//
// Registers:
// - X0: reserved for return values
// - X1-X10: caller-saved (preferred for allocation)
// - X11-X13: reserved as scratch registers for spill code
// - X19-X26: callee-saved (used when caller-saved exhausted)
// - X27: reserved for free list base pointer
// - X28: reserved for heap bump pointer
// - X29: frame pointer
// - X30: link register
//
// Callee-saved registers (X19-X26) are saved/restored in function prologue/epilogue.

module RegisterAllocation

// ============================================================================
// Types
// ============================================================================

/// Result of register allocation
type AllocationResult = {
    Mapping: Map<int, Allocation>
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
}

/// Allocation target for a virtual register
and Allocation =
    | PhysReg of LIR.PhysReg
    | StackSlot of int

/// Live interval for a virtual register
type LiveInterval = {
    VRegId: int
    Start: int
    End: int
}

/// Liveness information for a basic block
type BlockLiveness = {
    LiveIn: Set<int>
    LiveOut: Set<int>
}

// ============================================================================
// Liveness Analysis
// ============================================================================

/// Get virtual register IDs used (read) by an instruction
let getUsedVRegs (instr: LIR.Instr) : Set<int> =
    let regToVReg (reg: LIR.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    let operandToVReg (op: LIR.Operand) : int option =
        match op with
        | LIR.Reg reg -> regToVReg reg
        | _ -> None

    match instr with
    | LIR.Mov (_, src) ->
        operandToVReg src |> Option.toList |> Set.ofList
    | LIR.Store (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIR.Add (_, left, right) | LIR.Sub (_, left, right) ->
        let l = regToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.Mul (_, left, right) | LIR.Sdiv (_, left, right)
    | LIR.And (_, left, right) | LIR.Orr (_, left, right) ->
        let l = regToVReg left |> Option.toList
        let r = regToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.Cmp (left, right) ->
        let l = regToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.Cset (_, _) -> Set.empty
    | LIR.Mvn (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIR.Call (_, _, args) ->
        args |> List.choose operandToVReg |> Set.ofList
    | LIR.IndirectCall (_, func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (funcVReg @ argsVRegs)
    | LIR.PrintInt reg | LIR.PrintBool reg ->
        regToVReg reg |> Option.toList |> Set.ofList
    | LIR.HeapAlloc (_, _) -> Set.empty
    | LIR.HeapStore (addr, _, src) ->
        let a = regToVReg addr |> Option.toList
        let s = operandToVReg src |> Option.toList
        Set.ofList (a @ s)
    | LIR.HeapLoad (_, addr, _) ->
        regToVReg addr |> Option.toList |> Set.ofList
    | LIR.RefCountInc (addr, _) ->
        regToVReg addr |> Option.toList |> Set.ofList
    | LIR.RefCountDec (addr, _) ->
        regToVReg addr |> Option.toList |> Set.ofList
    | LIR.StringConcat (_, left, right) ->
        let l = operandToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.PrintHeapString reg ->
        regToVReg reg |> Option.toList |> Set.ofList
    | _ -> Set.empty

/// Get virtual register ID defined (written) by an instruction
let getDefinedVReg (instr: LIR.Instr) : int option =
    let regToVReg (reg: LIR.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    match instr with
    | LIR.Mov (dest, _) -> regToVReg dest
    | LIR.Add (dest, _, _) | LIR.Sub (dest, _, _) -> regToVReg dest
    | LIR.Mul (dest, _, _) | LIR.Sdiv (dest, _, _) -> regToVReg dest
    | LIR.Cset (dest, _) -> regToVReg dest
    | LIR.And (dest, _, _) | LIR.Orr (dest, _, _) -> regToVReg dest
    | LIR.Mvn (dest, _) -> regToVReg dest
    | LIR.Call (dest, _, _) -> regToVReg dest
    | LIR.IndirectCall (dest, _, _) -> regToVReg dest
    | LIR.HeapAlloc (dest, _) -> regToVReg dest
    | LIR.HeapLoad (dest, _, _) -> regToVReg dest
    | LIR.StringConcat (dest, _, _) -> regToVReg dest
    | _ -> None

/// Get virtual register used by terminator
let getTerminatorUsedVRegs (term: LIR.Terminator) : Set<int> =
    match term with
    | LIR.Branch (LIR.Virtual id, _, _) -> Set.singleton id
    | _ -> Set.empty

/// Get successor labels for a terminator
let getSuccessors (term: LIR.Terminator) : LIR.Label list =
    match term with
    | LIR.Ret -> []
    | LIR.Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | LIR.Jump label -> [label]

/// Compute GEN and KILL sets for a basic block
/// GEN = variables used before being defined
/// KILL = variables defined
let computeGenKill (block: LIR.BasicBlock) : Set<int> * Set<int> =
    // Process instructions in forward order
    let mutable gen = Set.empty
    let mutable kill = Set.empty

    for instr in block.Instrs do
        let used = getUsedVRegs instr
        let defined = getDefinedVReg instr

        // Add to GEN if used and not already killed (defined earlier in block)
        for u in used do
            if not (Set.contains u kill) then
                gen <- Set.add u gen

        // Add to KILL if defined
        match defined with
        | Some d -> kill <- Set.add d kill
        | None -> ()

    // Also add terminator uses to GEN
    let termUses = getTerminatorUsedVRegs block.Terminator
    for u in termUses do
        if not (Set.contains u kill) then
            gen <- Set.add u gen

    (gen, kill)

/// Compute liveness using backward dataflow analysis
let computeLiveness (cfg: LIR.CFG) : Map<LIR.Label, BlockLiveness> =
    // Initialize with empty sets
    let mutable liveness : Map<LIR.Label, BlockLiveness> =
        cfg.Blocks
        |> Map.map (fun _ _ -> { LiveIn = Set.empty; LiveOut = Set.empty })

    // Precompute GEN and KILL for each block
    let genKill =
        cfg.Blocks
        |> Map.map (fun _ block -> computeGenKill block)

    // Iterate until fixed point
    let mutable changed = true
    while changed do
        changed <- false
        for kvp in cfg.Blocks do
            let label = kvp.Key
            let block = kvp.Value
            let (gen, kill) = Map.find label genKill
            let oldLiveness = Map.find label liveness

            // LiveOut = union of LiveIn of all successors
            let successors = getSuccessors block.Terminator
            let newLiveOut =
                successors
                |> List.fold (fun acc succLabel ->
                    match Map.tryFind succLabel liveness with
                    | Some succ -> Set.union acc succ.LiveIn
                    | None -> acc
                ) Set.empty

            // LiveIn = GEN ∪ (LiveOut - KILL)
            let newLiveIn = Set.union gen (Set.difference newLiveOut kill)

            if newLiveIn <> oldLiveness.LiveIn || newLiveOut <> oldLiveness.LiveOut then
                changed <- true
                liveness <- Map.add label { LiveIn = newLiveIn; LiveOut = newLiveOut } liveness

    liveness

// ============================================================================
// Live Interval Construction
// ============================================================================

/// Build live intervals from CFG and liveness information
/// Uses a simpler approach: for each vreg, find first definition and last use
let buildLiveIntervals (cfg: LIR.CFG) (liveness: Map<LIR.Label, BlockLiveness>) : LiveInterval list =
    let mutable intervals : Map<int, int * int> = Map.empty  // VRegId -> (start, end)
    let mutable globalIdx = 0

    // Process blocks in BFS order from entry
    let mutable visited = Set.empty
    let mutable queue = [cfg.Entry]

    while not (List.isEmpty queue) do
        match queue with
        | [] -> ()
        | label :: rest ->
            queue <- rest
            if not (Set.contains label visited) then
                visited <- Set.add label visited
                match Map.tryFind label cfg.Blocks with
                | Some block ->
                    let blockStart = globalIdx
                    let blockLiveness = Map.find label liveness

                    // Variables live-in extend their intervals to this block
                    for vregId in blockLiveness.LiveIn do
                        match Map.tryFind vregId intervals with
                        | Some (s, e) -> intervals <- Map.add vregId (min s blockStart, max e blockStart) intervals
                        | None -> intervals <- Map.add vregId (blockStart, blockStart) intervals

                    // Process each instruction
                    for instr in block.Instrs do
                        globalIdx <- globalIdx + 1
                        let used = getUsedVRegs instr
                        let defined = getDefinedVReg instr

                        // Uses extend the interval
                        for vregId in used do
                            match Map.tryFind vregId intervals with
                            | Some (s, e) -> intervals <- Map.add vregId (s, max e globalIdx) intervals
                            | None -> intervals <- Map.add vregId (globalIdx, globalIdx) intervals

                        // Definitions start or extend the interval
                        match defined with
                        | Some vregId ->
                            match Map.tryFind vregId intervals with
                            | Some (s, e) -> intervals <- Map.add vregId (min s globalIdx, max e globalIdx) intervals
                            | None -> intervals <- Map.add vregId (globalIdx, globalIdx) intervals
                        | None -> ()

                    // Terminator position
                    globalIdx <- globalIdx + 1
                    let termUses = getTerminatorUsedVRegs block.Terminator
                    for vregId in termUses do
                        match Map.tryFind vregId intervals with
                        | Some (s, e) -> intervals <- Map.add vregId (s, max e globalIdx) intervals
                        | None -> intervals <- Map.add vregId (globalIdx, globalIdx) intervals

                    // Variables live-out extend their intervals to block end
                    for vregId in blockLiveness.LiveOut do
                        match Map.tryFind vregId intervals with
                        | Some (s, e) -> intervals <- Map.add vregId (s, max e globalIdx) intervals
                        | None -> intervals <- Map.add vregId (globalIdx, globalIdx) intervals

                    // Add successors to queue
                    let succs = getSuccessors block.Terminator
                    queue <- queue @ succs
                | None -> ()

    // Convert to list sorted by start position
    intervals
    |> Map.toList
    |> List.map (fun (vregId, (s, e)) -> { VRegId = vregId; Start = s; End = e })
    |> List.sortBy (fun i -> i.Start)

// ============================================================================
// Linear Scan Register Allocation
// ============================================================================

/// Caller-saved registers (X1-X10) - preferred for allocation
let callerSavedRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5
    LIR.X6; LIR.X7; LIR.X8; LIR.X9; LIR.X10
]

/// Callee-saved registers (X19-X26) - used when caller-saved exhausted
/// These must be saved/restored in function prologue/epilogue
/// Note: X27 and X28 are reserved for free list base and heap pointer respectively
let calleeSavedRegs = [
    LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23
    LIR.X24; LIR.X25; LIR.X26
]

/// All allocatable registers - caller-saved first (preferred), then callee-saved
let allocatableRegs = callerSavedRegs @ calleeSavedRegs

/// Check if a register is callee-saved
let isCalleeSaved (reg: LIR.PhysReg) : bool =
    List.contains reg calleeSavedRegs

/// Align to 16-byte boundary
let alignTo16 (size: int) : int =
    ((size + 15) / 16) * 16

/// Linear scan register allocation
let linearScan (intervals: LiveInterval list) : AllocationResult =
    let mutable active : (LiveInterval * LIR.PhysReg) list = []
    let mutable freeRegs = Set.ofList allocatableRegs
    let mutable nextStackSlot = -8
    let mutable mapping = Map.empty
    let mutable usedCalleeSaved = Set.empty

    /// Expire old intervals that are no longer active
    let expireOldIntervals (currentStart: int) =
        let (expired, stillActive) =
            active |> List.partition (fun (interval, _) -> interval.End < currentStart)

        for (_, reg) in expired do
            freeRegs <- Set.add reg freeRegs

        active <- stillActive

    /// Spill an interval to stack
    let spillToStack (vregId: int) =
        let slot = nextStackSlot
        nextStackSlot <- nextStackSlot - 8
        mapping <- Map.add vregId (StackSlot slot) mapping
        slot

    /// Track callee-saved register usage
    let trackCalleeSaved (reg: LIR.PhysReg) =
        if isCalleeSaved reg then
            usedCalleeSaved <- Set.add reg usedCalleeSaved

    // Process intervals in order of start position
    for interval in intervals do
        expireOldIntervals interval.Start

        if Set.isEmpty freeRegs then
            // Need to spill - spill the interval that ends latest
            let allCandidates = (interval, None) :: (active |> List.map (fun (i, r) -> (i, Some r)))
            let toSpill = allCandidates |> List.maxBy (fun (i, _) -> i.End)

            match toSpill with
            | (i, None) when i.VRegId = interval.VRegId ->
                // Spill current interval
                spillToStack interval.VRegId |> ignore
            | (i, Some reg) ->
                // Spill the one with latest end, reassign its register to current
                mapping <- Map.add i.VRegId (StackSlot nextStackSlot) mapping
                nextStackSlot <- nextStackSlot - 8
                mapping <- Map.add interval.VRegId (PhysReg reg) mapping
                active <- (interval, reg) :: (active |> List.filter (fun (x, _) -> x.VRegId <> i.VRegId))
                trackCalleeSaved reg
            | _ ->
                spillToStack interval.VRegId |> ignore
        else
            // Allocate a free register
            let reg = Set.minElement freeRegs
            freeRegs <- Set.remove reg freeRegs
            mapping <- Map.add interval.VRegId (PhysReg reg) mapping
            active <- (interval, reg) :: active
            trackCalleeSaved reg

    // Compute stack size (16-byte aligned)
    let stackSize =
        if nextStackSlot = -8 then 0
        else alignTo16 (abs nextStackSlot)

    {
        Mapping = mapping
        StackSize = stackSize
        UsedCalleeSaved = Set.toList usedCalleeSaved
    }

// ============================================================================
// Apply Allocation to LIR
// ============================================================================

/// Apply allocation to a register, returning the physical register and allocation info
let applyToReg (mapping: Map<int, Allocation>) (reg: LIR.Reg) : LIR.Reg * Allocation option =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, None)
    | LIR.Virtual id ->
        match Map.tryFind id mapping with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, None)
        | Some (StackSlot offset) -> (LIR.Physical LIR.X11, Some (StackSlot offset))
        | None -> (LIR.Physical LIR.X11, None)

/// Apply allocation to an operand, returning load instructions if needed
let applyToOperand (mapping: Map<int, Allocation>) (operand: LIR.Operand) (tempReg: LIR.PhysReg)
    : LIR.Operand * LIR.Instr list =
    match operand with
    | LIR.Imm n -> (LIR.Imm n, [])
    | LIR.FloatImm f -> (LIR.FloatImm f, [])
    | LIR.StringRef idx -> (LIR.StringRef idx, [])
    | LIR.FloatRef idx -> (LIR.FloatRef idx, [])
    | LIR.StackSlot s -> (LIR.StackSlot s, [])
    | LIR.Reg reg ->
        match reg with
        | LIR.Physical p -> (LIR.Reg (LIR.Physical p), [])
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) -> (LIR.Reg (LIR.Physical physReg), [])
            | Some (StackSlot offset) ->
                let loadInstr = LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)
                (LIR.Reg (LIR.Physical tempReg), [loadInstr])
            | None -> (LIR.Reg (LIR.Physical tempReg), [])
    | LIR.FuncAddr name -> (LIR.FuncAddr name, [])

/// Helper to load a spilled register
let loadSpilled (mapping: Map<int, Allocation>) (reg: LIR.Reg) (tempReg: LIR.PhysReg)
    : LIR.Reg * LIR.Instr list =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, [])
    | LIR.Virtual id ->
        match Map.tryFind id mapping with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, [])
        | Some (StackSlot offset) ->
            let loadInstr = LIR.Mov (LIR.Physical tempReg, LIR.StackSlot offset)
            (LIR.Physical tempReg, [loadInstr])
        | None -> (LIR.Physical tempReg, [])

/// Apply allocation to an instruction
let applyToInstr (mapping: Map<int, Allocation>) (instr: LIR.Instr) : LIR.Instr list =
    match instr with
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
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let addInstr = LIR.Add (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [addInstr] @ storeInstrs

    | LIR.Sub (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let subInstr = LIR.Sub (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [subInstr] @ storeInstrs

    | LIR.Mul (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let mulInstr = LIR.Mul (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [mulInstr] @ storeInstrs

    | LIR.Sdiv (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let divInstr = LIR.Sdiv (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [divInstr] @ storeInstrs

    | LIR.Cmp (left, right) ->
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
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
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let andInstr = LIR.And (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [andInstr] @ storeInstrs

    | LIR.Orr (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let orrInstr = LIR.Orr (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [orrInstr] @ storeInstrs

    | LIR.Mvn (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let mvnInstr = LIR.Mvn (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [mvnInstr] @ storeInstrs

    | LIR.Call (dest, funcName, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
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

    | LIR.IndirectCall (dest, func, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
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

    | LIR.SaveRegs -> [LIR.SaveRegs]
    | LIR.RestoreRegs -> [LIR.RestoreRegs]

    | LIR.PrintInt reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintInt regFinal]

    | LIR.PrintBool reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBool regFinal]

    | LIR.PrintFloat freg -> [LIR.PrintFloat freg]
    | LIR.PrintString (idx, len) -> [LIR.PrintString (idx, len)]

    // FP instructions pass through unchanged
    | LIR.FMov (dest, src) -> [LIR.FMov (dest, src)]
    | LIR.FLoad (dest, idx) -> [LIR.FLoad (dest, idx)]
    | LIR.FAdd (dest, left, right) -> [LIR.FAdd (dest, left, right)]
    | LIR.FSub (dest, left, right) -> [LIR.FSub (dest, left, right)]
    | LIR.FMul (dest, left, right) -> [LIR.FMul (dest, left, right)]
    | LIR.FDiv (dest, left, right) -> [LIR.FDiv (dest, left, right)]
    | LIR.FNeg (dest, src) -> [LIR.FNeg (dest, src)]
    | LIR.FCmp (left, right) -> [LIR.FCmp (left, right)]

    // Heap operations
    | LIR.HeapAlloc (dest, size) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocInstr = LIR.HeapAlloc (destReg, size)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [allocInstr] @ storeInstrs

    | LIR.HeapStore (addr, offset, src) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let (srcOp, srcLoads) = applyToOperand mapping src LIR.X13
        addrLoads @ srcLoads @ [LIR.HeapStore (addrReg, offset, srcOp)]

    | LIR.HeapLoad (dest, addr, offset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let loadInstr = LIR.HeapLoad (destReg, addrReg, offset)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        addrLoads @ [loadInstr] @ storeInstrs

    | LIR.RefCountInc (addr, payloadSize) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIR.RefCountInc (addrReg, payloadSize)]

    | LIR.RefCountDec (addr, payloadSize) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIR.RefCountDec (addrReg, payloadSize)]

    | LIR.StringConcat (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftOp, leftLoads) = applyToOperand mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let concatInstr = LIR.StringConcat (destReg, leftOp, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [concatInstr] @ storeInstrs

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

    | LIR.Exit -> [LIR.Exit]

/// Apply allocation to terminator
let applyToTerminator (mapping: Map<int, Allocation>) (term: LIR.Terminator)
    : LIR.Instr list * LIR.Terminator =
    match term with
    | LIR.Ret -> ([], LIR.Ret)
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        match cond with
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
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
    | LIR.Jump label -> ([], LIR.Jump label)

/// Apply allocation to a basic block
let applyToBlock (mapping: Map<int, Allocation>) (block: LIR.BasicBlock) : LIR.BasicBlock =
    let allocatedInstrs = block.Instrs |> List.collect (applyToInstr mapping)
    let (termLoads, allocatedTerm) = applyToTerminator mapping block.Terminator
    {
        LIR.Label = block.Label
        LIR.Instrs = allocatedInstrs @ termLoads
        LIR.Terminator = allocatedTerm
    }

/// Apply allocation to CFG
let applyToCFG (mapping: Map<int, Allocation>) (cfg: LIR.CFG) : LIR.CFG =
    {
        LIR.Entry = cfg.Entry
        LIR.Blocks = cfg.Blocks |> Map.map (fun _ block -> applyToBlock mapping block)
    }

// ============================================================================
// Main Entry Point
// ============================================================================

/// Allocate registers for a function
let allocateRegisters (func: LIR.Function) : LIR.Function =
    // Step 1: Compute liveness
    let liveness = computeLiveness func.CFG

    // Step 2: Build live intervals
    let intervals = buildLiveIntervals func.CFG liveness

    // Step 3: Run linear scan allocation
    let result = linearScan intervals

    // Step 4: Apply allocation to CFG
    let allocatedCFG = applyToCFG result.Mapping func.CFG

    // Step 5: Apply allocation to parameters
    let allocatedParams =
        func.Params
        |> List.map (fun reg ->
            match reg with
            | LIR.Virtual id ->
                match Map.tryFind id result.Mapping with
                | Some (PhysReg physReg) -> LIR.Physical physReg
                | _ -> reg
            | LIR.Physical _ -> reg)

    {
        LIR.Name = func.Name
        LIR.Params = allocatedParams
        LIR.CFG = allocatedCFG
        LIR.StackSize = result.StackSize
        LIR.UsedCalleeSaved = result.UsedCalleeSaved
    }
