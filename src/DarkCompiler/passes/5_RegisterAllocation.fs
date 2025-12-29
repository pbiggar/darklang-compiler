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
// General-Purpose Registers:
// - X0: reserved for return values
// - X1-X8: caller-saved (preferred for allocation)
// - X9-X10: excluded (used by StringHash/StringEq internally)
// - X11-X13: reserved as scratch registers for spill code
// - X19-X26: callee-saved (used when caller-saved exhausted)
// - X27: reserved for free list base pointer
// - X28: reserved for heap bump pointer
// - X29: frame pointer
// - X30: link register
//
// Float Registers:
// - D0: float return value
// - D0-D7: caller-saved (saved around calls when live)
// - D8-D15: callee-saved
//
// Callee-saved registers (X19-X26, D8-D15) are saved/restored in prologue/epilogue.
//
// See docs/features/register-allocation.md for detailed documentation.

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
    | LIR.And (_, left, right) | LIR.Orr (_, left, right) | LIR.Eor (_, left, right)
    | LIR.Lsl (_, left, right) | LIR.Lsr (_, left, right) ->
        let l = regToVReg left |> Option.toList
        let r = regToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.Msub (_, mulLeft, mulRight, sub) ->
        let ml = regToVReg mulLeft |> Option.toList
        let mr = regToVReg mulRight |> Option.toList
        let s = regToVReg sub |> Option.toList
        Set.ofList (ml @ mr @ s)
    | LIR.Cmp (left, right) ->
        let l = regToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.Cset (_, _) -> Set.empty
    | LIR.Mvn (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIR.Call (_, _, args) ->
        args |> List.choose operandToVReg |> Set.ofList
    | LIR.TailCall (_, args) ->
        args |> List.choose operandToVReg |> Set.ofList
    | LIR.IndirectCall (_, func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (funcVReg @ argsVRegs)
    | LIR.IndirectTailCall (func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (funcVReg @ argsVRegs)
    | LIR.ClosureAlloc (_, _, captures) ->
        captures |> List.choose operandToVReg |> Set.ofList
    | LIR.ClosureCall (_, closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (closureVReg @ argsVRegs)
    | LIR.ClosureTailCall (closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (closureVReg @ argsVRegs)
    | LIR.PrintInt reg | LIR.PrintBool reg
    | LIR.PrintIntNoNewline reg | LIR.PrintBoolNoNewline reg
    | LIR.PrintHeapStringNoNewline reg | LIR.PrintList (reg, _)
    | LIR.PrintSum (reg, _) | LIR.PrintRecord (reg, _, _) ->
        regToVReg reg |> Option.toList |> Set.ofList
    | LIR.PrintFloatNoNewline _ -> Set.empty  // FP register, not GP
    | LIR.PrintChars _ -> Set.empty  // No registers used
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
    | LIR.FileReadText (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIR.FileExists (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIR.FileWriteText (_, path, content) ->
        let p = operandToVReg path |> Option.toList
        let c = operandToVReg content |> Option.toList
        Set.ofList (p @ c)
    | LIR.FileAppendText (_, path, content) ->
        let p = operandToVReg path |> Option.toList
        let c = operandToVReg content |> Option.toList
        Set.ofList (p @ c)
    | LIR.FileDelete (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIR.FileSetExecutable (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIR.FileWriteFromPtr (_, path, ptr, length) ->
        let p = operandToVReg path |> Option.toList
        let ptr' = regToVReg ptr |> Option.toList
        let len = regToVReg length |> Option.toList
        Set.ofList (p @ ptr' @ len)
    | LIR.RawAlloc (_, numBytes) ->
        regToVReg numBytes |> Option.toList |> Set.ofList
    | LIR.RawFree ptr ->
        regToVReg ptr |> Option.toList |> Set.ofList
    | LIR.RawGet (_, ptr, byteOffset) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        Set.ofList (p @ o)
    | LIR.RawGetByte (_, ptr, byteOffset) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        Set.ofList (p @ o)
    | LIR.RawSet (ptr, byteOffset, value) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        let v = regToVReg value |> Option.toList
        Set.ofList (p @ o @ v)
    | LIR.RawSetByte (ptr, byteOffset, value) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        let v = regToVReg value |> Option.toList
        Set.ofList (p @ o @ v)
    // IntToFloat uses an integer source register
    | LIR.IntToFloat (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIR.StringHash (_, str) ->
        operandToVReg str |> Option.toList |> Set.ofList
    | LIR.StringEq (_, left, right) ->
        let l = operandToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIR.RefCountIncString str ->
        operandToVReg str |> Option.toList |> Set.ofList
    | LIR.RefCountDecString str ->
        operandToVReg str |> Option.toList |> Set.ofList
    | LIR.RandomInt64 _ ->
        Set.empty  // No operands to read
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
    | LIR.Mul (dest, _, _) | LIR.Sdiv (dest, _, _) | LIR.Msub (dest, _, _, _) -> regToVReg dest
    | LIR.Cset (dest, _) -> regToVReg dest
    | LIR.And (dest, _, _) | LIR.Orr (dest, _, _) | LIR.Eor (dest, _, _)
    | LIR.Lsl (dest, _, _) | LIR.Lsr (dest, _, _) -> regToVReg dest
    | LIR.Mvn (dest, _) -> regToVReg dest
    | LIR.Call (dest, _, _) -> regToVReg dest
    | LIR.TailCall _ -> None  // Tail calls don't return to caller
    | LIR.IndirectCall (dest, _, _) -> regToVReg dest
    | LIR.IndirectTailCall _ -> None  // Indirect tail calls don't return to caller
    | LIR.ClosureAlloc (dest, _, _) -> regToVReg dest
    | LIR.ClosureCall (dest, _, _) -> regToVReg dest
    | LIR.ClosureTailCall _ -> None  // Closure tail calls don't return to caller
    | LIR.HeapAlloc (dest, _) -> regToVReg dest
    | LIR.HeapLoad (dest, _, _) -> regToVReg dest
    | LIR.StringConcat (dest, _, _) -> regToVReg dest
    | LIR.FileReadText (dest, _) -> regToVReg dest
    | LIR.FileExists (dest, _) -> regToVReg dest
    | LIR.FileWriteText (dest, _, _) -> regToVReg dest
    | LIR.FileAppendText (dest, _, _) -> regToVReg dest
    | LIR.FileDelete (dest, _) -> regToVReg dest
    | LIR.FileSetExecutable (dest, _) -> regToVReg dest
    | LIR.FileWriteFromPtr (dest, _, _, _) -> regToVReg dest
    | LIR.RawAlloc (dest, _) -> regToVReg dest
    | LIR.RawGet (dest, _, _) -> regToVReg dest
    | LIR.RawGetByte (dest, _, _) -> regToVReg dest
    | LIR.RawFree _ -> None
    | LIR.RawSet _ -> None
    | LIR.RawSetByte _ -> None
    // FloatToInt defines an integer destination register
    | LIR.FloatToInt (dest, _) -> regToVReg dest
    | LIR.StringHash (dest, _) -> regToVReg dest
    | LIR.StringEq (dest, _, _) -> regToVReg dest
    | LIR.RefCountIncString _ -> None
    | LIR.RefCountDecString _ -> None
    | LIR.RandomInt64 dest -> regToVReg dest
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

/// Compute liveness at each instruction index within a block
/// Returns a list of live VReg sets, one per instruction (same order as Instrs)
let computeInstructionLiveness (block: LIR.BasicBlock) (liveOut: Set<int>) : Set<int> list =
    // Walk backwards from the terminator, tracking liveness
    let mutable live = liveOut

    // Handle terminator uses first
    let termUses = getTerminatorUsedVRegs block.Terminator
    for u in termUses do
        live <- Set.add u live

    // Walk instructions in reverse, collecting liveness at each point
    // We want liveness AFTER each instruction (what's live when that instruction completes)
    let instrsReversed = List.rev block.Instrs
    let mutable livenessListReversed = []

    for instr in instrsReversed do
        // Record liveness at this point (after the instruction executes)
        livenessListReversed <- live :: livenessListReversed

        // Update liveness: remove definition, add uses
        match getDefinedVReg instr with
        | Some def -> live <- Set.remove def live
        | None -> ()

        for used in getUsedVRegs instr do
            live <- Set.add used live

    // Since we walked backwards and prepended each result, the list is already
    // in forward order (matching the original instruction order)
    livenessListReversed

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

/// Caller-saved registers (X1-X8) - preferred for allocation
/// Note: X9-X10 are excluded because StringHash/StringEq use them internally
let callerSavedRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5
    LIR.X6; LIR.X7; LIR.X8
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

/// Get the caller-saved physical registers that contain live values
let getLiveCallerSavedRegs (liveVRegs: Set<int>) (mapping: Map<int, Allocation>) : LIR.PhysReg list =
    liveVRegs
    |> Set.toList
    |> List.choose (fun vregId ->
        match Map.tryFind vregId mapping with
        | Some (PhysReg reg) when List.contains reg callerSavedRegs -> Some reg
        | _ -> None)
    |> List.distinct
    |> List.sort  // Keep consistent order for deterministic output

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

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
        let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
        let (subReg, subLoads) = loadSpilled mapping sub LIR.X14
        let msubInstr = LIR.Msub (destReg, mulLeftReg, mulRightReg, subReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs

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

    | LIR.Eor (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let eorInstr = LIR.Eor (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [eorInstr] @ storeInstrs

    | LIR.Lsl (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let (shiftReg, shiftLoads) = loadSpilled mapping shift LIR.X13
        let lslInstr = LIR.Lsl (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lslInstr] @ storeInstrs

    | LIR.Lsr (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let (shiftReg, shiftLoads) = loadSpilled mapping shift LIR.X13
        let lsrInstr = LIR.Lsr (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lsrInstr] @ storeInstrs

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

    | LIR.TailCall (funcName, args) ->
        // Tail calls have no destination - just apply allocation to args
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
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
        // Apply allocation to each operand in the arg moves
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                let (allocatedOp, loads) = applyToOperand mapping srcOp LIR.X12
                (destReg, allocatedOp, loads))
        // Collect all load instructions and the allocated moves
        let allLoads = allocatedMoves |> List.collect (fun (_, _, loads) -> loads)
        let finalMoves = allocatedMoves |> List.map (fun (destReg, op, _) -> (destReg, op))
        allLoads @ [LIR.ArgMoves finalMoves]

    | LIR.TailArgMoves moves ->
        // Apply allocation to each operand in the tail arg moves (same as ArgMoves)
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                let (allocatedOp, loads) = applyToOperand mapping srcOp LIR.X12
                (destReg, allocatedOp, loads))
        // Collect all load instructions and the allocated moves
        let allLoads = allocatedMoves |> List.collect (fun (_, _, loads) -> loads)
        let finalMoves = allocatedMoves |> List.map (fun (destReg, op, _) -> (destReg, op))
        allLoads @ [LIR.TailArgMoves finalMoves]

    | LIR.FArgMoves moves ->
        // Pass through unchanged for now - float argument moves use physical registers only
        [LIR.FArgMoves moves]

    | LIR.PrintInt reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintInt regFinal]

    | LIR.PrintBool reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintBool regFinal]

    | LIR.PrintIntNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIR.PrintIntNoNewline regFinal]

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
    | LIR.PrintString (idx, len) -> [LIR.PrintString (idx, len)]
    | LIR.PrintChars chars -> [LIR.PrintChars chars]

    // FP instructions pass through unchanged
    | LIR.FMov (dest, src) -> [LIR.FMov (dest, src)]
    | LIR.FLoad (dest, idx) -> [LIR.FLoad (dest, idx)]
    | LIR.FAdd (dest, left, right) -> [LIR.FAdd (dest, left, right)]
    | LIR.FSub (dest, left, right) -> [LIR.FSub (dest, left, right)]
    | LIR.FMul (dest, left, right) -> [LIR.FMul (dest, left, right)]
    | LIR.FDiv (dest, left, right) -> [LIR.FDiv (dest, left, right)]
    | LIR.FNeg (dest, src) -> [LIR.FNeg (dest, src)]
    | LIR.FAbs (dest, src) -> [LIR.FAbs (dest, src)]
    | LIR.FSqrt (dest, src) -> [LIR.FSqrt (dest, src)]
    | LIR.FCmp (left, right) -> [LIR.FCmp (left, right)]
    // IntToFloat: src is integer register, dest is FP register
    | LIR.IntToFloat (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.IntToFloat (dest, srcReg)]
    // GpToFp: move bits from GP register to FP register (src is integer, dest is FP)
    | LIR.GpToFp (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIR.GpToFp (dest, srcReg)]
    // FloatToInt: src is FP register, dest is integer register
    | LIR.FloatToInt (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIR.FloatToInt (destReg, src)
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
        let (contentOp, contentLoads) = applyToOperand mapping content LIR.X13
        let fileInstr = LIR.FileWriteText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIR.FileAppendText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) = applyToOperand mapping content LIR.X13
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

    | LIR.RawFree ptr ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        ptrLoads @ [LIR.RawFree ptrReg]

    | LIR.RawGet (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let getInstr = LIR.RawGet (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let getInstr = LIR.RawGetByte (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIR.RawSet (ptr, byteOffset, value) ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
        ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSet (ptrReg, offsetReg, valueReg)]

    | LIR.RawSetByte (ptr, byteOffset, value) ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
        ptrLoads @ offsetLoads @ valueLoads @ [LIR.RawSetByte (ptrReg, offsetReg, valueReg)]

    | LIR.StringHash (dest, str) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        let hashInstr = LIR.StringHash (destReg, strOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        strLoads @ [hashInstr] @ storeInstrs

    | LIR.StringEq (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftOp, leftLoads) = applyToOperand mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let eqInstr = LIR.StringEq (destReg, leftOp, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [eqInstr] @ storeInstrs

    | LIR.RefCountIncString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIR.RefCountIncString strOp]

    | LIR.RefCountDecString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIR.RefCountDecString strOp]

    | LIR.RandomInt64 dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let randomInstr = LIR.RandomInt64 destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIR.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [randomInstr] @ storeInstrs

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

/// Apply allocation to a basic block with liveness-aware SaveRegs/RestoreRegs population
let applyToBlockWithLiveness
    (mapping: Map<int, Allocation>)
    (liveOut: Set<int>)
    (block: LIR.BasicBlock)
    : LIR.BasicBlock =

    // Compute liveness at each instruction point
    let instrLiveness = computeInstructionLiveness block liveOut

    // Process each instruction with its corresponding liveness
    // Debug: check lengths match
    let instrCount = List.length block.Instrs
    let livenessCount = List.length instrLiveness
    if instrCount <> livenessCount then
        failwithf "Instruction count (%d) doesn't match liveness count (%d)" instrCount livenessCount

    // First pass: find SaveRegs/RestoreRegs pairs and compute the registers to save
    // For each SaveRegs, look ahead to find the matching RestoreRegs and use its liveness
    // This ensures SaveRegs and RestoreRegs have matching register lists
    let mutable savedRegsStack : LIR.PhysReg list list = []

    let allocatedInstrs =
        List.zip block.Instrs instrLiveness
        |> List.collect (fun (instr, liveAfter) ->
            match instr with
            | LIR.SaveRegs ([], []) ->
                // At SaveRegs, we need to save registers that are:
                // 1. Currently live (have values that might be clobbered by the call)
                // 2. Needed after the call
                // The liveAfter here includes both categories, so we use it
                let liveCallerSaved = getLiveCallerSavedRegs liveAfter mapping
                // Push onto stack for matching RestoreRegs
                savedRegsStack <- liveCallerSaved :: savedRegsStack
                applyToInstr mapping (LIR.SaveRegs (liveCallerSaved, []))
            | LIR.RestoreRegs ([], []) ->
                // Pop the matching SaveRegs registers
                let liveCallerSaved =
                    match savedRegsStack with
                    | head :: tail ->
                        savedRegsStack <- tail
                        head
                    | [] ->
                        // Fallback: compute from liveness (shouldn't happen with matched pairs)
                        getLiveCallerSavedRegs liveAfter mapping
                applyToInstr mapping (LIR.RestoreRegs (liveCallerSaved, []))
            | _ ->
                applyToInstr mapping instr)

    let (termLoads, allocatedTerm) = applyToTerminator mapping block.Terminator
    {
        LIR.Label = block.Label
        LIR.Instrs = allocatedInstrs @ termLoads
        LIR.Terminator = allocatedTerm
    }

/// Apply allocation to a basic block (legacy - no liveness info)
let applyToBlock (mapping: Map<int, Allocation>) (block: LIR.BasicBlock) : LIR.BasicBlock =
    let allocatedInstrs = block.Instrs |> List.collect (applyToInstr mapping)
    let (termLoads, allocatedTerm) = applyToTerminator mapping block.Terminator
    {
        LIR.Label = block.Label
        LIR.Instrs = allocatedInstrs @ termLoads
        LIR.Terminator = allocatedTerm
    }

/// Apply allocation to CFG with liveness info
let applyToCFGWithLiveness
    (mapping: Map<int, Allocation>)
    (cfg: LIR.CFG)
    (liveness: Map<LIR.Label, BlockLiveness>)
    : LIR.CFG =
    {
        LIR.Entry = cfg.Entry
        LIR.Blocks = cfg.Blocks |> Map.map (fun label block ->
            let blockLiveness = Map.find label liveness
            applyToBlockWithLiveness mapping blockLiveness.LiveOut block)
    }

/// Apply allocation to CFG (legacy - no liveness info)
let applyToCFG (mapping: Map<int, Allocation>) (cfg: LIR.CFG) : LIR.CFG =
    {
        LIR.Entry = cfg.Entry
        LIR.Blocks = cfg.Blocks |> Map.map (fun _ block -> applyToBlock mapping block)
    }

// ============================================================================
// Main Entry Point
// ============================================================================

/// Parameter registers per ARM64 calling convention (X0-X7 for ints, D0-D7 for floats)
let parameterRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
let floatParamRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

/// Allocate registers for a function
let allocateRegisters (func: LIR.Function) : LIR.Function =
    // Step 1: Compute liveness
    let liveness = computeLiveness func.CFG

    // Step 2: Build live intervals
    let intervals = buildLiveIntervals func.CFG liveness

    // Step 3: Run linear scan allocation
    let result = linearScan intervals

    // Step 4: Build parameter info with separate int/float counters (AAPCS64)
    // Each parameter type uses its own register counter
    let paramsWithTypes = List.zip func.Params func.ParamTypes
    let _, _, intParams, floatParams =
        paramsWithTypes
        |> List.fold (fun (intIdx, floatIdx, intAcc, floatAcc) (reg, typ) ->
            if typ = AST.TFloat64 then
                // Float parameter - uses D registers
                (intIdx, floatIdx + 1, intAcc, (reg, floatIdx) :: floatAcc)
            else
                // Int/other parameter - uses X registers
                (intIdx + 1, floatIdx, (reg, intIdx) :: intAcc, floatAcc)
        ) (0, 0, [], [])
    let intParams = List.rev intParams
    let floatParams = List.rev floatParams

    // Step 5: Build mapping that copies INT parameters from X0-X7
    // to wherever linearScan allocated them
    let intParamCopyInstrs =
        intParams
        |> List.map (fun (reg, paramIdx) ->
            match reg with
            | LIR.Virtual id ->
                let paramReg = List.item paramIdx parameterRegs
                match Map.tryFind id result.Mapping with
                | Some (PhysReg allocatedReg) when allocatedReg <> paramReg ->
                    Some (LIR.Mov (LIR.Physical allocatedReg, LIR.Reg (LIR.Physical paramReg)))
                | Some (StackSlot offset) ->
                    Some (LIR.Store (offset, LIR.Physical paramReg))
                | _ -> None
            | LIR.Physical _ -> None)
        |> List.choose id
        |> List.rev  // Reverse to avoid clobbering

    // Step 6: Build mapping that copies FLOAT parameters from D0-D7
    // Float parameters use FVirtual registers (same ID as Virtual)
    // and don't go through linear scan - they map directly in CodeGen
    let floatParamCopyInstrs =
        // Reverse the order to avoid clobbering: copy from higher-indexed params first
        // E.g., if D0→D1 and D1→D2, we must do D1→D2 first, then D0→D1
        floatParams
        |> List.rev
        |> List.map (fun (reg, paramIdx) ->
            match reg with
            | LIR.Virtual id ->
                // Float param comes in D0/D1/etc, needs to be in FVirtual id
                // FVirtual id maps to D(id % 8) in CodeGen, so we need FMov if different
                let srcDReg = List.item paramIdx floatParamRegs
                let destFVirtual = LIR.FVirtual id
                // Always emit FMov from calling convention reg to virtual
                Some (LIR.FMov (destFVirtual, LIR.FPhysical srcDReg))
            | LIR.Physical _ -> None)
        |> List.choose id

    // Step 7: Apply allocation to CFG with liveness info for SaveRegs/RestoreRegs population
    let allocatedCFG = applyToCFGWithLiveness result.Mapping func.CFG liveness

    // Step 8: Insert parameter copy instructions at the start of the entry block
    // Float param copies go first (they use separate register bank)
    let entryBlock = Map.find func.CFG.Entry allocatedCFG.Blocks
    let entryBlockWithCopies = {
        entryBlock with
            Instrs = floatParamCopyInstrs @ intParamCopyInstrs @ entryBlock.Instrs
    }
    let updatedBlocks = Map.add func.CFG.Entry entryBlockWithCopies allocatedCFG.Blocks
    let cfgWithParamCopies = { allocatedCFG with Blocks = updatedBlocks }

    // Step 9: Set parameters to calling convention registers
    // For floats, we still record as X register in Params since Params is Reg list
    // The actual float handling is done via ParamTypes
    let allocatedParams =
        func.Params
        |> List.mapi (fun i _ -> LIR.Physical (List.item i parameterRegs))

    {
        LIR.Name = func.Name
        LIR.Params = allocatedParams
        LIR.ParamTypes = func.ParamTypes
        LIR.CFG = cfgWithParamCopies
        LIR.StackSize = result.StackSize
        LIR.UsedCalleeSaved = result.UsedCalleeSaved
    }
