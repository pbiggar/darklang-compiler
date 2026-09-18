// PhiResolution.fs - Lower phi edges to allocation-aware parallel moves.

module PhiResolution

open AllocationModel
open RegisterFacts
open FloatAllocation
open SpillOperands

// ============================================================================
// Float Move Generation (used by both phi resolution and param copies)
// ============================================================================

/// Generate float move instructions using allocation-based register mapping.
/// Uses the float allocation result instead of modulo-based mapping.
let generateFloatMoveInstrsWithAllocation
    (moves: (LIR.FReg * LIR.FReg) list)
    (floatAllocation: FAllocationResult) : LIR.Instr list =
    let location = function
        | LIR.FPhysical reg -> FPhysReg reg
        | LIR.FVirtual id ->
            match tryFloatAllocation floatAllocation id with
            | Some allocated -> allocated
            | None -> Crash.crash $"Float phi move vreg {id} has no allocation"
    let locatedMoves = moves |> List.map (fun (dest, src) -> (location dest, location src))
    let sourceLocation source =
        match source with
        | FRematerialized _ -> None
        | FPhysReg _ | FStackSlot _ -> Some source
    let loadInto target = function
        | FPhysReg src -> [LIR.FMov (target, LIR.FPhysical src)]
        | FStackSlot slot -> [LIR.FSpillLoad (target, slot)]
        | FRematerialized value -> [LIR.FLoad (target, value)]
    let moveTo dest src =
        match dest, src with
        | FPhysReg reg, source -> loadInto (LIR.FPhysical reg) source
        | FStackSlot slot, FPhysReg reg -> [LIR.FSpillStore (slot, LIR.FPhysical reg)]
        | FStackSlot slot, FStackSlot sourceSlot ->
            [ LIR.FSpillLoad (floatAllocation.SpillScratchLeft, sourceSlot)
              LIR.FSpillStore (slot, floatAllocation.SpillScratchLeft) ]
        | FStackSlot slot, FRematerialized value ->
            [ LIR.FLoad (floatAllocation.SpillScratchLeft, value)
              LIR.FSpillStore (slot, floatAllocation.SpillScratchLeft) ]
        | FRematerialized _, _ -> Crash.crash "A rematerialized Float cannot be a phi destination"
    ParallelMoves.resolve locatedMoves sourceLocation
    |> List.collect (function
        | ParallelMoves.SaveToTemp source -> loadInto floatAllocation.SpillScratchRight source
        | ParallelMoves.Move (dest, src) -> moveTo dest src
        | ParallelMoves.MoveFromTemp dest ->
            match dest, floatAllocation.SpillScratchRight with
            | FPhysReg reg, scratch -> [LIR.FMov (LIR.FPhysical reg, scratch)]
            | FStackSlot slot, scratch -> [LIR.FSpillStore (slot, scratch)]
            | FRematerialized _, _ -> Crash.crash "A rematerialized Float cannot be a phi destination")

// ============================================================================
// Phi Resolution
// ============================================================================

/// Resolve phi nodes by inserting parallel moves at predecessor block exits.
/// This function:
/// 1. Finds all phi nodes in each block
/// 2. Drops phis whose destination is never used
/// 3. For each predecessor, collects all (dest, src) pairs for moves
/// 4. Uses ParallelMoves.resolve to sequence the moves properly (handling cycles)
/// 5. Inserts the moves at the end of each predecessor (before terminator)
/// 6. Removes phi nodes from blocks
let resolvePhiNodes
    (blockIndex: BlockIndex)
    (blocks: LIR.BasicBlock array)
    (allocation: AllocationResult)
    (floatAllocation: FAllocationResult)
    : LIR.BasicBlock array =
    let neededDomain = allocation.Domain
    let n = neededDomain.Ids.Length
    let wordCount = neededDomain.WordCount

    let phiSources = Array.init n (fun _ -> Bitset.empty wordCount)
    blocks
    |> Array.iter (fun block ->
        block.Instrs
        |> List.iter (fun instr ->
            match instr with
            | LIR.Phi (LIR.Virtual destId, sources, _) ->
                match tryIndexOf neededDomain destId with
                | Some destIdx ->
                    sources
                    |> List.iter (fun (src, _) ->
                        match src with
                        | LIR.Reg (LIR.Virtual srcId) ->
                            match tryIndexOf neededDomain srcId with
                            | Some srcIdx -> Bitset.addIndexInPlace srcIdx phiSources.[destIdx]
                            | None -> ()
                        | _ -> ())
                | None -> ()
            | _ -> ()))

    let collectNonPhiUses (blocks: LIR.BasicBlock array) : BitSet =
        let uses = Bitset.empty wordCount
        blocks
        |> Array.iter (fun block ->
            block.Instrs
            |> List.iter (fun instr ->
                match instr with
                | LIR.Phi _ -> ()
                | LIR.FPhi _ -> ()
                | _ ->
                    getUsedVRegs instr
                    |> List.iter (fun id -> vregBitsAddInPlace neededDomain id uses))
            getTerminatorUsedVRegs block.Terminator
            |> List.iter (fun id -> vregBitsAddInPlace neededDomain id uses))
        uses

    let collectPhysicalPhiSources (blocks: LIR.BasicBlock array) : BitSet =
        let uses = Bitset.empty wordCount
        blocks
        |> Array.iter (fun block ->
            block.Instrs
            |> List.iter (fun instr ->
                match instr with
                | LIR.Phi (LIR.Physical _, sources, _) ->
                    sources
                    |> List.iter (fun (src, _) ->
                        match src with
                        | LIR.Reg (LIR.Virtual srcId) ->
                            vregBitsAddInPlace neededDomain srcId uses
                        | _ -> ())
                | _ -> ()))
        uses

    let computeNeededVRegs (blocks: LIR.BasicBlock array) : BitSet =
        let rootUses = collectNonPhiUses blocks
        Bitset.unionInPlace rootUses (collectPhysicalPhiSources blocks)
        let rec expand (needed: BitSet) (worklist: int list) : BitSet =
            match worklist with
            | [] -> needed
            | vIdx :: rest ->
                let sources = phiSources.[vIdx]
                let newSources = Bitset.diff sources needed
                if Bitset.isEmpty newSources then
                    expand needed rest
                else
                    Bitset.unionInPlace needed newSources
                    let worklist' = (Bitset.indicesToList newSources) @ rest
                    expand needed worklist'
        expand rootUses (Bitset.indicesToList rootUses)

    let neededVRegs = computeNeededVRegs blocks

    let phiDestNeeded (dest: LIR.Reg) : bool =
        match dest with
        | LIR.Virtual id -> vregBitsContains neededDomain neededVRegs id
        | LIR.Physical _ -> true

    // Get the allocation for a virtual register (register or stack slot)
    let getDestAllocation (reg: LIR.Reg) : Allocation =
        match reg with
        | LIR.Virtual id ->
            match tryAllocation allocation id with
            | Some alloc -> alloc
            | None -> Crash.crash $"RegisterAllocation: Virtual register {id} not found in allocation"
        | LIR.Physical p -> PhysReg p

    // Helper to convert a LIR.Operand to allocated version
    let operandToAllocated (op: LIR.Operand) : LIR.Operand =
        match op with
        | LIR.Reg (LIR.Virtual id) ->
            match tryAllocation allocation id with
            | Some (PhysReg r) -> LIR.Reg (LIR.Physical r)
            | Some (StackSlot offset) -> LIR.StackSlot offset
            | None -> op
        | LIR.Reg (LIR.Physical p) -> LIR.Reg (LIR.Physical p)
        | _ -> op

    // Collect all int phi info: for each phi, get (dest_reg, src_operand, pred_label)
    // This gives us: List of (dest, sources, valueType)
    let intPhiInfo =
        blocks
        |> Array.toList
        |> List.collect (fun block ->
            block.Instrs
            |> List.choose (fun instr ->
                match instr with
                | LIR.Phi (dest, sources, valueType) ->
                    if phiDestNeeded dest then
                        Some (dest, sources, valueType)
                    else
                        None
                | _ -> None))

    // Collect all float phi info: (dest FReg, source FRegs with labels)
    let floatPhiInfo =
        blocks
        |> Array.toList
        |> List.collect (fun block ->
            block.Instrs
            |> List.choose (fun instr ->
                match instr with
                | LIR.FPhi (dest, sources) -> Some (dest, sources)
                | _ -> None))

    // Group int phis by predecessor index: List<(dest_allocation, src_operand)> per block index
    // Keep the full Allocation type to handle both register and stack destinations
    let predecessorIntMoves = Array.init blocks.Length (fun _ -> [])
    for (dest, sources, _valueType) in intPhiInfo do
        let destAlloc = getDestAllocation dest
        for (src, predLabel) in sources do
            match tryBlockIndex blockIndex predLabel with
            | Some predIdx ->
                let srcAllocated = operandToAllocated src
                predecessorIntMoves.[predIdx] <- (destAlloc, srcAllocated) :: predecessorIntMoves.[predIdx]
            | None -> ()

    // Group float phis by predecessor index: List<(dest_freg, src_freg)> per block index
    // Float registers don't go through allocation - FVirtual maps directly to D regs in CodeGen
    let predecessorFloatMoves = Array.init blocks.Length (fun _ -> [])
    for (dest, sources) in floatPhiInfo do
        for (src, predLabel) in sources do
            match tryBlockIndex blockIndex predLabel with
            | Some predIdx ->
                predecessorFloatMoves.[predIdx] <- (dest, src) :: predecessorFloatMoves.[predIdx]
            | None -> ()

    // Generate move instructions for phi resolution using parallel move resolution
    // across both register and stack destinations (handles reg<->stack cycles).
    let generateIntMoveInstrs (moves: (Allocation * LIR.Operand) list) : LIR.Instr list =
        let getSrcAllocation (op: LIR.Operand) : Allocation option =
            match op with
            | LIR.Reg (LIR.Physical p) -> Some (PhysReg p)
            | LIR.StackSlot offset -> Some (StackSlot offset)
            | _ -> None

        let actions = ParallelMoves.resolve moves getSrcAllocation

        let saveToTemp (loc: Allocation) : LIR.Instr list =
            match loc with
            | PhysReg r -> [LIR.Mov (LIR.Physical LIR.X16, LIR.Reg (LIR.Physical r))]
            | StackSlot offset -> [LIR.Mov (LIR.Physical LIR.X16, LIR.StackSlot offset)]

        let moveFromTemp (loc: Allocation) : LIR.Instr list =
            match loc with
            | PhysReg r -> [LIR.Mov (LIR.Physical r, LIR.Reg (LIR.Physical LIR.X16))]
            | StackSlot offset -> [LIR.Store (offset, LIR.Physical LIR.X16)]

        let moveToDest (dest: Allocation) (src: LIR.Operand) : LIR.Instr list =
            match dest with
            | PhysReg r -> [LIR.Mov (LIR.Physical r, src)]
            | StackSlot offset ->
                match src with
                | LIR.Reg (LIR.Physical r) ->
                    [LIR.Store (offset, LIR.Physical r)]
                | _ ->
                    [LIR.Mov (LIR.Physical LIR.X16, src)
                     LIR.Store (offset, LIR.Physical LIR.X16)]

        actions
        |> List.collect (fun action ->
            match action with
            | ParallelMoves.SaveToTemp loc -> saveToTemp loc
            | ParallelMoves.Move (dest, src) -> moveToDest dest src
            | ParallelMoves.MoveFromTemp dest -> moveFromTemp dest)

    // Add moves to predecessor blocks
    let updatedBlocks = Array.copy blocks

    // Add int phi moves
    for predIdx in 0 .. updatedBlocks.Length - 1 do
        let moves = predecessorIntMoves.[predIdx]
        if not (List.isEmpty moves) then
            let predBlock = updatedBlocks.[predIdx]
            let moveInstrs = generateIntMoveInstrs moves
            updatedBlocks.[predIdx] <- { predBlock with Instrs = predBlock.Instrs @ moveInstrs }

    // Add float phi moves
    // IMPORTANT: For tail call blocks, the phi resolution is ALREADY handled by:
    // 1. FArgMoves: puts new values in D0-D7
    // 2. TailCall: jumps back to function entry
    // 3. Param copy at entry: copies D0-D7 to phi destination registers
    // So we should SKIP phi resolution for tail call backedges - it's redundant and incorrect.
    //
    // For non-tail-call predecessors, append moves at the end as usual.
    for predIdx in 0 .. updatedBlocks.Length - 1 do
        let moves = predecessorFloatMoves.[predIdx]
        if not (List.isEmpty moves) then
            let predBlock = updatedBlocks.[predIdx]
            // Add phi moves at end of predecessor block
            let moveInstrs = generateFloatMoveInstrsWithAllocation moves floatAllocation
            updatedBlocks.[predIdx] <- { predBlock with Instrs = predBlock.Instrs @ moveInstrs }

    // Remove phi and fphi nodes from all blocks
    updatedBlocks
    |> Array.map (fun block ->
        let filteredInstrs =
            block.Instrs
            |> List.filter (fun instr ->
                match instr with
                | LIR.Phi _ -> false
                | LIR.FPhi _ -> false
                | _ -> true)
        { block with Instrs = filteredInstrs })
