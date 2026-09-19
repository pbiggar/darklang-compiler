// RegisterAllocation.fs - Orchestrate integer and floating allocation and phi elimination.

module RegisterAllocation

open AllocationModel
open RegisterFacts
open RegisterLiveness
open RegisterPolicy
open RegisterInterference
open RegisterCoalescing
open RegisterColoring
open FloatAllocation
open SpillOperands
open ApplyBlockAllocation
open PhiResolution

// ============================================================================
// Main Entry Point
// ============================================================================

/// Parameter registers per ARM64 calling convention (X0-X7 for ints, D0-D7 for floats)
let parameterRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
let floatParamRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

let private appendTiming
    (phase: string)
    (elapsedMs: float)
    (timings: RegisterAllocationTiming list)
    : RegisterAllocationTiming list =
    timings @ [{ Phase = phase; ElapsedMs = elapsedMs }]

/// Allocate registers for a function
let private timePhase
    (swOpt: System.Diagnostics.Stopwatch option)
    (phase: string)
    (timings: RegisterAllocationTiming list)
    (f: unit -> 'a)
    : 'a * RegisterAllocationTiming list =
    match swOpt with
    | None -> (f (), timings)
    | Some sw ->
        let start = sw.Elapsed.TotalMilliseconds
        let result = f ()
        let elapsedMs = sw.Elapsed.TotalMilliseconds - start
        (result, appendTiming phase elapsedMs timings)

let private allocateRegistersInternal
    (arch: Platform.Arch)
    (swOpt: System.Diagnostics.Stopwatch option)
    (func: LIR.Function)
    : LIR.Function * RegisterAllocationTiming list =
    let scheduledCFG = scheduleFloatLoadsInCFG func.CFG
    // Precompute parameter info with separate int/float counters (AAPCS64)
    // Needed for entry defs and float allocation.
    let paramsWithTypes = func.TypedParams |> List.map (fun tp -> (tp.Reg, tp.Type))
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

    let intParamVRegIds =
        intParams
        |> List.choose (fun (reg, _) ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)

    // Extract FVirtual IDs from float params for allocation
    // Float params use Virtual register IDs that are also FVirtual IDs
    let floatParamFVirtualIds =
        floatParams
        |> List.choose (fun (reg, _) ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)
    let floatParamPrecolors =
        floatParams
        |> List.choose (fun (reg, paramIdx) ->
            match reg with
            | LIR.Virtual id -> Some (id, paramIdx)
            | LIR.Physical _ -> None)

    let (blockIndex, blocks) = buildBlockIndex scheduledCFG

    // Step 1: Classify instructions once, then solve both liveness domains together.
    let ((classifiedBlocks, domain, livenessBits, floatDomain, floatLiveness), timings) =
        timePhase swOpt "RegAlloc: Liveness" [] (fun () ->
            let classifiedBlocks = classifyBlocks blocks
            let (domain, livenessBits, floatDomain, floatLiveness) =
                computeCombinedLivenessBitsFromFacts
                    blockIndex
                    classifiedBlocks
                    intParamVRegIds
                    floatParamFVirtualIds
            (classifiedBlocks, domain, livenessBits, floatDomain, floatLiveness))
    let intParamBits = vregBitsFromList domain intParamVRegIds

    // Step 2: Build interference graph
    let (graph, timings) =
        timePhase swOpt "RegAlloc: Interference Graph" timings (fun () ->
            buildInterferenceGraphBitsetWithLiveness
                blockIndex
                classifiedBlocks
                domain
                livenessBits
                intParamBits)

    // Step 2b: Collect coalescing preferences and move pairs
    let ((preferences, movePairs), timings) =
        timePhase swOpt "RegAlloc: Coalescing Prep" timings (fun () ->
            let phiPairs = collectPhiPairs blocks
            let preferences = phiPairs
            let moves = collectMovePairs blocks
            let movePairs = dedupePairs (moves @ phiPairs)
            (preferences, movePairs))

    // Step 3: Run chordal graph coloring with phi coalescing
    // Use optimal register order based on calling pattern:
    // - Functions with non-tail calls: callee-saved first (save once in prologue/epilogue)
    // - Leaf functions / tail-call-only: caller-saved first (no prologue overhead)
    let (result, timings) =
        match swOpt with
        | None ->
            timePhase swOpt "RegAlloc: Coloring" timings (fun () ->
                let regs = getAllocatableRegs arch blocks
                let colorResult = chordalGraphColor graph [] (List.length regs) preferences movePairs
                coloringToAllocation colorResult regs)
        | Some sw ->
            let start = sw.Elapsed.TotalMilliseconds
            let regs = getAllocatableRegs arch blocks
            let (colorResult, colorTiming) =
                chordalGraphColorWithTiming sw graph [] (List.length regs) preferences movePairs
            let result = coloringToAllocation colorResult regs
            let totalMs = sw.Elapsed.TotalMilliseconds - start
            let timings =
                timings
                |> appendTiming "RegAlloc: Coloring" totalMs
                |> appendTiming "RegAlloc: Coloring - Coalesce" colorTiming.CoalesceMs
                |> appendTiming "RegAlloc: Coloring - MCS" colorTiming.McsMs
                |> appendTiming "RegAlloc: Coloring - Greedy" colorTiming.GreedyMs
                |> appendTiming "RegAlloc: Coloring - Expand" colorTiming.ExpandMs
            (result, timings)

    // Step 3b: Parameter info already computed (needed for float allocation and param moves)

    // Step 3c: Run float register allocation. Its liveness was solved with the
    // integer domain above, including float parameters absent from the CFG.
    let floatParamBits = vregBitsFromList floatDomain floatParamFVirtualIds
    let (floatAllocation, timings) =
        timePhase swOpt "RegAlloc: Float Allocation" timings (fun () ->
            chordalFloatAllocationWithLiveness
                (allocatableFloatRegsFor arch)
                result.StackSize
                blockIndex
                blocks
                classifiedBlocks
                floatParamBits
                floatParamPrecolors
                floatDomain
                floatLiveness)

    let ((intParamCopyInstrs, floatParamCopyInstrs, entryEdgePhiInstrs), timings) =
        timePhase swOpt "RegAlloc: Param Moves" timings (fun () ->
            // Step 5: Build mapping that copies INT parameters from X0-X7
            // to wherever chordal graph coloring allocated them.
            // IMPORTANT: Use proper parallel move resolution to handle cycles!
            // (e.g., X1→X2 and X2→X1 require a temp register)
            let intParamMoves =
                intParams
                |> List.choose (fun (reg, paramIdx) ->
                    match reg with
                    | LIR.Virtual id ->
                        let paramReg = List.item paramIdx parameterRegs
                        match tryAllocation result id with
                        | Some (PhysReg allocatedReg) when allocatedReg <> paramReg ->
                            // Need to copy from paramReg to allocatedReg
                            Some (allocatedReg, LIR.Reg (LIR.Physical paramReg))
                        | Some (StackSlot _offset) ->
                            // Store to stack - not a register move, handle separately
                            None // We'll handle stack stores separately
                        | _ -> None // Same register or not in mapping
                    | LIR.Physical _ -> None)

            // Collect stack stores separately (they don't conflict with register moves)
            let intParamStackStores =
                intParams
                |> List.choose (fun (reg, paramIdx) ->
                    match reg with
                    | LIR.Virtual id ->
                        let paramReg = List.item paramIdx parameterRegs
                        match tryAllocation result id with
                        | Some (StackSlot offset) ->
                            Some (LIR.Store (offset, LIR.Physical paramReg))
                        | _ -> None
                    | LIR.Physical _ -> None)

            // Use parallel move resolution for register-to-register moves
            let getSrcReg (op: LIR.Operand) : LIR.PhysReg option =
                match op with
                | LIR.Reg (LIR.Physical r) -> Some r
                | _ -> None

            let moveActions = ParallelMoves.resolve intParamMoves getSrcReg

            // Convert move actions to LIR instructions using X16 as temp register
            let regMoveInstrs =
                moveActions
                |> List.collect (fun action ->
                    match action with
                    | ParallelMoves.SaveToTemp reg ->
                        [LIR.Mov (LIR.Physical LIR.X16, LIR.Reg (LIR.Physical reg))]
                    | ParallelMoves.Move (dest, src) ->
                        [LIR.Mov (LIR.Physical dest, src)]
                    | ParallelMoves.MoveFromTemp dest ->
                        [LIR.Mov (LIR.Physical dest, LIR.Reg (LIR.Physical LIR.X16))])

            // IMPORTANT: stack stores must happen BEFORE register shuffles.
            // Otherwise a shuffle may clobber a source parameter register
            // (for example X5) before we spill that original parameter value.
            let intParamCopyInstrs = intParamStackStores @ regMoveInstrs

            // Step 6: Build mapping that copies FLOAT parameters from D0-D7
            // Float parameters use FVirtual registers (same ID as Virtual)
            // and don't go through linear scan - they map directly in CodeGen
            // IMPORTANT: Use parallel move resolution to handle cases where destination
            // registers collide with source registers (e.g., when FVirtual id maps to D0
            // which is also a source register for other params)
            let floatParamMoves =
                floatParams
                |> List.choose (fun (reg, paramIdx) ->
                    match reg with
                    | LIR.Virtual id ->
                        // Float param comes in D0/D1/etc, needs to be in FVirtual id
                        let srcDReg = List.item paramIdx floatParamRegs
                        let destFVirtual = LIR.FVirtual id
                        Some (destFVirtual, LIR.FPhysical srcDReg)
                    | LIR.Physical _ -> None)

            let floatParamCopyInstrs = generateFloatMoveInstrsWithAllocation floatParamMoves floatAllocation

            // Step 6b: Extract entry-edge phi moves for float phis
            // For phis at the entry block, we need to add moves from entry-edge sources
            // to phi destinations. These moves don't get added by resolvePhiNodes because
            // there's no predecessor block for "before function entry".
            let entryBlockBeforeResolution = blocks.[blockIndex.EntryIndex]

            let entryEdgeFloatPhiMoves =
                entryBlockBeforeResolution.Instrs
                |> List.choose (fun instr ->
                    match instr with
                    | LIR.FPhi (dest, sources) ->
                        // Find sources where the predecessor label doesn't exist in the CFG
                        // (these are entry-edge sources)
                        let entryEdgeSources =
                            sources
                            |> List.filter (fun (_, predLabel) ->
                                match tryBlockIndex blockIndex predLabel with
                                | Some _ -> false
                                | None -> true)
                        // Generate moves for entry-edge sources
                        entryEdgeSources
                        |> List.map (fun (src, _) -> (dest, src))
                        |> Some
                    | _ -> None)
                |> List.concat

            // Generate FMov instructions for entry-edge phi resolution
            // Use parallel move resolution to handle potential register conflicts
            let entryEdgePhiInstrs = generateFloatMoveInstrsWithAllocation entryEdgeFloatPhiMoves floatAllocation

            (intParamCopyInstrs, floatParamCopyInstrs, entryEdgePhiInstrs))

    // Step 7: Resolve phi nodes (convert to moves at predecessor exits)
    // This must happen BEFORE applying allocation since we need to know where each
    // value is allocated to generate the correct moves
    let (blocksWithPhiResolved, timings) =
        timePhase swOpt "RegAlloc: Phi Resolution" timings (fun () ->
            if classifiedBlocks |> Array.exists (fun block -> block.HasPhiNodes) then
                resolvePhiNodes blockIndex blocks result floatAllocation
            else
                blocks)

    // Step 8: Apply allocation to CFG with liveness info for SaveRegs/RestoreRegs population
    let applyStart = swOpt |> Option.map (fun sw -> sw.Elapsed.TotalMilliseconds)
    let (blockPreparations, timings) =
        timePhase swOpt "RegAlloc: Apply Preparation" timings (fun () ->
            prepareCFGAllocation
                arch
                blocksWithPhiResolved
                result
                floatAllocation
                livenessBits
                floatLiveness
                classifiedBlocks)
    let (allocatedBlocks, timings) =
        timePhase swOpt "RegAlloc: Apply Rewrite" timings (fun () ->
            applyPreparedCFGAllocation
                arch
                blocksWithPhiResolved
                result
                floatAllocation
                blockPreparations)
    let timings =
        match swOpt, applyStart with
        | Some sw, Some start ->
            appendTiming "RegAlloc: Apply Allocation" (sw.Elapsed.TotalMilliseconds - start) timings
        | _ -> timings

    let ((cfgWithParamCopies, allocatedTypedParams), timings) =
        timePhase swOpt "RegAlloc: Finalize" timings (fun () ->
            // Step 9: Insert parameter copy instructions at the start of the entry block
            // Float param copies go first (they use separate register bank)
            // Entry-edge phi moves come after param copies (they copy from param FVirtual to phi dest FVirtual)
            // IMPORTANT: Apply float allocation to param copy instructions since they were generated
            // before applyFloatAllocationToCFG ran and still contain FVirtual registers
            let allocatedFloatParamCopyInstrs =
                floatParamCopyInstrs |> List.collect (applyFloatAllocationToInstrs floatAllocation)
            let allocatedEntryEdgePhiInstrs =
                entryEdgePhiInstrs |> List.collect (applyFloatAllocationToInstrs floatAllocation)

            let updatedBlocks = Array.copy allocatedBlocks
            let entryBlock = updatedBlocks.[blockIndex.EntryIndex]
            let entryBlockWithCopies = {
                entryBlock with
                    Instrs = allocatedFloatParamCopyInstrs @ allocatedEntryEdgePhiInstrs @ intParamCopyInstrs @ entryBlock.Instrs
            }

            updatedBlocks.[blockIndex.EntryIndex] <- entryBlockWithCopies
            let cfgWithParamCopies : LIR.CFG =
                { Entry = scheduledCFG.Entry; Blocks = blocksToMap blockIndex updatedBlocks }

            // Step 10: Set integer parameters to their calling convention registers.
            // AAPCS64 uses separate counters for X and D argument registers; floats are
            // skipped here because float setup is emitted by the allocator's FMovs.
            let (_, allocatedTypedParamsRev) =
                func.TypedParams
                |> List.fold (fun (intIdx, acc) tp ->
                    if tp.Type = AST.TFloat64 then
                        (intIdx, { tp with Reg = LIR.Physical LIR.X0 } :: acc)
                    else
                        let paramReg = List.item intIdx parameterRegs
                        (intIdx + 1, { tp with Reg = LIR.Physical paramReg } :: acc)
                ) (0, [])
            let allocatedTypedParams = List.rev allocatedTypedParamsRev

            (cfgWithParamCopies, allocatedTypedParams))

    let allocatedFunc : LIR.Function = {
        Id = func.Id
        Name = func.Name
        TypedParams = allocatedTypedParams
        CFG = cfgWithParamCopies
        StackSize = floatAllocation.StackSize
        UsedCalleeSaved = result.UsedCalleeSaved
        CodegenFacts = func.CodegenFacts
    }

    (allocatedFunc
     |> LIR_Peephole.removePostAllocationMovesFromFunction
     |> LIR_Peephole.optimizeAllocatedCounterUpdates, timings)

/// Allocate registers for a function
let allocateRegisters (arch: Platform.Arch) (func: LIR.Function) : LIR.Function =
    allocateRegistersInternal arch None func |> fst

/// Allocate registers for a function and collect phase timings
let allocateRegistersWithTiming
    (arch: Platform.Arch)
    (func: LIR.Function)
    : LIR.Function * RegisterAllocationTiming list =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    allocateRegistersInternal arch (Some sw) func
