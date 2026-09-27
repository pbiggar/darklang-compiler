// Functions.fs - Lower allocated LIR functions with target frame and return conventions.

module ARM64Functions

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Frames
open ARM64Blocks
open ARM64ProcessLifecycle

/// Convert LIR function to ARM64 instructions with prologue and epilogue
let convertFunction
    (heapOverflowTrapBody: ARM64Symbolic.Instr list)
    (ctx: CodeGenContext)
    (func: LIR.Function)
    : Result<ARM64Symbolic.Instr list, string> =
    // Generate epilogue label for this function (passed to convertCFG for Ret terminators)
    let epilogueLabel = "_epilogue_" + func.Name
    let overflowLabel = heapOverflowLabelPrefix + func.Name
    let needsHeapOverflowTrap =
        func.CFG.Blocks
        |> Map.exists (fun _ block ->
            block.Instrs
            |> List.exists (function
                | LIR.HeapAlloc _ -> true
                | LIR.RawAlloc _ -> true
                | LIR.MappedAlloc _ | LIR.MappedFree _ -> true
                | _ -> false))

    // Create function-specific context with stack info for tail call epilogue generation
    let usedCalleeSavedF =
        func.CodegenFacts
        |> Option.map (fun facts -> facts.Arm64UsedCalleeSavedF)
        |> Option.defaultValue []
    let funcCtx = {
        ctx with
            FunctionName = func.Name
            RawSlotInitRetainTargets =
                func.CodegenFacts
                |> Option.bind (fun facts -> facts.Arm64RawSlotInitRetainTargets)
            StackSize = func.StackSize
            UsedCalleeSaved = func.UsedCalleeSaved
            UsedCalleeSavedF = usedCalleeSavedF
            HeapOverflowLabel = overflowLabel
    }

    // Convert CFG to ARM64 instructions
    match convertCFG funcCtx epilogueLabel func.CFG with
    | Error err -> Error err
    | Ok cfgInstrs ->
        // Generate prologue (save FP/LR, allocate stack)
        let prologue = generatePrologue func.UsedCalleeSaved usedCalleeSavedF func.StackSize

        // Generate heap initialization for _start only
        let heapInit =
            if func.Name = "_start" then generateHeapInit ctx.Target
            else []

        // Note: Coverage buffer is in BSS section (zero-initialized by OS)
        // No runtime initialization needed - CoverageHit uses ADRP+ADD to access it

        // Shared cold path for allocation overflow in this function.
        let heapOverflowTrap =
            if needsHeapOverflowTrap then
                generateHeapOverflowTrapBlock heapOverflowTrapBody overflowLabel
            else
                []

        // Generate epilogue (deallocate stack, restore FP/LR, return or exit)
        let epilogueLabelInstr = [ARM64Symbolic.Label ("_epilogue_" + func.Name)]
        let epilogue =
            if func.Name = "_start" then
                // For _start, flush coverage (if enabled) then exit instead of return
                let coverageFlush =
                    if ctx.Options.EnableCoverage then
                        runtimeInstrs (ARM64Coverage.generateCoverageFlush ctx.Target ctx.Options.CoverageExprCount)
                    else []
                let leakCheckReport = generateLeakCheckReport ctx
                generateEpilogue func.UsedCalleeSaved usedCalleeSavedF func.StackSize
                |> List.filter (function ARM64Symbolic.RET -> false | _ -> true)  // Remove RET
                |> fun instrs -> instrs @ coverageFlush @ leakCheckReport @ runtimeInstrs (ARM64PrintAndExit.generateExit ctx.Target)
            else
                generateEpilogue func.UsedCalleeSaved usedCalleeSavedF func.StackSize

        // Add function entry label (for BL to branch to)
        let functionEntryLabel = [ARM64Symbolic.Label func.Name]

        // Terminate _start's frame-pointer chain so CLI helpers can recover
        // argv/envp from the original stack without reserving X18.
        let rootFrameInit =
            if func.Name = "_start" then
                [ARM64Symbolic.MOVZ (ARM64Symbolic.X29, 0us, 0)
                 ARM64Symbolic.MOVZ (ARM64Symbolic.X25, 0us, 0)]
            else []

        // Parameter stack stores and parallel register moves are already the first
        // instructions in the allocated CFG entry block.
        // Keep the epilogue immediately after the CFG so its final Ret terminator
        // can fall through. The terminating epilogue makes the overflow trap a
        // cold out-of-line block reached only by explicit allocation branches.
        Ok (functionEntryLabel @ rootFrameInit @ prologue @ heapInit @ cfgInstrs @ epilogueLabelInstr @ epilogue @ heapOverflowTrap)
