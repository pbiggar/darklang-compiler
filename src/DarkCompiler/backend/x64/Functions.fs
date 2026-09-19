// Functions.fs - Lower allocated LIR functions with target frame and return conventions.

module X64Functions

open X64Operands
open X64Printing
open X64Frames
open X64CodeGenTypes
open X64FieldReferenceCounts
open X64Instructions
open X64Blocks

/// Translate a LIR function to x86-64 instructions
let translateFunction
    (enableLeakCheck: bool)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (functionNames: Map<AST.FunctionId, string>)
    (func: LIR.Function)
    : Result<X86_64.Instr list, string> =
    let epilogueLabel = "_epilogue_" + func.Name
    let prologue = genPrologue func.StackSize func.UsedCalleeSaved

    // Float parameter setup: the register allocator inserts FMov instructions
    // at the start of the entry block (e.g., "D1 <- FMov(D0)"). These are
    // handled by the FMov case in translateInstr. No extra codegen needed.

    let translateBlocks blocks =
        let rec loop acc indexedBlocks =
            match indexedBlocks with
            | [] -> Ok (List.rev acc |> List.concat)
            | (block, nextBlock) :: rest ->
            let ctx : FuncCtx = {
                FunctionName = func.Name
                StackSize = func.StackSize
                UsedCalleeSaved = func.UsedCalleeSaved
                EnableLeakCheck = enableLeakCheck
                RecordRegistry = recordRegistry
                SumShapeRegistry = sumShapeRegistry
                FunctionNames = functionNames
            }
            match translateBlock ctx epilogueLabel nextBlock block with
            | Error e -> Error e
            | Ok instrs -> loop (instrs :: acc) rest

        blocks
        |> List.mapi (fun index block -> block, List.tryItem (index + 1) blocks)
        |> loop []

    // Layout blocks into deterministic fallthrough chains before translation.
    let allBlocksResult =
        LIR.layoutBlocks func.CFG
        |> Result.mapError (fun e -> $"x64 codegen: function {func.Name}: {e}")

    match allBlocksResult |> Result.bind translateBlocks with
    | Error e -> Error e
    | Ok blockInstrs ->
        // Heap initialization for _start only
        let heapInit =
            if func.Name = "_start" then genHeapInit ()
            else []

        let funcLabel = [X86_64.Label func.Name]
        // Generate leak check report for _start exit
        let leakReport =
            if func.Name = "_start" && enableLeakCheck then
                genLeakCheckReport ()
            else []

        let epilogue =
            [X86_64.Label epilogueLabel]
            @ genEpilogue func.StackSize func.UsedCalleeSaved
            @ (if func.Name = "_start" then
                   // _start: report leaks then exit(0)
                   leakReport @ loadImm64 X86_64.RDI 0L @ genExitSyscall
               else
                   [X86_64.RET])
        // The root frame terminates the normal frame-pointer chain. This lets
        // CLI helpers recover process arguments from a known stack-relative
        // address without reserving R13 for the lifetime of the program.
        let rootFrameInit =
            if func.Name = "_start" then
                [X86_64.XOR_reg (X86_64.RBP, X86_64.RBP)]
            else []
        Ok (funcLabel @ rootFrameInit @ prologue @ heapInit @ blockInstrs @ epilogue)
