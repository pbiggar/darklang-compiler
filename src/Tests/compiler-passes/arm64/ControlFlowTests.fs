// ControlFlowTests.fs - Verify target return transfers, print control flow, and RC instruction costs.

module ARM64ControlFlowTests

open ARM64CodeGenFixtures

let private uint64ZeroBranchTargetsDigit (instrs: ARM64.Instr list) : bool =
    instrs
    |> List.mapi (fun index instr -> index, instr)
    |> List.tryPick (fun (index, instr) ->
        match instr with
        | ARM64.CBZ_offset (ARM64.X2, offset) -> Some (index + offset)
        | _ -> None)
    |> Option.bind (fun targetIndex -> List.tryItem targetIndex instrs)
    |> Option.exists (function
        | ARM64.MOVZ (ARM64.X2, 48us, 0) -> true
        | _ -> false)

let testPrintUInt64RuntimeZeroBranches () : TestResult =
    let withNewline = ARM64PrintValues.generatePrintUInt64NoExit target
    let withoutNewline = ARM64PrintValues.generatePrintUInt64NoNewline target

    if not (uint64ZeroBranchTargetsDigit withNewline) then
        Error "ARM64 UInt64 newline printer zero branch does not target the zero digit handler"
    else if not (uint64ZeroBranchTargetsDigit withoutNewline) then
        Error "ARM64 UInt64 no-newline printer zero branch does not target the zero digit handler"
    else
        Ok ()

let testPrintUInt64RuntimePreservesNewline () : TestResult =
    let preservesNewline =
        ARM64PrintValues.generatePrintUInt64NoExit target
        |> List.windowed 3
        |> List.exists (function
            | [ ARM64.MOVZ (ARM64.X3, 10us, 0)
                ARM64.STRB (ARM64.X3, ARM64.X1, 0)
                ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us) ] -> true
            | _ -> false)

    if preservesNewline then
        Ok ()
    else
        Error "ARM64 UInt64 newline printer does not move the digit cursor before conversion"

let testBranchFalseEdgeFallsThrough () : TestResult =
    let entry = LIR.Label "arm64_layout_entry"
    let trueBlock = LIR.Label "arm64_layout_true"
    let falseBlock = LIR.Label "arm64_layout_false"
    let block label instrs terminator : LIR.BasicBlock = { Label = label; Instrs = instrs; Terminator = terminator }
    let func : LIR.Function = {
        Id = TestIds.functionIdForName "arm64_layout"
        Name = "arm64_layout"
        TypedParams = []
        CFG = {
            Entry = entry
            Blocks = Map.ofList [
                entry, block entry [] (LIR.Branch (LIR.Physical LIR.X0, trueBlock, falseBlock))
                trueBlock, block trueBlock [] LIR.Ret
                falseBlock, block falseBlock [LIR.HeapAlloc (LIR.Physical LIR.X1, 8)] LIR.Ret
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    let ctx : ARM64CodeGenTypes.CodeGenContext = {
        Target = target; Options = ARM64CodeGenTypes.defaultOptions; SumShapeRegistry = Map.empty; RecordRegistry = Map.empty
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = Map.empty; ClosureCaptureTypes = Map.empty; FunctionNames = Map.empty
        FunctionName = func.Name; InstructionSite = ""; StackSize = 0; UsedCalleeSaved = []
        HeapOverflowLabel = "__heap_oom_arm64_layout"
        RecordLirOpExpansion = None
    }
    match ARM64Functions.convertFunction [] ctx func with
    | Error e -> Error e
    | Ok instrs ->
        let falseJump = ARM64Symbolic.B_label "arm64_layout_false"
        let epilogueJumps = instrs |> List.filter ((=) (ARM64Symbolic.B_label "_epilogue_arm64_layout")) |> List.length
        let epilogueIndex = instrs |> List.tryFindIndex ((=) (ARM64Symbolic.Label "_epilogue_arm64_layout"))
        let overflowIndex = instrs |> List.tryFindIndex ((=) (ARM64Symbolic.Label "__heap_oom_arm64_layout"))
        if List.contains falseJump instrs then Error "ARM64 emitted a jump to the immediately following false block"
        elif epilogueJumps <> 1 then Error $"ARM64 emitted {epilogueJumps} jumps to the epilogue; expected one before the final fallthrough"
        else
            match epilogueIndex, overflowIndex with
            | Some epilogue, Some overflow when epilogue < overflow -> Ok ()
            | Some epilogue, Some overflow -> Error $"ARM64 heap-overflow trap at {overflow} blocks final return fallthrough to epilogue at {epilogue}"
            | _ -> Error "ARM64 allocation fixture did not emit both epilogue and heap-overflow labels"

/// A diamond needs one jump over the sibling branch, but neither a backward
/// jump from that sibling nor a return-to-epilogue jump. Count emitted transfers
/// rather than merely asserting a particular order of LIR labels.
let testSharedReturnTransferCost () : TestResult =
    let entry = LIR.Label "common_return_entry"
    let yes = LIR.Label "a_common_return_true"
    let no = LIR.Label "z_common_return_false"
    let join = LIR.Label "common_return_join"
    let block label instrs terminator : LIR.BasicBlock = { Label = label; Instrs = instrs; Terminator = terminator }
    let func : LIR.Function = {
        Id = TestIds.functionIdForName "common_return"
        Name = "common_return"
        TypedParams = []
        CFG = {
            Entry = entry
            Blocks = Map.ofList [
                entry, block entry [] (LIR.Branch (LIR.Physical LIR.X0, yes, no))
                yes, block yes [LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 11L)] (LIR.Jump join)
                no, block no [LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 22L)] (LIR.Jump join)
                join, block join [] LIR.Ret
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    let ctx : ARM64CodeGenTypes.CodeGenContext = {
        Target = target; Options = ARM64CodeGenTypes.defaultOptions; SumShapeRegistry = Map.empty; RecordRegistry = Map.empty
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = Map.empty; ClosureCaptureTypes = Map.empty; FunctionNames = Map.empty
        FunctionName = func.Name; InstructionSite = ""; StackSize = 0; UsedCalleeSaved = []
        HeapOverflowLabel = "__heap_oom_common_return"
        RecordLirOpExpansion = None
    }
    ARM64Functions.convertFunction [] ctx func
    |> Result.bind (fun instrs ->
        let transfers = instrs |> List.filter (function ARM64Symbolic.B_label _ -> true | _ -> false)
        if List.length transfers = 1 then Ok ()
        else Error $"Common-return diamond needs one unconditional transfer, got {transfers}")

/// Count the whole RC operation, including literal protection and scratch
/// preservation. Executable RC tests cover the heap/literal outcomes.
let testDynamicBufferRcInstructionCost () : TestResult =
    let ctx : ARM64CodeGenTypes.CodeGenContext = {
        Target = target; Options = ARM64CodeGenTypes.defaultOptions; SumShapeRegistry = Map.empty; RecordRegistry = Map.empty
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = Map.empty; ClosureCaptureTypes = Map.empty; FunctionNames = Map.empty
        FunctionName = "buffer_rc_cost"; InstructionSite = ""; StackSize = 0; UsedCalleeSaved = []
        HeapOverflowLabel = "__heap_oom_buffer_rc_cost"
        RecordLirOpExpansion = None
    }
    let operations = [LIR.RefCountIncString; LIR.RefCountDecString; LIR.RefCountIncBlob; LIR.RefCountDecBlob]
    let operands = [LIR.X0, 6; LIR.X13, 7; LIR.X14, 6; LIR.X15, 7]
    operations
    |> List.collect (fun operation -> operands |> List.map (fun (reg, cost) -> operation (LIR.Reg (LIR.Physical reg)), cost))
    |> List.fold (fun result (operation, cost) ->
        result |> Result.bind (fun () ->
            ARM64Instructions.convertInstr ctx operation
            |> Result.bind (fun instrs ->
                if List.length instrs = cost then Ok ()
                else Error $"{operation}: expected {cost} instructions, got {List.length instrs}"))) (Ok ())

let testPrimitiveListPayloadPreservationCost () : TestResult =
    let helper = "__dark_list_refcount_dec_helper"
    let program =
        makeSimpleProgramWithVariants
            [LIR.RefCountDec (LIR.Physical LIR.X0, 0, LIR.TaggedList, Some (rcMetadata (AST.TList AST.TInt64)))]
            Map.empty
    generatePreparedARM64 target program
    |> Result.bind (fun instrs ->
        match instrs |> List.skipWhile ((<>) (ARM64Symbolic.Label helper)) with
        | [] -> Error "Primitive list release code was not emitted"
        | _ :: rest ->
            let body =
                rest |> List.takeWhile (function
                    | ARM64Symbolic.Label name -> name.StartsWith(helper + "_")
                    | _ -> true)
            // The DFS work stack remains necessary. Only spills and copies
            // protecting node state across payload destruction are redundant.
            let preservation =
                body |> List.filter (function
                    | ARM64Symbolic.STP_pre (ARM64.X19, ARM64.X20, ARM64.SP, _)
                    | ARM64Symbolic.LDP_post (ARM64.X19, ARM64.X20, ARM64.SP, _)
                    | ARM64Symbolic.STR (ARM64.X21, ARM64.SP, _)
                    | ARM64Symbolic.LDR (ARM64.X21, ARM64.SP, _)
                    | ARM64Symbolic.MOV_reg ((ARM64.X19 | ARM64.X20 | ARM64.X21), _)
                    | ARM64Symbolic.MOV_reg (_, (ARM64.X19 | ARM64.X20 | ARM64.X21)) -> true
                    | _ -> false)
            if List.isEmpty preservation then Ok ()
            else Error $"Primitive list release needs no payload preservation instructions, got {List.length preservation}")

let internal makeEmptyFunction
    (name: string)
    (typedParams: LIR.TypedLIRParam list)
    : LIR.Function =
    let label = LIR.Label $"{name}_entry"
    {
        Id = TestIds.functionIdForName name
        Name = name
        TypedParams = typedParams
        CFG = {
            Entry = label
            Blocks = Map.ofList [
                label,
                {
                    Label = label
                    Instrs = []
                    Terminator = LIR.Ret
                }
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

let private makeAllocatedEntryFunction
    (name: string)
    (typedParams: LIR.TypedLIRParam list)
    (entryInstrs: LIR.Instr list)
    (stackSize: int)
    : LIR.Function =
    let label = LIR.Label $"{name}_entry"
    {
        Id = TestIds.functionIdForName name
        Name = name
        TypedParams = typedParams
        CFG = {
            Entry = label
            Blocks = Map.ofList [
                label,
                {
                    Label = label
                    Instrs = entryInstrs
                    Terminator = LIR.Ret
                }
            ]
        }
        StackSize = stackSize
        UsedCalleeSaved = []
        CodegenFacts = None
    }

let private generatedEntryTransfers
    (func: LIR.Function)
    : Result<ARM64Symbolic.Instr list, string> =
    let ctx : ARM64CodeGenTypes.CodeGenContext = {
        Target = target
        Options = ARM64CodeGenTypes.defaultOptions
        SumShapeRegistry = Map.empty
        RecordRegistry = Map.empty
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = Map.empty
        ClosureCaptureTypes = Map.empty
        FunctionNames = Map.empty
        FunctionName = func.Name
        InstructionSite = ""
        StackSize = func.StackSize
        UsedCalleeSaved = func.UsedCalleeSaved
        HeapOverflowLabel = $"__heap_oom_{func.Name}"
        RecordLirOpExpansion = None
    }

    ARM64Functions.convertFunction [] ctx func
    |> Result.map (List.filter (function
        | ARM64Symbolic.MOV_reg (ARM64.X29, ARM64.SP) -> false
        | ARM64Symbolic.MOV_reg _
        | ARM64Symbolic.FMOV_reg _
        | ARM64Symbolic.STUR _ -> true
        | _ -> false))

let private assertGeneratedEntryTransfers
    (caseName: string)
    (func: LIR.Function)
    (expected: ARM64Symbolic.Instr list)
    : TestResult =
    match generatedEntryTransfers func with
    | Error e -> Error $"{caseName} failed code generation: {e}"
    | Ok actual when actual = expected -> Ok ()
    | Ok actual ->
        let render instrs =
            instrs
            |> List.map TestDSL.PassTestRunner.prettyPrintARM64Instr
            |> String.concat "; "
        Error $"{caseName} expected [{render expected}], got [{render actual}]"

let testGeneratedEntryUsesAllocatorTransfersOnly () : TestResult =
    let intParam reg = { LIR.Reg = LIR.Physical reg; LIR.Type = AST.TInt64 }
    let floatParam = { LIR.Reg = LIR.Physical LIR.X0; LIR.Type = AST.TFloat64 }

    let identity =
        makeAllocatedEntryFunction "arm64_entry_identity" [intParam LIR.X0] [] 0

    let mixedWithSpill =
        makeAllocatedEntryFunction
            "arm64_entry_mixed_spill"
            [intParam LIR.X0; floatParam; intParam LIR.X1; floatParam]
            [
                LIR.FMov (LIR.FPhysical LIR.D4, LIR.FPhysical LIR.D0)
                LIR.FMov (LIR.FPhysical LIR.D5, LIR.FPhysical LIR.D1)
                LIR.Store (-8, LIR.Physical LIR.X1)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Reg (LIR.Physical LIR.X0))
            ]
            16

    let swap =
        makeAllocatedEntryFunction
            "arm64_entry_swap"
            [intParam LIR.X0; intParam LIR.X1]
            [
                LIR.Mov (LIR.Physical LIR.X16, LIR.Reg (LIR.Physical LIR.X0))
                LIR.Mov (LIR.Physical LIR.X0, LIR.Reg (LIR.Physical LIR.X1))
                LIR.Mov (LIR.Physical LIR.X1, LIR.Reg (LIR.Physical LIR.X16))
            ]
            0

    let eightArgs =
        makeAllocatedEntryFunction
            "arm64_entry_eight_args"
            [
                intParam LIR.X0; intParam LIR.X1; intParam LIR.X2; intParam LIR.X3
                intParam LIR.X4; intParam LIR.X5; intParam LIR.X6; intParam LIR.X7
            ]
            [
                LIR.Store (-8, LIR.Physical LIR.X7)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Reg (LIR.Physical LIR.X6))
            ]
            16

    assertGeneratedEntryTransfers "identity parameter" identity []
    |> Result.bind (fun () ->
        assertGeneratedEntryTransfers
            "mixed integer/float parameters with spill"
            mixedWithSpill
            [
                ARM64Symbolic.FMOV_reg (ARM64.D4, ARM64.D0)
                ARM64Symbolic.FMOV_reg (ARM64.D5, ARM64.D1)
                ARM64Symbolic.STUR (ARM64.X1, ARM64.X29, -8s)
                ARM64Symbolic.MOV_reg (ARM64.X3, ARM64.X0)
            ])
    |> Result.bind (fun () ->
        assertGeneratedEntryTransfers
            "parallel-move swap"
            swap
            [
                ARM64Symbolic.MOV_reg (ARM64.X16, ARM64.X0)
                ARM64Symbolic.MOV_reg (ARM64.X0, ARM64.X1)
                ARM64Symbolic.MOV_reg (ARM64.X1, ARM64.X16)
            ])
    |> Result.bind (fun () ->
        assertGeneratedEntryTransfers
            "eight integer arguments"
            eightArgs
            [
                ARM64Symbolic.STUR (ARM64.X7, ARM64.X29, -8s)
                ARM64Symbolic.MOV_reg (ARM64.X7, ARM64.X6)
            ])

/// Test: malformed ARM64 CFGs should be reported as codegen errors instead of silently dropping the entry.
let testReportsMissingEntryBlock () : TestResult =
    let entryLabel = LIR.Label "_start_entry"
    let bodyLabel = LIR.Label "_start_body"
    let bodyBlock : LIR.BasicBlock = {
        Label = bodyLabel
        Instrs = []
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Id = TestIds.functionIdForName "_start"
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [(bodyLabel, bodyBlock)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    let program = LIR.Program ([func], Map.empty, Map.empty)

    match generatePreparedARM64 target program with
    | Error e when e.Contains "missing entry block" -> Ok ()
    | Error e -> Error $"Expected missing entry block error, got '{e}'"
    | Ok _ -> Error "Expected ARM64 codegen to reject a CFG whose entry block is absent"
