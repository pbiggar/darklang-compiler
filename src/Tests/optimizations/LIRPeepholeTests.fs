// LIRPeepholeTests.fs - Unit tests for local LIR cleanup helpers.
//
// These tests cover post-register-allocation cleanup that is not directly
// visible through the source-to-optimized-LIR test runner.

module LIRPeepholeTests

open LIR
open LIR_Peephole

type TestResult = Result<unit, string>

let testRemoveSelfMovesFromAllocatedFunction () : TestResult =
    let label = Label "entry"
    let block : BasicBlock = {
        Label = label
        Instrs = [
            Mov (Physical X1, Reg (Physical X1))
            Mov (Physical X2, Reg (Physical X3))
            Mov (Virtual 4, Reg (Virtual 4))
            FMov (FPhysical D3, FPhysical D3)
            Add (Physical X4, Physical X4, Imm 0L)
        ]
        Terminator = Ret
    }
    let func : Function = {
        Name = "self_move_cleanup"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

    match removeSelfMovesFromFunction func |> fun f -> Map.tryFind label f.CFG.Blocks with
    | None ->
        Error "Expected cleanup to preserve the entry block"
    | Some cleanedBlock ->
        let expected = [
            Mov (Physical X2, Reg (Physical X3))
            Add (Physical X4, Physical X4, Imm 0L)
        ]

        if cleanedBlock.Instrs = expected then
            Ok ()
        else
            Error $"Expected only self-moves to be removed, got: {cleanedBlock.Instrs}"

let testRemoveFloatingCopyBackMovesFromAllocatedFunction () : TestResult =
    let label = Label "entry"
    let block : BasicBlock = {
        Label = label
        Instrs = [
            FMov (FPhysical D3, FPhysical D5)
            FMov (FPhysical D2, FPhysical D4)
            FMov (FPhysical D5, FPhysical D3)
            FMov (FPhysical D4, FPhysical D2)
            FAdd (FPhysical D0, FPhysical D3, FPhysical D2)
        ]
        Terminator = Ret
    }
    let func : Function = {
        Name = "floating_copy_back_cleanup"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

    match removePostAllocationMovesFromFunction func |> fun f -> Map.tryFind label f.CFG.Blocks with
    | None ->
        Error "Expected cleanup to preserve the entry block"
    | Some cleanedBlock ->
        let expected = [
            FMov (FPhysical D3, FPhysical D5)
            FMov (FPhysical D2, FPhysical D4)
            FAdd (FPhysical D0, FPhysical D3, FPhysical D2)
        ]

        if cleanedBlock.Instrs = expected then
            Ok ()
        else
            Error $"Expected copy-back moves to be removed, got: {cleanedBlock.Instrs}"

let testFloatingCopyBackKeepsMoveAfterFPhiWritesSource () : TestResult =
    let label = Label "entry"
    let sourceLabel = Label "source"
    let block : BasicBlock = {
        Label = label
        Instrs = [
            FMov (FPhysical D3, FPhysical D5)
            FPhi (FPhysical D5, [(FPhysical D2, sourceLabel)])
            FMov (FPhysical D3, FPhysical D5)
        ]
        Terminator = Ret
    }
    let func : Function = {
        Name = "floating_copy_back_phi_write"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

    match removePostAllocationMovesFromFunction func |> fun f -> Map.tryFind label f.CFG.Blocks with
    | None ->
        Error "Expected cleanup to preserve the entry block"
    | Some cleanedBlock ->
        if cleanedBlock.Instrs = block.Instrs then
            Ok ()
        else
            Error $"Expected FPhi write to invalidate stale float aliases, got: {cleanedBlock.Instrs}"

let testFNegMoveChainFusesWhenTempDies () : TestResult =
    let instrs = [
        FNeg (FPhysical D0, FPhysical D2)
        FMov (FPhysical D2, FPhysical D0)
        PrintInt64 (Physical X0)
    ]

    let expected = [
        FNeg (FPhysical D2, FPhysical D2)
        PrintInt64 (Physical X0)
    ]

    let optimized = removeSelfMovesFromInstrs instrs
    if optimized = expected then
        Ok ()
    else
        Error $"Expected dead FNeg/FMov chain to fuse, got: {optimized}"

let testFloatingArithmeticMoveChainsFuseWhenTempsDie () : TestResult =
    let instrs = [
        FAdd (FVirtual 1, FVirtual 2, FVirtual 3)
        FMov (FVirtual 4, FVirtual 1)
        FSub (FVirtual 5, FVirtual 6, FVirtual 7)
        FMov (FVirtual 8, FVirtual 5)
        FMul (FVirtual 9, FVirtual 10, FVirtual 11)
        FMov (FVirtual 12, FVirtual 9)
        FDiv (FVirtual 13, FVirtual 14, FVirtual 15)
        FMov (FVirtual 16, FVirtual 13)
    ]

    let expected = [
        FAdd (FVirtual 4, FVirtual 2, FVirtual 3)
        FSub (FVirtual 8, FVirtual 6, FVirtual 7)
        FMul (FVirtual 12, FVirtual 10, FVirtual 11)
        FDiv (FVirtual 16, FVirtual 14, FVirtual 15)
    ]

    let optimized = optimizeInstrs instrs
    if optimized = expected then
        Ok ()
    else
        Error $"Expected dead floating arithmetic copies to fold, got: {optimized}"

let testFloatingArithmeticMoveChainKeepsLiveTemp () : TestResult =
    let instrs = [
        FAdd (FVirtual 1, FVirtual 2, FVirtual 3)
        FMov (FVirtual 4, FVirtual 1)
        PrintFloat (FVirtual 1)
    ]

    let optimized = optimizeInstrs instrs
    if optimized = instrs then
        Ok ()
    else
        Error $"Expected live floating arithmetic temporary to stay available, got: {optimized}"

let testSeparatedFloatAddKeepsLiveTemporary () : TestResult =
    let instrs = [
        FAdd (FVirtual 1, FVirtual 2, FVirtual 3)
        Mov (Virtual 10, Imm 1L)
        FMov (FVirtual 4, FVirtual 1)
        PrintFloat (FVirtual 1)
    ]

    let optimized = retargetSeparatedDeadFAdds instrs
    if optimized = instrs then
        Ok ()
    else
        Error $"Expected separated FAdd with a live temporary to stay unchanged, got: {optimized}"

let testSinkSeparatedAllocatedFloatAdd () : TestResult =
    let instrs = [
        FAdd (FPhysical D4, FPhysical D4, FPhysical D0)
        FAdd (FPhysical D2, FPhysical D2, FPhysical D2)
        FMul (FPhysical D2, FPhysical D2, FPhysical D3)
        FAdd (FPhysical D3, FPhysical D2, FPhysical D1)
        Add (Physical X1, Physical X1, Imm 1L)
        FMov (FPhysical D2, FPhysical D4)
    ]
    let expected = [
        FAdd (FPhysical D2, FPhysical D2, FPhysical D2)
        FMul (FPhysical D2, FPhysical D2, FPhysical D3)
        FAdd (FPhysical D3, FPhysical D2, FPhysical D1)
        Add (Physical X1, Physical X1, Imm 1L)
        FAdd (FPhysical D2, FPhysical D4, FPhysical D0)
    ]

    let optimized = sinkSeparatedAllocatedFAdds instrs
    if optimized = expected then
        Ok ()
    else
        Error $"Expected allocated FAdd to replace its separated copy, got: {optimized}"

let testSinkImmediateCounterUpdatePastAccumulator () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Add (Physical X2, Physical X2, Reg (Physical X1))
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Add (Physical X2, Physical X2, Reg (Physical X1))
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastSubtraction () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Sub (Physical X2, Physical X2, Reg (Physical X1))
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Sub (Physical X2, Physical X2, Reg (Physical X1))
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected subtraction counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastDivision () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Sdiv (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Sdiv (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected division counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastProduct () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Mul (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Mul (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected product counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastMultiplyAdd () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Madd (Physical X2, Physical X1, Physical X1, Physical X2)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Madd (Physical X2, Physical X1, Physical X1, Physical X2)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected multiply-add counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastMultiplySubtract () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Msub (Physical X2, Physical X1, Physical X1, Physical X2)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Msub (Physical X2, Physical X1, Physical X1, Physical X2)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected multiply-subtract counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastXor () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Eor (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Eor (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected XOR counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastAnd () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        And (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        And (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected AND counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastOr () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Orr (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Orr (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected OR counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastLeftShift () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Lsl (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Lsl (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected left-shift counter update to replace its copy-back move, got: {other}"

let testSinkImmediateCounterUpdatePastRightShift () : TestResult =
    let instrs = [
        Sub (Physical X3, Physical X1, Imm 1L)
        Lsr (Physical X2, Physical X2, Physical X1)
        Mov (Physical X1, Reg (Physical X3))
    ]
    let expected = [
        Lsr (Physical X2, Physical X2, Physical X1)
        Sub (Physical X1, Physical X1, Imm 1L)
    ]

    match sinkImmediateCounterUpdate instrs with
    | Some optimized when optimized = expected -> Ok ()
    | other -> Error $"Expected right-shift counter update to replace its copy-back move, got: {other}"

let testMulAddFusionKeepsLiveTempForPrint () : TestResult =
    let instrs = [
        Mul (Virtual 1, Virtual 2, Virtual 3)
        Add (Virtual 4, Virtual 1, Reg (Virtual 5))
        PrintInt64 (Virtual 1)
    ]

    let optimized = tryFuseMulAdd instrs
    if optimized = instrs then
        Ok ()
    else
        Error $"Expected MUL temp used by PrintInt64 to stay available, got: {optimized}"

let private optimizeBlockInstrs (instrs: Instr list) : Instr list =
    let label = Label "entry"
    let block : BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = Ret
    }

    optimizeBlock block |> fst |> fun optimized -> optimized.Instrs

let testMulSubFusionReplacesDeadTemp () : TestResult =
    let instrs = [
        Mul (Virtual 1, Virtual 2, Virtual 3)
        Sub (Virtual 4, Virtual 5, Reg (Virtual 1))
    ]

    let expected = [Msub (Virtual 4, Virtual 2, Virtual 3, Virtual 5)]
    let optimized = optimizeBlockInstrs instrs
    if optimized = expected then
        Ok ()
    else
        Error $"Expected dead MUL/SUB temporary to fuse into MSUB, got: {optimized}"

let testMulSubFusionKeepsLiveTempForPrint () : TestResult =
    let instrs = [
        Mul (Virtual 1, Virtual 2, Virtual 3)
        Sub (Virtual 4, Virtual 5, Reg (Virtual 1))
        PrintInt64 (Virtual 1)
    ]

    let optimized = optimizeBlockInstrs instrs
    if optimized = instrs then
        Ok ()
    else
        Error $"Expected MUL temp used by PrintInt64 to stay available, got: {optimized}"

let testFloatMultiplyAddCombineAndTargetDecision () : TestResult =
    let label = Label "entry"
    let block : BasicBlock = {
        Label = label
        Instrs = [
            FMul (FVirtual 1, FVirtual 2, FVirtual 3)
            FAdd (FVirtual 4, FVirtual 1, FVirtual 5)
        ]
        Terminator = Ret
    }
    let func : Function = {
        Name = "float_multiply_add"
        TypedParams = []
        CFG = { Entry = label; Blocks = Map.ofList [(label, block)] }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    let combined = tryFuseFloatMultiplyAdd block.Instrs |> fst
    let armBlock = optimizeFunctionFor Platform.ARM64 func |> fun optimized -> Map.tryFind label optimized.CFG.Blocks
    let x64Block = optimizeFunctionFor Platform.X86_64 func |> fun optimized -> Map.tryFind label optimized.CFG.Blocks
    match combined, armBlock, x64Block with
    | [FMadd (FVirtual 4, FVirtual 2, FVirtual 3, FVirtual 5)], Some arm, Some x64
        when arm.Instrs = block.Instrs && x64.Instrs = block.Instrs -> Ok ()
    | _ ->
        Error
            $"Expected an available FMADD combine rejected by strict target policies, got combine={combined}, ARM64={armBlock}, x64={x64Block}"

let testScalarDiamondFormsSelect () : TestResult =
    let entry = Label "entry"
    let trueLabel = Label "true"
    let falseLabel = Label "false"
    let join = Label "join"
    let blocks =
        [ (entry,
           { Label = entry
             Instrs = [Cmp (Virtual 0, Imm 0L)]
             Terminator = CondBranch (GT, trueLabel, falseLabel) })
          (trueLabel, { Label = trueLabel; Instrs = []; Terminator = Jump join })
          (falseLabel, { Label = falseLabel; Instrs = []; Terminator = Jump join })
          (join,
           { Label = join
             Instrs = [Phi (Virtual 3, [(Reg (Virtual 1), trueLabel); (Reg (Virtual 2), falseLabel)], Some AST.TInt64)]
             Terminator = Ret }) ]
        |> Map.ofList
    let optimized = optimizeCFG { Entry = entry; Blocks = blocks }
    match Map.tryFind entry optimized.Blocks, Map.tryFind join optimized.Blocks with
    | Some entryBlock, Some joinBlock
        when entryBlock.Instrs = [Cmp (Virtual 0, Imm 0L); Select (Virtual 3, Virtual 1, Virtual 2, GT)]
             && entryBlock.Terminator = Jump join
             && List.isEmpty joinBlock.Instrs
             && not (Map.containsKey trueLabel optimized.Blocks)
             && not (Map.containsKey falseLabel optimized.Blocks) -> Ok ()
    | _ -> Error $"Expected empty scalar diamond to become Select, got: {optimized.Blocks}"

let testMulConstantKeepsLiveConstRegister () : TestResult =
    let instrs = [
        Mov (Physical X1, Imm 3L)
        Mul (Physical X2, Physical X3, Physical X1)
        PrintInt64 (Physical X1)
    ]

    let optimized = tryMulByConstant instrs
    if optimized = instrs then
        Ok ()
    else
        Error $"Expected live constant register to block strength reduction, got: {optimized}"

let testBooleanNotBranchSwapsSuccessors () : TestResult =
    let condition = Virtual 1
    let negated = Virtual 2
    let trueLabel = Label "true"
    let falseLabel = Label "false"
    let block : BasicBlock = {
        Label = Label "entry"
        Instrs = [
            Mov (negated, Imm 1L)
            Sub (negated, negated, Reg condition)
        ]
        Terminator = Branch (negated, trueLabel, falseLabel)
    }

    let optimized = optimizeBlock block |> fst
    let expectedTerminator = Branch (condition, falseLabel, trueLabel)

    if optimized.Instrs = [] && optimized.Terminator = expectedTerminator then
        Ok ()
    else
        Error $"Expected Boolean negation branch to swap successors, got: {optimized}"

let testConditionalBranchKeepsBooleanUsedInSuccessor () : TestResult =
    let entry = Label "entry"
    let trueLabel = Label "true"
    let falseLabel = Label "false"
    let condition = Virtual 1
    let entryBlock : BasicBlock = {
        Label = entry
        Instrs = [
            Cmp (Virtual 2, Reg (Virtual 3))
            Cset (condition, LT)
        ]
        Terminator = Branch (condition, trueLabel, falseLabel)
    }
    let trueBlock : BasicBlock = {
        Label = trueLabel
        Instrs = [PrintBool condition]
        Terminator = Ret
    }
    let falseBlock : BasicBlock = {
        Label = falseLabel
        Instrs = []
        Terminator = Ret
    }
    let cfg : CFG = {
        Entry = entry
        Blocks =
            [ (entry, entryBlock)
              (trueLabel, trueBlock)
              (falseLabel, falseBlock) ]
            |> Map.ofList
    }

    match optimizeCFG cfg |> fun optimized -> Map.tryFind entry optimized.Blocks with
    | None -> Error "Expected optimization to preserve the entry block"
    | Some optimizedEntry when optimizedEntry = entryBlock -> Ok ()
    | Some optimizedEntry ->
        Error $"Expected successor-visible Boolean to stay materialized, got: {optimizedEntry}"

let testOptimizeCFGRejectsMissingSuccessorLabel () : TestResult =
    let entry = Label "entry"
    let missing = Label "missing"
    let block : BasicBlock = {
        Label = entry
        Instrs = []
        Terminator = Jump missing
    }
    let cfg : CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, block)]
    }

    try
        optimizeCFG cfg |> ignore
        Error "Expected LIR peephole to reject a missing successor label"
    with
    | ex when ex.Message.Contains("successor label") -> Ok ()
    | ex -> Error $"Expected missing successor label crash, got: {ex.Message}"

let private functionWithInstrs name instrs : Function =
    let label = Label $"{name}_entry"
    let block : BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = Ret
    }
    {
        Name = name
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

let testConstantReturnCallsAreRewritten () : TestResult =
    let tagFunc =
        functionWithInstrs
            "Stdlib.__FingerTree.__TAG_SINGLE"
            [Mov (Physical X0, Imm 1L)]
    let caller =
        functionWithInstrs
            "caller"
            [
                SaveRegs ([X1], [])
                Call (Virtual 1, "Stdlib.__FingerTree.__TAG_SINGLE", [])
                RestoreRegs ([X1], [])
                Mov (Virtual 1, Reg (Physical X0))
                Cmp (Virtual 2, Reg (Virtual 1))
            ]
    let (Program (functions, _, _)) =
        optimizeProgram (Program ([tagFunc; caller], Map.empty, Map.empty))
    match functions |> List.tryFind (fun f -> f.Name = "caller") with
    | None ->
        Error "Expected optimized program to preserve caller"
    | Some optimizedCaller ->
        match Map.tryFind optimizedCaller.CFG.Entry optimizedCaller.CFG.Blocks with
        | None ->
            Error "Expected optimized caller to preserve entry block"
        | Some block ->
            let expected = [
                Mov (Virtual 1, Imm 1L)
                Cmp (Virtual 2, Reg (Virtual 1))
            ]
            if block.Instrs = expected then
                Ok ()
            else
                Error $"Expected constant-return call to become a move, got: {block.Instrs}"

let tests = [
    ("LIR peephole rewrites constant-return calls", testConstantReturnCallsAreRewritten)
    ("LIR peephole removes self-moves from allocated function", testRemoveSelfMovesFromAllocatedFunction)
    ("LIR peephole removes floating copy-back moves", testRemoveFloatingCopyBackMovesFromAllocatedFunction)
    ("LIR peephole keeps floating copy-back after FPhi writes source", testFloatingCopyBackKeepsMoveAfterFPhiWritesSource)
    ("LIR peephole fuses FNeg followed by dead-temp FMov", testFNegMoveChainFusesWhenTempDies)
    ("LIR peephole folds dead floating arithmetic copies", testFloatingArithmeticMoveChainsFuseWhenTempsDie)
    ("LIR peephole keeps live floating arithmetic temporaries", testFloatingArithmeticMoveChainKeepsLiveTemp)
    ("LIR peephole keeps live separated FAdd temporaries", testSeparatedFloatAddKeepsLiveTemporary)
    ("LIR peephole sinks separated allocated FAdd", testSinkSeparatedAllocatedFloatAdd)
    ("LIR peephole sinks immediate counter update", testSinkImmediateCounterUpdatePastAccumulator)
    ("LIR peephole sinks immediate counter update past subtraction", testSinkImmediateCounterUpdatePastSubtraction)
    ("LIR peephole sinks immediate counter update past division", testSinkImmediateCounterUpdatePastDivision)
    ("LIR peephole sinks immediate counter update past product", testSinkImmediateCounterUpdatePastProduct)
    ("LIR peephole sinks immediate counter update past multiply-add", testSinkImmediateCounterUpdatePastMultiplyAdd)
    ("LIR peephole sinks immediate counter update past multiply-subtract", testSinkImmediateCounterUpdatePastMultiplySubtract)
    ("LIR peephole sinks immediate counter update past XOR", testSinkImmediateCounterUpdatePastXor)
    ("LIR peephole sinks immediate counter update past AND", testSinkImmediateCounterUpdatePastAnd)
    ("LIR peephole sinks immediate counter update past OR", testSinkImmediateCounterUpdatePastOr)
    ("LIR peephole sinks immediate counter update past left shift", testSinkImmediateCounterUpdatePastLeftShift)
    ("LIR peephole sinks immediate counter update past right shift", testSinkImmediateCounterUpdatePastRightShift)
    ("LIR peephole keeps MUL temp used by later print", testMulAddFusionKeepsLiveTempForPrint)
    ("LIR peephole fuses dead MUL/SUB temporary into MSUB", testMulSubFusionReplacesDeadTemp)
    ("LIR peephole keeps MUL/SUB temporary used by later print", testMulSubFusionKeepsLiveTempForPrint)
    ("LIR peephole exposes FMADD combine but preserves strict target rounding", testFloatMultiplyAddCombineAndTargetDecision)
    ("LIR peephole forms scalar selects from empty diamonds", testScalarDiamondFormsSelect)
    ("LIR peephole keeps multiply constants that are used later", testMulConstantKeepsLiveConstRegister)
    ("LIR peephole swaps Boolean negation branch successors", testBooleanNotBranchSwapsSuccessors)
    ("LIR peephole keeps branch Boolean used by a successor", testConditionalBranchKeepsBooleanUsedInSuccessor)
    ("LIR peephole rejects missing successor labels", testOptimizeCFGRejectsMissingSuccessorLabel)
]
