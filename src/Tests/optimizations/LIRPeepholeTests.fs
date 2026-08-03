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

let tests = [
    ("LIR peephole removes self-moves from allocated function", testRemoveSelfMovesFromAllocatedFunction)
    ("LIR peephole removes floating copy-back moves", testRemoveFloatingCopyBackMovesFromAllocatedFunction)
    ("LIR peephole keeps floating copy-back after FPhi writes source", testFloatingCopyBackKeepsMoveAfterFPhiWritesSource)
    ("LIR peephole fuses FNeg followed by dead-temp FMov", testFNegMoveChainFusesWhenTempDies)
    ("LIR peephole keeps MUL temp used by later print", testMulAddFusionKeepsLiveTempForPrint)
    ("LIR peephole fuses dead MUL/SUB temporary into MSUB", testMulSubFusionReplacesDeadTemp)
    ("LIR peephole keeps MUL/SUB temporary used by later print", testMulSubFusionKeepsLiveTempForPrint)
    ("LIR peephole keeps multiply constants that are used later", testMulConstantKeepsLiveConstRegister)
    ("LIR peephole rejects missing successor labels", testOptimizeCFGRejectsMissingSuccessorLabel)
]
