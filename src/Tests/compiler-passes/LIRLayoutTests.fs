// LIRLayoutTests.fs - Tests deterministic CFG block layout for backend fallthrough.

module LIRLayoutTests

type TestResult = Result<unit, string>

let private branchFixture () : LIR.CFG =
    let entry = LIR.Label "entry"
    let trueBlock = LIR.Label "a_true"
    let falseBlock = LIR.Label "z_false"
    let join = LIR.Label "join"
    let block label terminator : LIR.BasicBlock = {
        Label = label
        Instrs = []
        Terminator = terminator
    }
    {
        Entry = entry
        Blocks = Map.ofList [
            entry, block entry (LIR.Branch (LIR.Physical LIR.X0, trueBlock, falseBlock))
            trueBlock, block trueBlock (LIR.Jump join)
            falseBlock, block falseBlock (LIR.Jump join)
            join, block join LIR.Ret
        ]
    }

let testLayoutFollowsFalseAndJumpSuccessors () : TestResult =
    match LIR.layoutBlocks (branchFixture ()) with
    | Error e -> Error e
    | Ok blocks ->
        let labels = blocks |> List.map (fun block -> block.Label)
        let expected = [LIR.Label "entry"; LIR.Label "z_false"; LIR.Label "join"; LIR.Label "a_true"]
        if labels = expected then Ok ()
        else Error $"Expected deterministic fallthrough layout {expected}, got {labels}"

let testLayoutLeavesMissingSuccessorValidationToConsumers () : TestResult =
    let entry = LIR.Label "entry"
    let missing = LIR.Label "missing"
    let entryBlock : LIR.BasicBlock = {
        Label = entry
        Instrs = []
        Terminator = LIR.Jump missing
    }
    let cfg : LIR.CFG = { Entry = entry; Blocks = Map.ofList [entry, entryBlock] }

    match LIR.layoutBlocks cfg with
    | Ok [block] when block.Label = entry -> Ok ()
    | Ok blocks -> Error $"Expected only the entry block, got {blocks |> List.map (fun block -> block.Label)}"
    | Error e -> Error $"Layout preempted consumer validation of a missing successor: {e}"

let testLayoutReportsMissingEntryBlock () : TestResult =
    let entry = LIR.Label "entry"
    let cfg : LIR.CFG = { Entry = entry; Blocks = Map.empty }

    match LIR.layoutBlocks cfg with
    | Error e when e.Contains "missing entry block" -> Ok ()
    | Error e -> Error $"Expected missing entry block error, got '{e}'"
    | Ok _ -> Error "Expected layout to reject a missing entry block"

let tests : (string * (unit -> TestResult)) list = [
    ("LIR layout follows false and jump successor chains", testLayoutFollowsFalseAndJumpSuccessors)
    ("LIR layout leaves missing successor validation to consumers", testLayoutLeavesMissingSuccessorValidationToConsumers)
    ("LIR layout reports missing entry block", testLayoutReportsMissingEntryBlock)
]
