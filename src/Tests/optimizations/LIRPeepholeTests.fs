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

let tests = [
    ("LIR peephole removes self-moves from allocated function", testRemoveSelfMovesFromAllocatedFunction)
]
