// DeadCodeEliminationTests.fs - Unit tests for LIR-level function reachability
//
// Verifies tree shaking keeps function-address references that flow through
// lower-level call setup instructions, not just direct call instructions.

module DeadCodeEliminationTests

type TestResult = Result<unit, string>

let private blockWith instrs =
    { LIR.Label = LIR.Label "entry"
      LIR.Instrs = instrs
      LIR.Terminator = LIR.Ret }

let private functionWith instrs =
    { LIR.Name = "user"
      LIR.TypedParams = []
      LIR.CFG =
        { LIR.Entry = LIR.Label "entry"
          LIR.Blocks = Map.ofList [ (LIR.Label "entry", blockWith instrs) ] }
      LIR.StackSize = 0
      LIR.UsedCalleeSaved = [] }

let private expectCalls expected instrs =
    let actual = DeadCodeElimination.getCalledFunctions (functionWith instrs)
    if actual = Set.ofList expected then
        Ok ()
    else
        Error $"Expected calls {expected}, got {Set.toList actual}"

let testArgMovesFunctionAddressIsReachable () : TestResult =
    expectCalls
        ["Stdlib.List.map"]
        [ LIR.ArgMoves [ (LIR.X0, LIR.FuncAddr "Stdlib.List.map") ] ]

let tests = [
    ("arg moves function address is reachable", testArgMovesFunctionAddressIsReachable)
]

let runAll () : TestResult =
    let rec run remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> run rest
            | Error msg -> Error $"{name} test failed: {msg}"
    run tests
