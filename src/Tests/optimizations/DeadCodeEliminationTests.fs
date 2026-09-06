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

let private namedFunctionWith name instrs =
    { LIR.Name = name
      LIR.TypedParams = []
      LIR.CFG =
        { LIR.Entry = LIR.Label "entry"
          LIR.Blocks = Map.ofList [ (LIR.Label "entry", blockWith instrs) ] }
      LIR.StackSize = 0
      LIR.UsedCalleeSaved = []
      LIR.CodegenFacts = None }

let private functionWith instrs = namedFunctionWith "user" instrs

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

let testFilteredFunctionsPreserveReachableSetAndInputOrder () : TestResult =
    let userFunctions =
        [ namedFunctionWith
              "user"
              [ LIR.Call (LIR.Virtual 0, "stdlib_b", [ LIR.FuncAddr "stdlib_a" ]) ] ]
    let stdlibFunctions =
        [ namedFunctionWith "unused" []
          namedFunctionWith "stdlib_c" []
          namedFunctionWith "stdlib_a" []
          namedFunctionWith "stdlib_b" [] ]
    let callGraph =
        Map.ofList
            [ "stdlib_a", Set.empty
              "stdlib_b", Set.ofList [ "stdlib_c" ]
              "stdlib_c", Set.empty
              "unused", Set.empty ]
    let actual =
        DeadCodeElimination.filterFunctions callGraph userFunctions stdlibFunctions
        |> List.map (fun function_ -> function_.Name)
    let expected = [ "stdlib_c"; "stdlib_a"; "stdlib_b" ]
    if actual = expected then
        Ok ()
    else
        Error $"Expected reachable functions in order {expected}, got {actual}"

let tests = [
    ("arg moves function address is reachable", testArgMovesFunctionAddressIsReachable)
    ("filtered functions preserve reachable set and input order", testFilteredFunctionsPreserveReachableSetAndInputOrder)
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
