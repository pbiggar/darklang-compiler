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
    { LIR.Id = TestIds.functionIdForName name
      LIR.Name = name
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
    let expectedIds = expected |> List.map TestIds.functionIdForName |> Set.ofList
    if actual = expectedIds then
        Ok ()
    else
        Error $"Expected calls {expected}, got {Set.toList actual}"

let testArgMovesFunctionAddressIsReachable () : TestResult =
    expectCalls
        ["Darklang.Stdlib.List.map"]
        [ LIR.ArgMoves [
            (LIR.X0, LIR.FuncAddr (TestIds.functionIdForName "Darklang.Stdlib.List.map"))
          ] ]

let testListDisplayHelperIsReachableByCanonicalIdentity () : TestResult =
    expectCalls
        ["Darklang.Stdlib.List.__toDisplayString_i64"]
        [ LIR.PrintSum (
              LIR.Physical LIR.X0,
              [("Values", 0, Some (AST.TList AST.TInt64))],
              false
          ) ]

let testFilteredFunctionsPreserveReachableSetAndInputOrder () : TestResult =
    let userFunctions =
        [ namedFunctionWith
              "user"
              [ LIR.Call (
                    LIR.Virtual 0,
                    TestIds.functionIdForName "stdlib_b",
                    [ LIR.FuncAddr (TestIds.functionIdForName "stdlib_a") ]
                ) ] ]
    let stdlibFunctions =
        [ namedFunctionWith "unused" []
          namedFunctionWith "stdlib_c" []
          namedFunctionWith "stdlib_a" []
          namedFunctionWith "stdlib_b" [] ]
    let callGraph =
        Map.ofList
            [ TestIds.functionIdForName "stdlib_a", Set.empty
              TestIds.functionIdForName "stdlib_b", Set.ofList [ TestIds.functionIdForName "stdlib_c" ]
              TestIds.functionIdForName "stdlib_c", Set.empty
              TestIds.functionIdForName "unused", Set.empty ]
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
    ("list display helper is reachable by canonical identity", testListDisplayHelperIsReachableByCanonicalIdentity)
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
