// E2EScheduling.fs - Helpers for scheduling E2E tests
//
// Groups E2E tests for execution planning (e.g., per-file batching).

module TestDSL.E2EScheduling

open TestDSL.E2EFormat

/// A batch of E2E tests from the same source file
type E2ETestBatch = {
    SourceFile: string
    Tests: (int * E2ETest) list
}

/// Group tests by source file (no ordering guarantees)
let groupTestsBySourceFile (tests: (int * E2ETest) list) : E2ETestBatch list =
    tests
    |> List.fold
        (fun grouped (idx, test) ->
            let existing = Map.tryFind test.SourceFile grouped |> Option.defaultValue []
            Map.add test.SourceFile ((idx, test) :: existing) grouped)
        Map.empty
    |> Map.toList
    |> List.map (fun (file, testsForFile) ->
        { SourceFile = file; Tests = testsForFile })
