// TestRunner.fs - Standalone test runner for DSL-based tests
//
// Discovers and runs test files from passes/ directories.

module TestRunner.Main

open System.IO
open TestDSL.PassTestRunner

[<EntryPoint>]
let main args =
    printfn "=== Running DSL-based Tests ==="
    printfn ""

    // Find all MIR→LIR tests
    let testDir = "passes/mir2lir"
    let mir2lirTests =
        if Directory.Exists testDir then
            Directory.GetFiles(testDir, "*.mir2lir")
        else
            printfn "Test directory not found: %s" testDir
            [||]

    let mutable passed = 0
    let mutable failed = 0

    printfn "Running MIR→LIR tests from %s" testDir
    printfn ""

    for testPath in mir2lirTests do
        let testName = Path.GetFileName testPath
        printf "  %s... " testName

        match loadMIR2LIRTest testPath with
        | Ok (input, expected) ->
            let result = runMIR2LIRTest input expected
            if result.Success then
                printfn "✓ PASS"
                passed <- passed + 1
            else
                printfn "✗ FAIL"
                printfn "    %s" result.Message
                match result.Expected, result.Actual with
                | Some exp, Some act ->
                    printfn "    Expected:"
                    for line in exp.Split('\n') do
                        printfn "      %s" line
                    printfn "    Actual:"
                    for line in act.Split('\n') do
                        printfn "      %s" line
                | _ -> ()
                failed <- failed + 1
        | Error msg ->
            printfn "✗ ERROR"
            printfn "    Failed to load test: %s" msg
            failed <- failed + 1

    printfn ""
    printfn "=== Results ==="
    printfn "Passed: %d" passed
    printfn "Failed: %d" failed

    if failed = 0 then 0 else 1
