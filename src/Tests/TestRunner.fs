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

    let mutable passed = 0
    let mutable failed = 0

    // Run ANF→MIR tests
    let anf2mirDir = "passes/anf2mir"
    if Directory.Exists anf2mirDir then
        let anf2mirTests = Directory.GetFiles(anf2mirDir, "*.anf2mir")
        if anf2mirTests.Length > 0 then
            printfn "Running ANF→MIR tests from %s" anf2mirDir
            printfn ""

            for testPath in anf2mirTests do
                let testName = Path.GetFileName testPath
                printf "  %s... " testName

                match loadANF2MIRTest testPath with
                | Ok (input, expected) ->
                    let result = runANF2MIRTest input expected
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

    // Run MIR→LIR tests
    let mir2lirDir = "passes/mir2lir"
    if Directory.Exists mir2lirDir then
        let mir2lirTests = Directory.GetFiles(mir2lirDir, "*.mir2lir")
        if mir2lirTests.Length > 0 then
            printfn "Running MIR→LIR tests from %s" mir2lirDir
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
