// TestRunner.fs - Standalone test runner for DSL-based tests
//
// Discovers and runs test files from passes/ directories.

module TestRunner.Main

open System.IO
open System.Reflection
open TestDSL.PassTestRunner
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner

[<EntryPoint>]
let main args =
    printfn "=== Running DSL-based Tests ==="
    printfn ""

    // Get the directory where the assembly is located (where test files are copied)
    let assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let mutable passed = 0
    let mutable failed = 0

    // Run ANF→MIR tests
    let anf2mirDir = Path.Combine(assemblyDir, "passes/anf2mir")
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
    let mir2lirDir = Path.Combine(assemblyDir, "passes/mir2lir")
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

    // Run LIR→ARM64 tests
    let lir2arm64Dir = Path.Combine(assemblyDir, "passes/lir2arm64")
    if Directory.Exists lir2arm64Dir then
        let lir2arm64Tests = Directory.GetFiles(lir2arm64Dir, "*.lir2arm64")
        if lir2arm64Tests.Length > 0 then
            printfn "Running LIR→ARM64 tests from %s" lir2arm64Dir
            printfn ""

            for testPath in lir2arm64Tests do
                let testName = Path.GetFileName testPath
                printf "  %s... " testName

                match loadLIR2ARM64Test testPath with
                | Ok (input, expected) ->
                    let result = runLIR2ARM64Test input expected
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

    // Run E2E tests
    let e2eDir = Path.Combine(assemblyDir, "e2e")
    if Directory.Exists e2eDir then
        let e2eTests = Directory.GetFiles(e2eDir, "*.test", SearchOption.AllDirectories)
        if e2eTests.Length > 0 then
            printfn "Running E2E tests from %s" e2eDir
            printfn ""

            // Find compiler executable - it's in the same bin directory as Tests
            // assemblyDir is: /path/to/bin/Tests/Debug/net9.0
            // compiler is at: /path/to/bin/DarkCompiler/Debug/net9.0/DarkCompiler.dll
            let binDir = Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName(assemblyDir)))
            let compilerPath = Path.Combine(binDir, "DarkCompiler/Debug/net9.0/DarkCompiler.dll")

            for testPath in e2eTests do
                let testName = Path.GetFileName testPath
                printf "  %s... " testName

                match parseE2ETest testPath with
                | Ok test ->
                    let result = runE2ETest test compilerPath
                    if result.Success then
                        printfn "✓ PASS"
                        passed <- passed + 1
                    else
                        printfn "✗ FAIL"
                        printfn "    %s" result.Message
                        match result.ExitCode with
                        | Some code when code <> test.ExpectedExitCode ->
                            printfn "    Expected exit code: %d" test.ExpectedExitCode
                            printfn "    Actual exit code: %d" code
                        | _ -> ()
                        // Always show stdout/stderr on failure
                        match result.Stdout with
                        | Some stdout when stdout.Trim() <> "" ->
                            printfn "    Stdout: %s" (stdout.Trim())
                        | _ -> ()
                        match result.Stderr with
                        | Some stderr when stderr.Trim() <> "" ->
                            printfn "    Stderr: %s" (stderr.Trim())
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
