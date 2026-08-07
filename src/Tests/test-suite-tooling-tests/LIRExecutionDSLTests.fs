// LIRExecutionDSLTests.fs - Unit tests for executable LIR fixtures.
//
// Validates typed parsing, expectation rules, and native x64 execution.

module LIRExecutionDSLTests

open TestDSL.LIRExecutionFormat
open TestDSL.LIRExecutionTestRunner

type TestResult = Result<unit, string>

let testParsesAndRunsExitCase () : TestResult =
    let content =
        """---NAME---
exit 7
---INPUT-LIR---
X1 <- Mov(Imm 7)
Exit
Ret
---EXPECT-EXIT---
7
"""

    match parseLIRExecutionFileContent "simple.lirexec" content with
    | Error msg -> Error $"Expected executable LIR case to parse, got: {msg}"
    | Ok [ test ] -> runLIRExecutionTest test
    | Ok cases -> Error $"Expected one executable LIR case, got {List.length cases}"

let testRequiresAnExpectation () : TestResult =
    let content =
        """---NAME---
missing expectation
---INPUT-LIR---
Ret
"""

    match parseLIRExecutionFileContent "bad.lirexec" content with
    | Error msg when msg.Contains "expectation" -> Ok ()
    | Error msg -> Error $"Expected missing-expectation validation, got: {msg}"
    | Ok _ -> Error "Expected executable LIR case without an expectation to be rejected"

let tests = [
    ("LIR-execution DSL parses and runs an exit case", testParsesAndRunsExitCase)
    ("LIR-execution DSL requires an expectation", testRequiresAnExpectation)
]
