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

let testParsesAndRunsCodegenErrorCase () : TestResult =
    let content =
        """---NAME---
unsupported register
---INPUT-LIR---
X24 <- Mov(Imm 1)
Ret
---EXPECT-CODEGEN-ERROR---
X24
"""

    match parseLIRExecutionFileContent "error.lirexec" content with
    | Error msg -> Error $"Expected codegen-error LIR case to parse, got: {msg}"
    | Ok [ test ] -> runLIRExecutionTest test
    | Ok cases -> Error $"Expected one codegen-error LIR case, got {List.length cases}"

let testRejectsMixedOutcomeKinds () : TestResult =
    let content =
        """---NAME---
mixed outcomes
---INPUT-LIR---
X24 <- Mov(Imm 1)
Ret
---EXPECT-EXIT---
0
---EXPECT-CODEGEN-ERROR---
X24
"""

    match parseLIRExecutionFileContent "bad.lirexec" content with
    | Error msg when msg.Contains "cannot be combined" -> Ok ()
    | Error msg -> Error $"Expected mixed-outcome validation, got: {msg}"
    | Ok _ -> Error "Expected codegen and process outcomes to be mutually exclusive"

let tests = [
    ("LIR-execution DSL parses and runs an exit case", testParsesAndRunsExitCase)
    ("LIR-execution DSL requires an expectation", testRequiresAnExpectation)
    ("LIR-execution DSL parses and runs a codegen-error case", testParsesAndRunsCodegenErrorCase)
    ("LIR-execution DSL rejects mixed outcome kinds", testRejectsMixedOutcomeKinds)
]
