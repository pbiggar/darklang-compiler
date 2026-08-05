// ParallelMoveDSLTests.fs - Unit tests for parallel-move fixture parsing and execution.
//
// Validates the DSL boundary without relying on the fixtures that it loads.

module ParallelMoveDSLTests

open TestDSL.ParallelMoveFormat
open TestDSL.ParallelMoveTestRunner

type TestResult = Result<unit, string>

let testParsesAndRunsMultipleMoveCases () : TestResult =
    let content =
        """---NAME---
simple
---INPUT-MOVES---
X1 <- Reg X2
---OUTPUT-ARM64---
MOV_reg(X1, X2)

---NAME---
swap
---INPUT-MOVES---
X1 <- Reg X2
X2 <- Reg X1
---OUTPUT-ARM64---
MOV_reg(X16, X1)
MOV_reg(X1, X2)
MOV_reg(X2, X16)
"""

    match parseParallelMoveFileContent "moves.parallelmoves" content with
    | Error msg -> Error $"Expected move fixtures to parse, got: {msg}"
    | Ok [ first; second ] ->
        let firstResult = runParallelMoveTest first
        let secondResult = runParallelMoveTest second
        if firstResult.Success && secondResult.Success then Ok ()
        else Error $"Expected move fixtures to pass, got: {firstResult.Message}; {secondResult.Message}"
    | Ok cases -> Error $"Expected two move fixtures, got {List.length cases}"

let testRejectsVirtualDestination () : TestResult =
    let content =
        """---NAME---
virtual destination
---INPUT-MOVES---
v0 <- Reg X1
---OUTPUT-ARM64---
MOV_reg(X0, X1)
"""

    match parseParallelMoveFileContent "bad.parallelmoves" content with
    | Error msg when msg.Contains "physical register" -> Ok ()
    | Error msg -> Error $"Expected physical-register validation, got: {msg}"
    | Ok _ -> Error "Expected a virtual move destination to be rejected"

let tests = [
    ("parallel-move DSL parses and runs multiple cases", testParsesAndRunsMultipleMoveCases)
    ("parallel-move DSL rejects virtual destinations", testRejectsVirtualDestination)
]
