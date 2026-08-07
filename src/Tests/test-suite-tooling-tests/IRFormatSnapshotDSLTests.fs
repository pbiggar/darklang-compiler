// IRFormatSnapshotDSLTests.fs - Unit tests for IR formatting snapshot fixtures.
//
// Validates multi-case parsing and exact formatter execution outside the fixture DSL.

module IRFormatSnapshotDSLTests

open TestDSL.IRFormatSnapshotFormat
open TestDSL.IRFormatSnapshotTestRunner

type TestResult = Result<unit, string>

let testParsesAndRunsMultipleIRKinds () : TestResult =
    let content =
        """---NAME---
ANF return
---IR---
anf
---INPUT---
return 1
---EXPECTED---
return 1

---NAME---
LIR return
---IR---
lir
---INPUT---
X0 <- Mov(Imm 1)
Ret
---EXPECTED---
_start:
  StackSize: 0
  UsedCalleeSaved: []
  Label "entry":
    X0 <- Mov(Imm 1)
    Ret
"""

    match parseIRFormatSnapshotFileContent "format.irformat" content with
    | Error msg -> Error $"Expected IR formatting cases to parse, got: {msg}"
    | Ok [ first; second ] ->
        let firstResult = runIRFormatSnapshotTest first
        let secondResult = runIRFormatSnapshotTest second
        if firstResult.Success && secondResult.Success then Ok ()
        else Error $"Expected IR formatting cases to pass, got: {firstResult.Message}; {secondResult.Message}"
    | Ok cases -> Error $"Expected two IR formatting cases, got {List.length cases}"

let testRejectsUnknownIRKind () : TestResult =
    let content =
        """---NAME---
unknown
---IR---
ssa
---INPUT---
return 1
---EXPECTED---
return 1
"""

    match parseIRFormatSnapshotFileContent "bad.irformat" content with
    | Error msg when msg.Contains "ssa" -> Ok ()
    | Error msg -> Error $"Expected unknown IR validation, got: {msg}"
    | Ok _ -> Error "Expected unknown IR kind to be rejected"

let tests = [
    ("IR-format DSL parses and runs multiple IR kinds", testParsesAndRunsMultipleIRKinds)
    ("IR-format DSL rejects unknown IR kind", testRejectsUnknownIRKind)
]
