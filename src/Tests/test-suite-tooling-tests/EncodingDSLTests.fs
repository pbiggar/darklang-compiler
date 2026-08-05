// EncodingDSLTests.fs - Unit tests for ARM64 and x64 encoding fixture extensions.
//
// Tests parser and runner behavior that cannot safely be asserted by their own DSL files.

module EncodingDSLTests

open TestDSL.ARM64EncodingFormat
open TestDSL.ARM64EncodingTestRunner
open TestDSL.X86_64EncodingFormat
open TestDSL.X86_64EncodingTestRunner

type TestResult = Result<unit, string>

let testParsesAndRunsARM64ExpectedEncodingError () : TestResult =
    let content =
        """---NAME---
invalid add immediate
---INPUT-ARM64---
ADD_imm(X0, X1, 4096)
---EXPECT-ERROR---
immediate
"""

    match parseARM64EncodingTest content with
    | Error msg -> Error $"Expected ARM64 error fixture to parse, got: {msg}"
    | Ok test ->
        let result = runARM64EncodingTest test
        if result.Success then Ok ()
        else Error $"Expected ARM64 encoding error fixture to pass, got: {result.Message}"

let testParsesMultipleX64EncodingCases () : TestResult =
    let content =
        """---NAME---
register move
---INPUT-X64---
MOV_reg(RAX, RBX)
---OUTPUT-HEX---
48 89 D8

---NAME---
forward jump
---INPUT-X64---
JMP(skip)
MOV_reg(RAX, RAX)
Label(skip)
RET
---OUTPUT-HEX---
E9 03 00 00 00 48 89 C0 C3
"""

    match parseX64EncodingFileContent "encoding.x64enc" content with
    | Ok [ first; second ] when first.Name = "register move" && second.Name = "forward jump" -> Ok ()
    | Ok cases -> Error $"Expected two x64 encoding cases, got {cases}"
    | Error msg -> Error $"Expected x64 encoding cases to parse, got: {msg}"

let testRunsX64DeferredFixupExpectation () : TestResult =
    let content =
        """---NAME---
deferred data label
---INPUT-X64---
JMP(data)
---EXPECT-FIXUPS---
data
"""

    match parseX64EncodingFileContent "fixup.x64enc" content with
    | Error msg -> Error $"Expected x64 fixup fixture to parse, got: {msg}"
    | Ok [ test ] ->
        let result = runX64EncodingTest test
        if result.Success then Ok ()
        else Error $"Expected x64 fixup fixture to pass, got: {result.Message}"
    | Ok cases -> Error $"Expected one x64 fixup case, got {List.length cases}"

let tests = [
    ("ARM64 encoding DSL supports expected errors", testParsesAndRunsARM64ExpectedEncodingError)
    ("x64 encoding DSL parses multiple cases", testParsesMultipleX64EncodingCases)
    ("x64 encoding DSL checks deferred fixups", testRunsX64DeferredFixupExpectation)
]
