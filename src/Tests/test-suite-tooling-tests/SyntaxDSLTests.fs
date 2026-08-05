// SyntaxDSLTests.fs - Unit tests for the syntax fixture parser and runner.
//
// Keeps the DSL implementation honest without expressing its own behavior in the DSL.

module SyntaxDSLTests

open TestDSL.SyntaxFormat
open TestDSL.SyntaxTestRunner

type TestResult = Result<unit, string>

let testParsesMultipleSyntaxCases () : TestResult =
    let content =
        """---NAME---
compiler to interpreter
---PARSE-AS---
compiler
---SOURCE---
let x = 5 in x
---FORMAT-AS---
interpreter
---EXPECTED---
let x = 5L in x
---ROUNDTRIP-AS---
compiler, interpreter

---NAME---
reject compiler lambda
---PARSE-AS---
interpreter
---SOURCE---
let inc = (x: Int64) => x + 1
---EXPECT-ERROR---
Unexpected
"""

    match parseSyntaxFileContent "syntax.syntax" content with
    | Ok [ first; second ]
        when first.Name = "compiler to interpreter"
             && first.ParseAs = Compiler
             && first.FormatAs = Some Interpreter
             && first.RoundtripAs = [ Compiler; Interpreter ]
             && second.ExpectedError = Some "Unexpected" ->
        Ok ()
    | Ok cases -> Error $"Expected two fully parsed syntax cases, got {cases}"
    | Error msg -> Error $"Expected syntax cases to parse, got: {msg}"

let testRejectsExpectedWithoutFormatTarget () : TestResult =
    let content =
        """---NAME---
missing format target
---PARSE-AS---
compiler
---SOURCE---
1
---EXPECTED---
1
"""

    match parseSyntaxFileContent "invalid.syntax" content with
    | Error msg when msg.Contains "FORMAT-AS" -> Ok ()
    | Error msg -> Error $"Expected FORMAT-AS validation error, got: {msg}"
    | Ok _ -> Error "Expected EXPECTED without FORMAT-AS to be rejected"

let testRunsFormattingAndRoundtripChecks () : TestResult =
    let testCase =
        { Name = "compiler to interpreter"
          ParseAs = Compiler
          Source = "let x = 5 in Stdlib.Int64.add(x, 1)"
          ExpectedError = None
          FormatAs = Some Interpreter
          ExpectedFormat = Some "let x = 5L in Stdlib.Int64.add x 1L"
          RoundtripAs = [ Compiler; Interpreter ]
          SourceFile = "syntax.syntax" }

    let result = runSyntaxTest testCase
    if result.Success then Ok ()
    else Error $"Expected syntax runner success, got: {result.Message}"

let tests = [
    ("syntax DSL parses multiple cases", testParsesMultipleSyntaxCases)
    ("syntax DSL validates formatting sections", testRejectsExpectedWithoutFormatTarget)
    ("syntax DSL runs format and roundtrip checks", testRunsFormattingAndRoundtripChecks)
]
