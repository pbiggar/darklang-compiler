// GraphColorDSLTests.fs - Unit tests for graph-coloring fixture parsing and execution.
//
// Keeps format validation outside the fixture DSL being validated.

module GraphColorDSLTests

open TestDSL.GraphColorFormat
open TestDSL.GraphColorTestRunner

type TestResult = Result<unit, string>

let testParsesAndRunsMultipleGraphCases () : TestResult =
    let content =
        """---NAME---
edge
---VERTICES---
0 1
---EDGES---
0-1
---AVAILABLE-COLORS---
8
---EXPECT-CHROMATIC---
2
---EXPECT-DIFFERENT---
0-1

---NAME---
move preference
---VERTICES---
0 1 2
---EDGES---
1-2
---AVAILABLE-COLORS---
2
---PREFER---
0-1
---MOVE-PREFER---
0-2
---EXPECT-SAME---
0-2
"""

    match parseGraphColorFileContent "graphs.graphcolor" content with
    | Error msg -> Error $"Expected graph fixtures to parse, got: {msg}"
    | Ok [ first; second ] ->
        let firstResult = runGraphColorTest first
        let secondResult = runGraphColorTest second
        if firstResult.Success && secondResult.Success then Ok ()
        else Error $"Expected graph fixtures to pass, got: {firstResult.Message}; {secondResult.Message}"
    | Ok cases -> Error $"Expected two graph fixtures, got {List.length cases}"

let testRejectsUnknownVerticesInEdges () : TestResult =
    let content =
        """---NAME---
bad edge
---VERTICES---
0
---EDGES---
0-1
---AVAILABLE-COLORS---
1
---EXPECT-CHROMATIC---
1
"""

    match parseGraphColorFileContent "bad.graphcolor" content with
    | Error msg when msg.Contains "vertex 1" -> Ok ()
    | Error msg -> Error $"Expected unknown-vertex validation, got: {msg}"
    | Ok _ -> Error "Expected an edge with an unknown vertex to be rejected"

let tests = [
    ("graph-color DSL parses and runs multiple cases", testParsesAndRunsMultipleGraphCases)
    ("graph-color DSL rejects unknown edge vertices", testRejectsUnknownVerticesInEdges)
]
