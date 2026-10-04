(* GraphColorDSLTests.fs - Unit tests for graph-coloring fixture parsing and execution.
   Keeps format validation outside the fixture DSL being validated. *)
open Dark_compiler
module G=GraphColorFormat
module R=GraphColorTestRunner
type testResult=(unit,string) result
let testParsesAndRunsMultipleGraphCases ()=
 let content={fixture|---NAME---
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
|fixture} in
 match G.parseGraphColorFileContent "graphs.graphcolor" content with
 |Error message->Error ("Expected graph fixtures to parse, got: "^message)
 |Ok [first;second]->let firstResult=R.runGraphColorTest first in let secondResult=R.runGraphColorTest second in
 if firstResult.TestOutcome.success && secondResult.TestOutcome.success then Ok () else Error ("Expected graph fixtures to pass, got: "^firstResult.TestOutcome.message^"; "^secondResult.TestOutcome.message)
 |Ok cases->Error (Printf.sprintf "Expected two graph fixtures, got %d" (List.length cases))
let testRejectsUnknownVerticesInEdges ()=
 let content={fixture|---NAME---
bad edge
---VERTICES---
0
---EDGES---
0-1
---AVAILABLE-COLORS---
1
---EXPECT-CHROMATIC---
1
|fixture} in
 match G.parseGraphColorFileContent "bad.graphcolor" content with
 |Error message when HostText.contains message "vertex 1"->Ok ()
 |Error message->Error ("Expected unknown-vertex validation, got: "^message)
 |Ok _->Error "Expected an edge with an unknown vertex to be rejected"
let tests=["graph-color DSL parses and runs multiple cases",testParsesAndRunsMultipleGraphCases;"graph-color DSL rejects unknown edge vertices",testRejectsUnknownVerticesInEdges]
