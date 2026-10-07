(*
   ParallelMoveDSLTests.fs - Unit tests for parallel-move fixture parsing and execution.
   Validates the DSL boundary without relying on the fixtures that it loads.
*)
open Dark_compiler
open ParallelMoveFormat
open ParallelMoveTestRunner
type testResult=(unit,string) result
let testParsesAndRunsMultipleMoveCases ()=
 let content={fixture|---NAME---
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
|fixture} in
 match parseParallelMoveFileContent "moves.parallelmoves" content with
 |Error msg->Error ("Expected move fixtures to parse, got: "^msg)
 |Ok [first;second]->let firstResult=runParallelMoveTest first in let secondResult=runParallelMoveTest second in if firstResult.TestOutcome.success && secondResult.TestOutcome.success then Ok () else Error ("Expected move fixtures to pass, got: "^firstResult.TestOutcome.message^"; "^secondResult.TestOutcome.message)
 |Ok cases->Error (Printf.sprintf "Expected two move fixtures, got %d" (List.length cases))
let testRejectsVirtualDestination ()=
 let content={fixture|---NAME---
virtual destination
---INPUT-MOVES---
v0 <- Reg X1
---OUTPUT-ARM64---
MOV_reg(X0, X1)
|fixture} in
 match parseParallelMoveFileContent "bad.parallelmoves" content with
 |Error msg when HostText.contains msg "physical register"->Ok ()
 |Error msg->Error ("Expected physical-register validation, got: "^msg)
 |Ok _->Error "Expected a virtual move destination to be rejected"
let tests=["parallel-move DSL parses and runs multiple cases",testParsesAndRunsMultipleMoveCases;"parallel-move DSL rejects virtual destinations",testRejectsVirtualDestination]
