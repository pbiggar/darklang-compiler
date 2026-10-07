(*
   LIRExecutionDSLTests.fs - Unit tests for executable LIR fixtures.
   Validates typed parsing, expectation rules, and native x64 execution.
*)
open Dark_compiler
open LIRExecutionFormat
open LIRExecutionTestRunner
type testResult=(unit,string) result
let testParsesAndRunsExitCase ()=match parseLIRExecutionFileContent "simple.lirexec" {fixture|---NAME---
exit 7
---INPUT-LIR---
X1 <- Mov(Imm 7)
Exit
Ret
---EXPECT-EXIT---
7
|fixture} with
 |Error msg->Error ("Expected executable LIR case to parse, got: "^msg)|Ok [test]->runLIRExecutionTest test|Ok cases->Error (Printf.sprintf "Expected one executable LIR case, got %d" (List.length cases))
let testRequiresAnExpectation ()=match parseLIRExecutionFileContent "bad.lirexec" {fixture|---NAME---
missing expectation
---INPUT-LIR---
Ret
|fixture} with
 |Error msg when Text.contains msg "expectation"->Ok ()|Error msg->Error ("Expected missing-expectation validation, got: "^msg)|Ok _->Error "Expected executable LIR case without an expectation to be rejected"
let testParsesAndRunsCodegenErrorCase ()=match parseLIRExecutionFileContent "error.lirexec" {fixture|---NAME---
unsupported register
---INPUT-LIR---
X24 <- Mov(Imm 1)
Ret
---EXPECT-CODEGEN-ERROR---
X24
|fixture} with
 |Error msg->Error ("Expected codegen-error LIR case to parse, got: "^msg)|Ok [test]->runLIRExecutionTest test|Ok cases->Error (Printf.sprintf "Expected one codegen-error LIR case, got %d" (List.length cases))
let testRejectsMixedOutcomeKinds ()=match parseLIRExecutionFileContent "bad.lirexec" {fixture|---NAME---
mixed outcomes
---INPUT-LIR---
X24 <- Mov(Imm 1)
Ret
---EXPECT-EXIT---
0
---EXPECT-CODEGEN-ERROR---
X24
|fixture} with
 |Error msg when Text.contains msg "cannot be combined"->Ok ()|Error msg->Error ("Expected mixed-outcome validation, got: "^msg)|Ok _->Error "Expected codegen and process outcomes to be mutually exclusive"
let tests=["LIR-execution DSL parses and runs an exit case",testParsesAndRunsExitCase;"LIR-execution DSL requires an expectation",testRequiresAnExpectation;"LIR-execution DSL parses and runs a codegen-error case",testParsesAndRunsCodegenErrorCase;"LIR-execution DSL rejects mixed outcome kinds",testRejectsMixedOutcomeKinds]
