(*
   IRFormatSnapshotDSLTests.fs - Unit tests for IR formatting snapshot fixtures.
   Validates multi-case parsing and exact formatter execution outside the fixture DSL.
*)
(* Retain the original snapshot DSL tests and their exact formatter expectations. *)
open Dark_compiler
type testResult=(unit,string) result
let testParsesAndRunsMultipleIRKinds ()=
 let content="---NAME---\nANF return\n---IR---\nanf\n---INPUT---\nreturn 1\n---EXPECTED---\nreturn 1\n\n---NAME---\nLIR return\n---IR---\nlir\n---INPUT---\nX0 <- Mov(Imm 1)\nRet\n---EXPECTED---\n_start:\n  StackSize: 0\n  UsedCalleeSaved: []\n  Label \"entry\":\n    X0 <- Mov(Imm 1)\n    Ret\n" in
 match IRFormatSnapshotFormat.parseIRFormatSnapshotFileContent "format.irformat" content with
 |Error msg->Error ("Expected IR formatting cases to parse, got: "^msg)
 |Ok [first;second]->
 let firstResult=IRFormatSnapshotTestRunner.runIRFormatSnapshotTest first in
 let secondResult=IRFormatSnapshotTestRunner.runIRFormatSnapshotTest second in
 if firstResult.TestOutcome.success && secondResult.TestOutcome.success then Ok () else Error ("Expected IR formatting cases to pass, got: "^firstResult.TestOutcome.message^"; "^secondResult.TestOutcome.message)
 |Ok cases->Error (Printf.sprintf "Expected two IR formatting cases, got %d" (List.length cases))
let testRejectsUnknownIRKind ()=
 let content="---NAME---\nunknown\n---IR---\nssa\n---INPUT---\nreturn 1\n---EXPECTED---\nreturn 1\n" in
 match IRFormatSnapshotFormat.parseIRFormatSnapshotFileContent "bad.irformat" content with
 |Error msg when HostText.contains msg "ssa"->Ok ()
 |Error msg->Error ("Expected unknown IR validation, got: "^msg)
 |Ok _->Error "Expected unknown IR kind to be rejected"
let tests=["IR-format DSL parses and runs multiple IR kinds",testParsesAndRunsMultipleIRKinds;"IR-format DSL rejects unknown IR kind",testRejectsUnknownIRKind]
