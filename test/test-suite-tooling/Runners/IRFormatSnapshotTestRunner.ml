(*
   IRFormatSnapshotTestRunner.ml - Executes exact IR formatter snapshot fixtures.
   Formats typed ANF, MIR, or LIR inputs and reports stable expected/actual diagnostics.
*)
(* Compare full formatter output and preserve original fixture registration and diagnostics. *)
open Dark_compiler
open IRFormatSnapshotFormat
let runIRFormatSnapshotTest (test:irFormatSnapshotTest)=
 let actual=Common.normalizeLineEndings (match test.input with ANFInput program->ANFPrinter.formatANF program|MIRInput program->MIRPrinter.formatMIR program|LIRInput program->LIRPrinter.formatLIR program) in
 if actual=test.expected then {TestOutcome.success=true;message="Test passed";expected=None;actual=None}
 else {TestOutcome.success=false;message="Formatted IR did not match";expected=Some test.expected;actual=Some actual}
let loadIRFormatSnapshotTests path=
 if not (TestFileIO.exists path) then Error ("IR-format test file not found: "^path) else
 try parseIRFormatSnapshotFileContent path (FileIO.readText path) with exn->Error ("Failed to read IR-format test file "^path^": "^Printexc.to_string exn)
let tests testFiles=
 let optional=function None->""|Some value->"Some("^value^")" in
 let testsForFile path=match loadIRFormatSnapshotTests path with
 |Error msg->["parse "^Filename.basename path,(fun ()->Error msg)]
 |Ok cases->List.map (fun test->test.name,(fun ()->let result=runIRFormatSnapshotTest test in if result.TestOutcome.success then Ok () else Error (result.TestOutcome.message^"\nExpected:\n"^optional result.TestOutcome.expected^"\nActual:\n"^optional result.TestOutcome.actual))) cases in
 Array.to_list testFiles |> List.sort StringOrder.compare |> List.concat_map testsForFile
