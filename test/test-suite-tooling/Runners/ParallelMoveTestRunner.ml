(*
   ParallelMoveTestRunner.fs - Executes parallel-move lowering fixtures.
   Lowers LIR TailArgMoves and compares the complete symbolic ARM64 sequence.
*)
[@@@warning "-42"]
open Dark_compiler
open ParallelMoveFormat
let context:ARM64CodeGenTypes.codeGenContext={ARM64CodeGenTypes.target=ARM64.targetConfigFor Platform.LinuxARM64;options=ARM64CodeGenTypes.defaultOptions;sumShapeRegistry=StringOrder.Map.empty;recordRegistry=StringOrder.Map.empty;rawSlotInitRetainTargets=None;closurePayloadSizes=StringOrder.Map.empty;closureCaptureTypes=StringOrder.Map.empty;functionNames=FunctionIdMap.empty;functionName="parallel_move_fixture";instructionSite="fixture_0";stackSize=0;usedCalleeSaved=[];usedCalleeSavedF=[];heapOverflowLabel="__heap_oom_parallel_move_fixture";recordLirOpExpansion=None}
let render instructions=List.map PassTestRunner.prettyPrintARM64Instr instructions |> String.concat "\n"
let runParallelMoveTest (test:parallelMoveTest)=match ARM64Instructions.convertInstr context (LIR.TailArgMoves test.moves) with
 |Error msg->{TestOutcome.success=false;message="Parallel-move lowering failed: "^msg;expected=None;actual=None}
 |Ok actual when actual=test.expected->{TestOutcome.success=true;message="Test passed";expected=None;actual=None}
 |Ok actual->{TestOutcome.success=false;message="Parallel-move ARM64 output did not match";expected=Some (render test.expected);actual=Some (render actual)}
let loadParallelMoveTests path=
 if not (TestFileIO.exists path) then Error ("Parallel-move test file not found: "^path) else
 try parseParallelMoveFileContent path (FileIO.readText path) with exn->Error ("Failed to read parallel-move test file "^path^": "^Printexc.to_string exn)
let tests testFiles=
 let testsForFile path=match loadParallelMoveTests path with
 |Error msg->["parse "^Filename.basename path,(fun ()->Error msg)]
 |Ok cases->List.map (fun test->test.name,(fun ()->let result=runParallelMoveTest test in if result.TestOutcome.success then Ok () else match result.TestOutcome.expected,result.TestOutcome.actual with Some expected,Some actual->Error (result.TestOutcome.message^"\nExpected:\n"^expected^"\nActual:\n"^actual)|_->Error result.TestOutcome.message)) cases in
 Array.to_list testFiles |> List.sort StringOrder.compare |> List.concat_map testsForFile
