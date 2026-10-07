(*
   ARM64EncodingTestRunner.fs - Test runner for ARM64 encoding tests
   Loads ARM64 encoding test files (.arm64enc), runs the ARM64 encoder,
   and compares the output with expected machine code hex values.
   Load ARM64 encoding test from file
   Check if all values in a list are different
   Format encoding mismatches for display
   Run ARM64 encoding test
   Encode each instruction
   Check each encoding matches expected
   Check if all values are different (if required)
*)
[@@@warning "-4-42"]
open Dark_compiler
open ARM64EncodingFormat
let loadARM64EncodingTest path=if not (TestFileIO.exists path) then Error ("Test file not found: "^path) else parseARM64EncodingTest (FileIO.readText path)
let hasAllDifferent values=List.length (List.sort_uniq Int32.compare values)=List.length values
let formatMismatches mismatches=if mismatches=[] then "All encodings matched" else
 List.map (fun (i,instr,expected,actual)->Printf.sprintf "Instruction %d: %s\n      Expected: 0x%08lX, Got: 0x%08lX" i (PassTestRunner.prettyPrintARM64Instr (Symbolic.ofARM64 instr)) expected actual) mismatches |> String.concat "\n    "
let encodeInstruction instr=try Ok (ARM64_Encoding.encode instr) with exn->Error (match exn with Failure message|Invalid_argument message->message|_->Printexc.to_string exn)
let runARM64EncodingTest (test:arm64EncodingTest)=
 let success={TestOutcome.success=true;message="Test passed";expected=None;actual=None} in
 let failure message expected={TestOutcome.success=false;message;expected;actual=None} in
 let rec encodeInstructions index=function []->Ok []|instr::rest->match encodeInstruction instr with Error msg->Error msg|Ok [code]->Result.map (fun codes->code::codes) (encodeInstructions (index+1) rest)|Ok codes->Error (Printf.sprintf "Instruction %d: Expected single machine code word per instruction, got %d" index (List.length codes)) in
 let rec checkExpectedErrors index instructions expected=match instructions with []->Ok ()|instr::rest->match encodeInstruction instr with
 |Error msg when Text.contains msg expected->checkExpectedErrors (index+1) rest expected
 |Error msg->Error (Printf.sprintf "Instruction %d: encoding error did not contain expected text\nExpected: %s\nActual: %s" index expected msg)
 |Ok _->Error (Printf.sprintf "Instruction %d unexpectedly encoded: %s" index (PassTestRunner.prettyPrintARM64Instr (Symbolic.ofARM64 instr))) in
 match test.expectation with
 |EncodingErrorContaining expected->(match checkExpectedErrors 0 test.instructions expected with Ok ()->success|Error msg->failure msg (Some expected))
 |EncodesTo expectedHex->match encodeInstructions 0 test.instructions with Error msg->failure msg None|Ok results->
 let triples=List.map2 (fun (instr,actual) expected->instr,actual,expected) (List.combine test.instructions results) expectedHex in
 let mismatches=List.mapi (fun i (instr,actual,expected)->if actual<>expected then Some (i,instr,expected,actual) else None) triples |> List.filter_map Fun.id in
 let allDifferentCheck=not test.assertDifferent || hasAllDifferent results in
 if mismatches=[] && allDifferentCheck then success else failure (if mismatches<>[] then formatMismatches mismatches else "ASSERT-DIFFERENT failed: not all hex values are different") None
