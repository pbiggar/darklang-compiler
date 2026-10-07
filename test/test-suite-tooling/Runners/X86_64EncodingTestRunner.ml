(*
   X86_64EncodingTestRunner.fs - Executes x64 encoding and label-resolution fixtures.
   Reports final byte streams and deferred fixup labels with stable diagnostics.
*)
[@@@warning "-42"]
open Dark_compiler
open X86_64EncodingFormat
let bytesToHex bytes=Bytes.to_seq bytes |> List.of_seq |> List.map (fun value->Printf.sprintf "%02X" (Char.code value)) |> String.concat " "
let success={TestOutcome.success=true;message="Test passed";expected=None;actual=None}
let failure message expected actual={TestOutcome.success=false;message;expected;actual}
let stringList values=match values with a::b::c::_::_->"["^String.concat "; " [a;b;c]^"; ... ]"|_->"["^String.concat "; " values^"]"
let runX64EncodingTest (test:x64EncodingTest)=match X86_64_Resolve.resolveAndEncode test.instructions,test.expectation with
 |Error msg,ResolutionErrorContaining expected when HostText.contains msg expected->success
 |Error msg,ResolutionErrorContaining expected->failure "Resolution error did not contain expected text" (Some expected) (Some msg)
 |Ok _,ResolutionErrorContaining expected->failure "Expected x64 resolution to fail" (Some expected) (Some "Resolution succeeded")
 |Error msg,ResolvesTo _->failure ("x64 encoding/resolution failed: "^msg) None None
 |Ok result,ResolvesTo (expectedBytes,expectedFixups)->let actualFixups=List.map (fun (fixup:X86_64_Resolve.fixup)->fixup.X86_64_Resolve.targetLabel) result.X86_64_Resolve.deferredFixups in
 match expectedBytes with
 |Some expected when expected<>result.X86_64_Resolve.machineCode->failure "x64 machine code did not match" (Some (bytesToHex expected)) (Some (bytesToHex result.X86_64_Resolve.machineCode))
 |_ when expectedFixups<>actualFixups->failure "x64 deferred fixups did not match" (Some (stringList expectedFixups)) (Some (stringList actualFixups))
 |_->success
let loadX64EncodingTests path=if not (TestFileIO.exists path) then Error ("x64 encoding test file not found: "^path) else try parseX64EncodingFileContent path (HostFile.readText path) with exn->Error ("Failed to read x64 encoding test file "^path^": "^HostFile.errorMessage path exn)
let tests testFiles=let testsForFile path=match loadX64EncodingTests path with Error msg->["parse "^Filename.basename path,(fun ()->Error msg)]|Ok cases->List.map (fun test->test.name,(fun ()->let result=runX64EncodingTest test in if result.TestOutcome.success then Ok () else match result.TestOutcome.expected,result.TestOutcome.actual with Some expected,Some actual->Error (result.TestOutcome.message^"\nExpected: "^expected^"\nActual: "^actual)|_->Error result.TestOutcome.message)) cases in Array.to_list testFiles |> List.sort StringOrder.compare |> List.concat_map testsForFile
