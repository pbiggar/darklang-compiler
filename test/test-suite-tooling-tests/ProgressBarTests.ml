(* ProgressBarTests.ml - Unit tests for the test runner progress bar
   Ensures the progress bar handles over-completion without crashing. *)
open Dark_compiler
type testResult=(unit,string) result
let captureProgressError run=let (),_,error=TestCapture.run run in error
let testProgressBarHandlesOverCompletion ()=
 let state=ProgressBar.create "Progress" 1 in
 try ProgressBar.increment state true;ProgressBar.increment state true;Ok ()
 with exn->Error ("ProgressBar threw exception: "^Printexc.to_string exn)
let testProgressBarClampsOverCompletionDisplay ()=
 let state=ProgressBar.create "Progress" 1 in
 let output=captureProgressError (fun ()->ProgressBar.increment state true;ProgressBar.increment state true) in
 if Text.contains output "2/1" then Error ("Expected over-completion display to clamp to total, got: "^output)
 else if Text.contains output "1/1" then Ok ()
 else Error ("Expected progress output to include completed count, got: "^output)
let tests=["handles over-completion",testProgressBarHandlesOverCompletion;"clamps over-completion display",testProgressBarClampsOverCompletionDisplay]
