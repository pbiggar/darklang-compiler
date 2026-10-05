(* Linux regression for descriptor reuse across parallel compiler/test captures.
   Link against the native compiler and runner libraries; no DSL fixtures change. *)
open Dark_compiler
let running=Atomic.make true
let failure=ref None
let protect action=try action () with ex->failure:=Some (Printexc.to_string ex);Atomic.set running false
let churn ()=protect (fun ()->while Atomic.get running do
 let descriptor=Unix.openfile "/dev/null" [Unix.O_WRONLY;Unix.O_CLOEXEC] 0 in
 Fun.protect ~finally:(fun ()->Unix.close descriptor) (fun ()->Thread.yield ();ignore (Unix.write descriptor (Bytes.of_string "still-open") 0 10))
 done)
let check condition message=if not condition then failwith message
let capture ()=protect (fun ()->for _=1 to 120 do
 match TestProcess.capture "/bin/sh" ["-c";"printf output; printf error >&2; exit 17"] 10000 with
 |Ok (code,out,err)->check (code=17 && out="output" && err="error") "Process output/status changed"
 |Error message->failwith message
 done)
let captureInput ()=protect (fun ()->for _=1 to 120 do
 match TestProcess.captureWithInputAndEnvironment "/bin/sh" ["-c";"cat; printf error >&2; exit 17"] [] (Bytes.of_string "input") 10000 with
 |Ok (code,out,err)->check (code=17 && out="input" && err="error") "Process input/output/status changed"
 |Error message->failwith message
 done)
let binary=In_channel.with_open_bin "/bin/true" (fun channel->Bytes.of_string (In_channel.input_all channel))
let execute ()=protect (fun ()->for _=1 to 120 do
 let result=CompilerExecution.execute Platform.LinuxX86_64 0 binary in
 check (result.CompilerOptions.exitCode=0) ("Compiler execution failed: "^result.CompilerOptions.stderr)
 done)
let ()=
 let churner=Thread.create churn () in
 let first=Thread.create capture () and second=Thread.create capture () and third=Thread.create execute () and fourth=Thread.create captureInput () in
 List.iter Thread.join [first;second;third;fourth];Atomic.set running false;Thread.join churner;
 match !failure with None->print_endline "480 concurrent captures preserved descriptor ownership"|Some message->failwith message
