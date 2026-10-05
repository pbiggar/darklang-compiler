(* Execution.fs - Run generated binaries through the host process boundary. *)
[@@@warning "-4"]
module O=CompilerOptions
external signalNumber : int -> int = "dark_execution_signal_number"
let exitCode=function Unix.WEXITED code->code|Unix.WSIGNALED signal|Unix.WSTOPPED signal->128+signalNumber signal
let wait pid=let rec loop ()=try snd (Unix.waitpid [] pid) with Unix.Unix_error (Unix.EINTR,_,_)->loop () in exitCode (loop ())
let elapsed start=HostClock.milliseconds ()-.start
let detail verbosity duration=if verbosity>=2 then (let scaled=duration*.10. in let lower=Float.floor scaled in let rounded=if scaled-.lower=0.5 then (if Float.rem lower 2.=0. then lower else lower+.1.) else Float.round scaled in Output.println ("      "^HostFloat.roundTrip (rounded/.10.)^"ms"))
let streamText bytes=
 let encoding=if String.starts_with ~prefix:"\000\000\254\255" bytes then Some "text/plain; charset=utf-32be" else None in
 HostPackageIO.decodeContent encoding bytes
let captured info input=
 let stdinRead,stdinWrite=Unix.pipe ~cloexec:true () in let stdoutRead,stdoutWrite=Unix.pipe ~cloexec:true () in let stderrRead,stderrWrite=Unix.pipe ~cloexec:true () in
 (* Parallel test execution can reuse closed descriptor numbers. Cleanup must
    only close descriptors still owned by this capture, never retired numbers. *)
 let opened=ref [stdinRead;stdinWrite;stdoutRead;stdoutWrite;stderrRead;stderrWrite] in
 let close fd=if List.mem fd !opened then (opened:=List.filter ((<>) fd) !opened;try Unix.close fd with Unix.Unix_error _->()) in
 let cleanup ()=List.iter close !opened in
 Fun.protect ~finally:cleanup (fun ()->
  let info={info with SourcePreparation.stdin=stdinRead;stdout=stdoutWrite;stderr=stderrWrite} in
  (* Retry up to 3 times with small delay if we get "Text file busy". *)
  let rec start attempts=match SourcePreparation.tryStartProcess info with Error error when attempts>0 && HostText.contains error "Text file busy"->ignore (Unix.select [] [] [] 0.01);start (attempts-1)|result->result in
  match start 3 with Error error->Error error|Ok pid->
   close stdinRead;close stdoutWrite;close stderrWrite;
   let bytes=match input with O.Closed->Bytes.empty|O.Bytes bytes->bytes in
   let position=ref 0 in let writable=ref (Bytes.length bytes>0) in if not !writable then close stdinWrite;
   if !writable then Unix.set_nonblock stdinWrite;List.iter Unix.set_nonblock [stdoutRead;stderrRead];
   let readers=ref [stdoutRead;stderrRead] in let stdout=Buffer.create 4096 in let stderr=Buffer.create 4096 in let scratch=Bytes.create 16384 in
   (* Start async reads immediately to avoid blocking. *)
   let rec pump ()=if !readers<>[] || !writable then (
    let readyRead,readyWrite,_=try Unix.select !readers (if !writable then [stdinWrite] else []) [] (-1.) with Unix.Unix_error (Unix.EINTR,_,_)->[],[],[] in
    List.iter (fun fd->try let count=Unix.read fd scratch 0 (Bytes.length scratch) in if count=0 then (close fd;readers:=List.filter ((<>) fd) !readers) else Buffer.add_subbytes (if fd=stdoutRead then stdout else stderr) scratch 0 count with Unix.Unix_error ((Unix.EAGAIN|Unix.EWOULDBLOCK|Unix.EINTR),_,_)->()) readyRead;
    List.iter (fun fd->try let count=Unix.write fd bytes !position (Bytes.length bytes- !position) in position:= !position+count;if !position=Bytes.length bytes then (close fd;writable:=false) with Unix.Unix_error ((Unix.EAGAIN|Unix.EWOULDBLOCK|Unix.EINTR),_,_)->()) readyWrite;
    pump ()) in
   (* Wait for process to complete; then consume both fully read output streams. *)
   pump ();let code=wait pid in Ok (code,streamText (Buffer.contents stdout),streamText (Buffer.contents stderr)))
let writeTemp binary=
 let path=Filename.concat (Filename.get_temp_dir_name ()) (HostGuid.newGuidN ()) in
 (* Write and flush to disk to minimize (but not eliminate) "Text file busy" race. *)
 (* FileStream does not inherit its writer into concurrently spawned children. *)
 let fd=Unix.openfile path [Unix.O_WRONLY;Unix.O_CLOEXEC;Unix.O_CREAT;Unix.O_TRUNC] 0o666 in
 Fun.protect ~finally:(fun ()->Unix.close fd) (fun ()->let rec write offset=if offset<Bytes.length binary then let count=Unix.write fd binary offset (Bytes.length binary-offset) in write (offset+count) in write 0;Unix.fsync fd);
 path
let permissions path=let stat=Unix.stat path in Unix.chmod path (stat.Unix.st_perm lor 0o100)
let info path arguments environment={SourcePreparation.fileName=path;arguments;environment;stdin=Unix.stdin;stdout=Unix.stdout;stderr=Unix.stderr}
let codeSign target verbosity start path=
 (* Code sign with adhoc signature (required for macOS only). *)
 if Platform.requiresCodeSigning (Platform.osFor target) then (
  if verbosity>=1 then Output.println "    • Code signing (adhoc)...";
  let signStart=elapsed start in
  match captured (info "codesign" ["-s";"-";path] []) O.Closed with
  |Error error->Crash.crash error
  |Ok (code,_,stderr)->if code<>0 then Some ("Code signing failed: "^stderr) else (detail verbosity (elapsed start-.signStart);None))
 else (if verbosity>=1 then Output.println "    • Code signing skipped (not required on Linux)";None)
let beginExecution verbosity=if verbosity>=1 then (Output.println "";Output.println "  Execution:";Output.println "    • Writing binary to temp file...")
let finished verbosity start=if verbosity>=1 then (
 let duration=elapsed start in let scaled=duration*.10. in let lower=Float.floor scaled in let rounded=if scaled-.lower=0.5 then (if Float.rem lower 2.=0. then lower else lower+.1.) else Float.round scaled in
 Output.println ("  ✓ Execution complete ("^HostFloat.roundTrip (rounded/.10.)^"ms)"))
(* Execute a compiled binary with positional arguments and finite stdin while
   capturing both output streams. *)
(*
   Flush both stream and OS buffers to disk
   Code signing or platform detection failed - return error
   Wait 10ms before retry
   Now wait for output to be fully read
   Cleanup - ignore deletion errors
*)
let executeCapturedWithArgumentsAndEnvironment target verbosity arguments environment input binary=
 let start=HostClock.milliseconds () in
 let finish code stdout stderr={O.exitCode=code;stdout;stderr;runtimeTime=HostTimeSpan.fromMilliseconds (elapsed start)} in
 beginExecution verbosity;
 (* Write binary to temp file. *)
 let path=writeTemp binary in let writeTime=elapsed start in detail verbosity writeTime;
 Fun.protect ~finally:(fun ()->SourcePreparation.tryDeleteFile path) (fun ()->
  (* Make executable using Unix file mode. *)
  if verbosity>=1 then Output.println "    • Setting executable permissions...";permissions path;detail verbosity (elapsed start-.writeTime);
  match codeSign target verbosity start path with
  |Some error->finish (-1) "" error
  |None->
   (* Execute (with retry for "Text file busy" race condition).
      Even with flush, kernel may not have fully synced file/permissions in fast test runs. *)
   if verbosity>=1 then Output.println "    • Running binary...";let execStart=elapsed start in
   match captured (info path arguments environment) input with
   |Error error->finish (-1) "" ("Failed to start process: "^error)
   |Ok (code,stdout,stderr)->detail verbosity (elapsed start-.execStart);finished verbosity start;finish code stdout stderr)
let executeCapturedWithArguments target verbosity arguments input binary=executeCapturedWithArgumentsAndEnvironment target verbosity arguments [] input binary
(* Execute a compiled binary with finite stdin while capturing both output streams. *)
let executeCaptured target verbosity input binary=executeCapturedWithArguments target verbosity [] input binary
(* Backward-compatible captured execution with an already-closed stdin stream. *)
let execute target verbosity binary=executeCaptured target verbosity O.Closed binary
(* Execute a compiled binary with stdin/stdout/stderr inherited from this process.
   This is the interactive run path: presentation bytes are visible immediately
   and the OS remains responsible for terminal and signal behavior. *)
let executeAttached target verbosity binary=
 let start=HostClock.milliseconds () in let finish code stderr={O.exitCode=code;stdout="";stderr;runtimeTime=HostTimeSpan.fromMilliseconds (elapsed start)} in
 beginExecution verbosity;let path=writeTemp binary in let writeTime=elapsed start in detail verbosity writeTime;
 Fun.protect ~finally:(fun ()->SourcePreparation.tryDeleteFile path) (fun ()->
  if verbosity>=1 then Output.println "    • Setting executable permissions...";permissions path;detail verbosity (elapsed start-.writeTime);
  match codeSign target verbosity start path with Some error->finish (-1) error|None->
   if verbosity>=1 then Output.println "    • Running binary...";let execStart=elapsed start in
   let rec retry attempts=match SourcePreparation.tryStartProcess (info path [] []) with Error error when attempts>0 && HostText.contains error "Text file busy"->ignore (Unix.select [] [] [] 0.01);retry (attempts-1)|result->result in
   match retry 3 with Error error->finish (-1) ("Failed to start process: "^error)|Ok pid->let code=wait pid in detail verbosity (elapsed start-.execStart);finished verbosity start;finish code "")
