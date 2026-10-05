(* Capture both streams without pipe deadlocks; terminate the child process group on timeout. *)
[@@@warning "-4-42"]
open Dark_compiler
external spawn : string * string array * string array * Unix.file_descr * Unix.file_descr * Unix.file_descr -> int*string = "dark_runner_spawn"
external signalNumber : int -> int = "dark_execution_signal_number"
exception TimedOut
let exitCode=function Unix.WEXITED code->code|Unix.WSIGNALED signal|Unix.WSTOPPED signal->128+signalNumber signal
let decode text=HostPackageIO.decodeContent (if String.starts_with ~prefix:"\000\000\254\255" text then Some "text/plain; charset=utf-32be" else None) text
let close fd=try Unix.close fd with Unix.Unix_error _->()
let kill pid=try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _->()
let rec wait pid=try snd (Unix.waitpid [] pid) with Unix.Unix_error (Unix.EINTR,_,_)->wait pid
let capture file arguments timeout=
 let stdoutRead,stdoutWrite=Unix.pipe ~cloexec:true () in let stderrRead,stderrWrite=Unix.pipe ~cloexec:true () in
 let child=ref None in
 Fun.protect ~finally:(fun ()->List.iter close [stdoutRead;stdoutWrite;stderrRead;stderrWrite];Option.iter (fun pid->kill pid;ignore (wait pid)) !child) (fun ()->
 let pid,error=spawn (file,Array.of_list (file::arguments),Unix.environment (),Unix.stdin,stdoutWrite,stderrWrite) in
 if pid<0 then Error ("Execution failed: An error occurred trying to start process '"^file^"' with working directory '"^Sys.getcwd ()^"'. "^error) else (
 child:=Some pid;close stdoutWrite;close stderrWrite;List.iter Unix.set_nonblock [stdoutRead;stderrRead];
 let readers=ref [stdoutRead;stderrRead] in let stdout=Buffer.create 4096 and stderr=Buffer.create 4096 in let scratch=Bytes.create 16384 in
 let deadline=HostClock.milliseconds ()+.float_of_int timeout in
 let rec pump status=
  let status=match status with Some _->status|None->let waited,status=Unix.waitpid [Unix.WNOHANG] pid in if waited=0 then None else (child:=None;Some status) in
  match status,!readers with Some status,[]->Ok (exitCode status,decode (Buffer.contents stdout),decode (Buffer.contents stderr))|_->
  if status=None && HostClock.milliseconds ()>=deadline then raise TimedOut;
  let delay=if status=None then min 0.05 (max 0. ((deadline-.HostClock.milliseconds ())/.1000.)) else -1. in
  let ready,_,_=try Unix.select !readers [] [] delay with Unix.Unix_error (Unix.EINTR,_,_)->[],[],[] in
  List.iter (fun fd->try let count=Unix.read fd scratch 0 (Bytes.length scratch) in if count=0 then (close fd;readers:=List.filter ((<>) fd) !readers) else Buffer.add_subbytes (if fd=stdoutRead then stdout else stderr) scratch 0 count with Unix.Unix_error ((Unix.EINTR|Unix.EAGAIN|Unix.EWOULDBLOCK),_,_)->()) ready;
  pump status in
 try pump None with TimedOut->Error (Printf.sprintf "Execution timed out after %dms" timeout)))
(* E2E cross-target execution supplies a finite stdin stream and explicit
   environment overrides. Drain both outputs while delivering the input. *)
let captureWithInputAndEnvironment file arguments overrides input timeout=
 let stdinRead,stdinWrite=Unix.pipe ~cloexec:true () in
 let stdoutRead,stdoutWrite=Unix.pipe ~cloexec:true () in let stderrRead,stderrWrite=Unix.pipe ~cloexec:true () in
 let environment=Array.to_list (Unix.environment ()) |> List.filter_map (fun entry->match String.index_opt entry '=' with
  |None->None|Some i->Some (String.sub entry 0 i,String.sub entry (i+1) (String.length entry-i-1))) |> StringOrder.Map.of_list in
 let environment=List.fold_left (fun env (name,value)->StringOrder.Map.add name value env) environment overrides
  |> StringOrder.Map.bindings |> List.map (fun (name,value)->name^"="^value) |> Array.of_list in
 let child=ref None in
 Fun.protect ~finally:(fun ()->List.iter close [stdinRead;stdinWrite;stdoutRead;stdoutWrite;stderrRead;stderrWrite];Option.iter (fun pid->kill pid;ignore (wait pid)) !child) (fun ()->
 let pid,error=spawn (file,Array.of_list (file::arguments),environment,stdinRead,stdoutWrite,stderrWrite) in
 if pid<0 then Error ("Execution failed: An error occurred trying to start process '"^file^"' with working directory '"^Sys.getcwd ()^"'. "^error) else (
 child:=Some pid;close stdinRead;close stdoutWrite;close stderrWrite;
 List.iter Unix.set_nonblock [stdoutRead;stderrRead];Unix.set_nonblock stdinWrite;
 let position=ref 0 and writable=ref (Bytes.length input>0) in if not !writable then close stdinWrite;
 let readers=ref [stdoutRead;stderrRead] in let stdout=Buffer.create 4096 and stderr=Buffer.create 4096 in let scratch=Bytes.create 16384 in
 let deadline=HostClock.milliseconds ()+.float_of_int timeout in
 let rec pump status=
  let status=match status with Some _->status|None->let waited,status=Unix.waitpid [Unix.WNOHANG] pid in if waited=0 then None else (child:=None;Some status) in
  match status,!readers,!writable with Some status,[],false->Ok (exitCode status,decode (Buffer.contents stdout),decode (Buffer.contents stderr))|_->
  if status=None && HostClock.milliseconds ()>=deadline then raise TimedOut;
  let delay=if status=None then min 0.05 (max 0. ((deadline-.HostClock.milliseconds ())/.1000.)) else -1. in
  let ready,writing,_=try Unix.select !readers (if !writable then [stdinWrite] else []) [] delay with Unix.Unix_error (Unix.EINTR,_,_)->[],[],[] in
  List.iter (fun fd->try let count=Unix.read fd scratch 0 (Bytes.length scratch) in if count=0 then (close fd;readers:=List.filter ((<>) fd) !readers) else Buffer.add_subbytes (if fd=stdoutRead then stdout else stderr) scratch 0 count with Unix.Unix_error ((Unix.EINTR|Unix.EAGAIN|Unix.EWOULDBLOCK),_,_)->()) ready;
  List.iter (fun fd->try let count=Unix.write fd input !position (Bytes.length input- !position) in position:= !position+count;if !position=Bytes.length input then (writable:=false;close fd)
    with Unix.Unix_error ((Unix.EINTR|Unix.EAGAIN|Unix.EWOULDBLOCK),_,_)->()) writing;
  pump status in
 try pump None with TimedOut->Error (Printf.sprintf "Execution timed out after %dms" timeout)))
