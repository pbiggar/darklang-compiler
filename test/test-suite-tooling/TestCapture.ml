(* Capture native console output for runner assertions and migration observations. *)
let run action=
 let out=Filename.temp_file "runner-output" ".txt" in let err=Filename.temp_file "runner-error" ".txt" in
 let savedOut=Unix.dup ~cloexec:true Unix.stdout in let savedErr=Unix.dup ~cloexec:true Unix.stderr in
 Fun.protect ~finally:(fun ()->flush stdout;flush stderr;Unix.dup2 savedOut Unix.stdout;Unix.dup2 savedErr Unix.stderr;Unix.close savedOut;Unix.close savedErr;Sys.remove out;Sys.remove err) (fun ()->
 let attach path target=let fd=Unix.openfile path [Unix.O_WRONLY;Unix.O_CLOEXEC;Unix.O_TRUNC] 0 in Unix.dup2 fd target;Unix.close fd in
 flush stdout;flush stderr;attach out Unix.stdout;attach err Unix.stderr;
 let value=action () in flush stdout;flush stderr;
 value,In_channel.with_open_bin out In_channel.input_all,In_channel.with_open_bin err In_channel.input_all)
