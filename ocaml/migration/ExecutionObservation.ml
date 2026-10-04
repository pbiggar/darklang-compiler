(* Compare process arguments, environment, stdin, streams and host exit codes. *)
open Dark_compiler
module O=CompilerOptions
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let capture action=
 let path=Filename.temp_file "execution-log" ".txt" in let saved=Unix.dup Unix.stdout in
 Fun.protect ~finally:(fun ()->flush stdout;Unix.dup2 saved Unix.stdout;Unix.close saved;Sys.remove path) (fun ()->
 let fd=Unix.openfile path [Unix.O_WRONLY;Unix.O_TRUNC] 0 in flush stdout;Unix.dup2 fd Unix.stdout;Unix.close fd;
 let result=action () in flush stdout;let text=In_channel.with_open_bin path In_channel.input_all in
 let text=Str.global_replace (Str.regexp "[0-9]+\\(\\.[0-9]+\\)?ms") "<duration>ms" text in result,text)
let clean text=Str.global_replace (Str.regexp (String.concat "" (List.init 32 (fun _->"[0-9a-f]")))) "<temporary-id>" text
let observe source=
 let script text=Bytes.of_string ("#!/bin/sh\n"^text^"\n") in
 let fixtures=[script "exit 0";script "printf '%s\\n' \"$PORT_VALUE\"; printf '%s\\n' 'error' >&2; for arg do printf '<%s>\\n' \"$arg\"; done; exit 17";script "cat";script "head -c 131072 /dev/zero; head -c 131073 /dev/zero >&2";script "printf '\\357\\273\\277hello'; printf '\\377\\376h\\000i\\000' >&2";script "printf '\\000\\000\\376\\377\\000\\000\\000h\\000\\000\\000i'";script "printf '\\300\\257\\355\\240\\200\\342\\202'";script "kill -TERM $$";Bytes.empty;Bytes.of_string "invalid executable"] in
 let describe (out:O.executionOutput)=tuple [SemanticJson.int32 out.O.exitCode;str out.O.stdout;str (clean out.O.stderr);`Bool (out.O.runtimeTime>=0L)] in
 let inputs=[O.Closed;O.Bytes (Bytes.of_string (source^"\nhello\n"))] in
 let captured=list (fun binary->list (fun input->list (fun verbosity->let out,text=capture (fun ()->CompilerExecution.executeCapturedWithArgumentsAndEnvironment Platform.LinuxX86_64 verbosity [source;"two words";"";"hé😀"] ["PORT_VALUE",source] input binary) in tuple [describe out;str text]) [0;3]) (if binary=script "cat" then inputs else [O.Closed])) fixtures in
 let wrappers=list (fun call->let out,text=capture call in tuple [describe out;str text]) [(fun ()->CompilerExecution.execute Platform.LinuxX86_64 1 (script "exit 3"));(fun ()->CompilerExecution.executeCaptured Platform.LinuxX86_64 1 O.Closed (script "exit 5"));(fun ()->CompilerExecution.executeCapturedWithArguments Platform.LinuxX86_64 1 [] O.Closed (script "exit 7"));(fun ()->CompilerExecution.executeAttached Platform.LinuxX86_64 1 (script "exit 11"))] in
 let raw=Buffer.create 300000 in
 for first=0 to 255 do Buffer.add_char raw (Char.chr first);Buffer.add_char raw '|' done;
 for first=0 to 255 do for second=0 to 255 do Buffer.add_char raw (Char.chr first);Buffer.add_char raw (Char.chr second);Buffer.add_char raw '|' done done;
 let edges=[0;0x7f;0x80;0x8f;0x90;0x9f;0xa0;0xbf;0xc0;0xff] in
 List.iter (fun first->for second=0x80 to 0xbf do List.iter (fun third->List.iter (fun fourth->List.iter (fun code->Buffer.add_char raw (Char.chr code)) [first;second;third;fourth];Buffer.add_char raw '|') edges) edges done) [0xe0;0xed;0xf0;0xf4];
 tuple [captured;wrappers;str (HostEncoding.utf8 (Buffer.contents raw))]
