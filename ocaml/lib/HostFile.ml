(* Native file boundaries with frozen .NET path, decoding and I/O diagnostics. *)
let absolutePath path=
 let absolute=if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path in
 let rec collapse reversed=function []->List.rev reversed|(""|".")::rest->collapse reversed rest|".."::rest->collapse (match reversed with []->[]|_::rest->rest) rest|part::rest->collapse (part::reversed) rest in
 let normalized="/"^String.concat "/" (collapse [] (String.split_on_char '/' absolute)) in
 if normalized<>"/" && String.ends_with ~suffix:"/" absolute then normalized^"/" else normalized
let validation path=if path="" then Some "The value cannot be an empty string. (Parameter 'path')" else if String.contains path '\000' then Some "Null character in path. (Parameter 'path')" else None
let[@warning "-4"] errorMessage path exn=match validation path with Some message->message|None->
 let absolute=absolutePath path in
 let error=match exn with Unix.Unix_error (error,_,_)->Some error|Sys_error message->List.find_opt (fun error->String.ends_with ~suffix:(Unix.error_message error) message) [Unix.ENOENT;Unix.ENOTDIR;Unix.EACCES;Unix.EPERM;Unix.EISDIR;Unix.ENAMETOOLONG;Unix.ENOSPC;Unix.EROFS;Unix.ELOOP;Unix.EMFILE;Unix.ENFILE;Unix.EIO]|_->None in
 match error with
 |Some (Unix.ENOENT|Unix.ENOTDIR)->"Could not find a part of the path '"^absolute^"'."
 |Some (Unix.EACCES|Unix.EPERM|Unix.EISDIR)->"Access to the path '"^absolute^"' is denied."
 |Some Unix.ENAMETOOLONG->"The path '"^absolute^"' is too long, or a component of the specified path is too long."
 |Some error->Unix.error_message error^" : '"^absolute^"'"
 |None->(match exn with Sys_error message|Failure message|Invalid_argument message->message|_->Printexc.to_string exn)
let exists path=try (Unix.stat path).Unix.st_kind<>Unix.S_DIR with Unix.Unix_error _->false
let readText path=In_channel.with_open_bin path (fun channel->let bytes=In_channel.input_all channel in
 let encoding=if String.starts_with ~prefix:"\000\000\254\255" bytes then Some "application/octet-stream; charset=utf-32be" else None in
 HostPackageIO.decodeContent encoding bytes)
let writeBytes path bytes=match validation path with Some message->Error message|None->
 try Out_channel.with_open_bin path (fun channel->Out_channel.output_bytes channel bytes);Ok () with exn->Error (errorMessage path exn)
