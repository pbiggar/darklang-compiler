(* Canonical absolute HTTP(S) addresses at the native CLI host boundary. *)
let absoluteHttp input=
 try
 let value=String.trim input in
 let split=String.index value ':' in let scheme=String.lowercase_ascii (String.sub value 0 split) in
 if scheme<>"http" && scheme<>"https" then None else
 let rest=String.sub value (split+1) (String.length value-split-1) in
 if not (String.starts_with ~prefix:"//" rest) then None else
 let authorityStart=split+3 in
 let finish=let rec loop index=if index=String.length value || String.contains "/?#" value.[index] then index else loop (index+1) in loop authorityStart in
 let authority=String.sub value authorityStart (finish-authorityStart) in
 let user,host=match String.rindex_opt authority '@' with None->"",authority|Some index->String.sub authority 0 (index+1),String.sub authority (index+1) (String.length authority-index-1) in
 if host="" || String.contains host '\\' then raise Not_found;
 let host=HostText.lowerInvariant host in
 let hostname,port=match String.rindex_opt host ':' with
 |Some index when not (String.starts_with ~prefix:"[" host) || (match String.rindex_opt host ']' with Some close->close<index|None->false)->String.sub host 0 index,Some (String.sub host (index+1) (String.length host-index-1))
 |_->host,None in
 let port=Option.bind port (fun value->if value="" then None else if String.for_all (fun c->c>='0' && c<='9') value then Some (int_of_string value) else raise Not_found) in
 let host=hostname^(match port with None->""|Some 80 when scheme="http"->""|Some 443 when scheme="https"->""|Some value->":"^string_of_int value) in
 let suffix=String.sub value finish (String.length value-finish) in
 let suffix=if suffix="" || suffix.[0]<>'/' then "/"^suffix else suffix in
 let escape text=
  let buffer=Buffer.create (String.length text) in
  let hex c=match c with '0'..'9'->Some (Char.code c-48)|'a'..'f'->Some (Char.code c-87)|'A'..'F'->Some (Char.code c-55)|_->None in
  let unreserved c=(c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9') || String.contains "-._~" c in
  let rec loop index=if index<String.length text then
   let code=text.[index] in let n=Char.code code in
   if code='%' && index+2<String.length text then (match hex text.[index+1],hex text.[index+2] with
    |Some first,Some second->let decoded=Char.chr (first*16+second) in if unreserved decoded then Buffer.add_char buffer decoded else Buffer.add_substring buffer text index 3;loop (index+3)
    |_->Buffer.add_string buffer "%25";loop (index+1))
   else (if n<=32 || n>=127 || String.contains "\"<>`{}|^%" code then Buffer.add_string buffer (Printf.sprintf "%%%02X" n) else Buffer.add_char buffer code;loop (index+1)) in
  loop 0;Buffer.contents buffer in
 let prepared=scheme^"://"^escape user^host^escape suffix in
 let result=HostPackageIO.resolveUrl prepared prepared in
 Some result
 with Invalid_argument _|Failure _|Not_found->None
