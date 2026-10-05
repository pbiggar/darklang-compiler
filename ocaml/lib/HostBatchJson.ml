(* Decode labeled batch manifests with native JSON token locations. *)
type item={kind:string;name:string;source:string;output:string}
type expected=AnyValue|ManifestArray|ManifestItem|StringField
let[@warning "-4"] parse text=
 let length=String.length text in let cursor=ref 0 in
 let space ()=while !cursor<length && String.contains " \t\r\n" text.[!cursor] do incr cursor done in
 let location offset=let line=ref 0 and start=ref 0 in for index=0 to min length offset-1 do if text.[index]='\n' then (incr line;start:=index+1) done;!line,offset- !start in
 let error path offset message=let line,position=location offset in failwith (Printf.sprintf "%s Path: %s | LineNumber: %d | BytePositionInLine: %d." message path line position) in
 let invalid path offset c=error path offset ("'"^String.make 1 c^"' is an invalid start of a value.") in
 let depthMessage="Expected depth to be zero at the end of the JSON payload. There is an open JSON object or array that should be closed." in
 let eof path=error path length depthMessage in
 let conversion path stop target=error path stop ("The JSON value could not be converted to "^target^".") in
 let stringToken path decode=
  let start= !cursor in incr cursor;
  let escaped=ref false and finished=ref false in
  while !cursor<length && not !finished do
   let c=text.[!cursor] in
   if !escaped then (
    if not (String.contains "\"\\/bfnrtu" c) then error path !cursor ("'"^String.make 1 c^"' is an invalid escapable character within a JSON string. The string should be correctly escaped.");
    if c='u' then for index=1 to 4 do if !cursor+index>=length || not (String.contains "0123456789abcdefABCDEF" text.[!cursor+index]) then error path (min length (!cursor+index)) "Invalid hexadecimal escape sequence." done;
    escaped:=false) else if c='\\' then escaped:=true else if c='"' then finished:=true else if Char.code c<32 then error path !cursor (Printf.sprintf "'0x%02X' is invalid within a JSON string. The string should be correctly escaped." (Char.code c));
   incr cursor
  done;
  if not !finished then error path length "Expected end of string, but instead reached end of data.";
  let raw=String.sub text start (!cursor-start) in
  if not decode then "" else
  try let value=HostJson.string (HostJson.parse raw) in
  let units=HostText.utf16Units value in
  let rec paired index=if index=Array.length units then true else if units.(index)>=0xd800 && units.(index)<=0xdbff then index+1<Array.length units && units.(index+1)>=0xdc00 && units.(index+1)<=0xdfff && paired (index+2) else (units.(index)<0xdc00 || units.(index)>0xdfff) && paired (index+1) in
  if not (paired 0) then conversion path !cursor "System.String";value
  with Yojson.Json_error _|Failure _|Invalid_argument _->conversion path !cursor "System.String"
 in
 let rec value expected path depth=
  space ();if !cursor=length then eof path;
  if depth>=64 && String.contains "[{" text.[!cursor] then error path !cursor "The maximum configured depth of 64 has been exceeded. Cannot read next JSON object or array.";
  let start= !cursor in
  let target=match expected with AnyValue->None|ManifestArray->Some "Program+BatchManifestItem[]"|ManifestItem->Some "Program+BatchManifestItem"|StringField->Some "System.String" in
  let incompatible=match expected,text.[!cursor] with ManifestArray,'{'|ManifestItem,'['|StringField,('['|'{')->true|_->false in
  if incompatible then conversion path (start+1) ((match target with Some target -> target | None -> Crash.crash "JSON conversion has no expected target type"));
  let complete ((node,_,stop) as token)=
   let compatible=match expected,node with AnyValue,_|ManifestArray,(`Null|`Array _)|ManifestItem,(`Null|`Object _)|StringField,(`Null|`String _)->true|_->false in
   if compatible then token else conversion path stop ((match target with Some target -> target | None -> Crash.crash "JSON conversion has no expected target type")) in
  complete (match text.[!cursor] with
  |'"'->let text=stringToken path (expected=StringField) in `String text,start,!cursor
  |'{'->incr cursor;space ();let rec fields reversed=
   if !cursor=length then eof path;
   if text.[!cursor]='}' then (incr cursor;`Object (List.rev reversed),start,start+1) else (
    if text.[!cursor]<>'"' then error path !cursor ("'"^String.make 1 text.[!cursor]^"' is an invalid start of a property name. Expected a '\"'.");
    let name=stringToken path true in space ();
    if !cursor=length then eof path;
    if text.[!cursor]<>':' then error path !cursor ("'"^String.make 1 text.[!cursor]^"' is invalid after a property name. Expected a ':'.");
    incr cursor;let expected=if expected=ManifestItem && List.mem name ["kind";"name";"source";"output"] then StringField else AnyValue in
    let child=value expected (path^"."^name) (depth+1) in space ();
    if !cursor=length then eof (path^"."^name);
    match text.[!cursor] with
    |'}'->incr cursor;`Object (List.rev ((name,child)::reversed)),start,start+1
    |','->incr cursor;space ();if !cursor<length && text.[!cursor]='}' then error path !cursor "The JSON object contains a trailing comma at the end which is not supported in this mode. Change the reader options.";fields ((name,child)::reversed)
    |c->error path !cursor ("'"^String.make 1 c^"' is invalid after a value. Expected either ',', '}', or ']'.")) in fields []
  |'['->incr cursor;space ();let rec items index reversed=
   if !cursor=length then eof (path^"["^string_of_int index^"]");
   if text.[!cursor]=']' then (incr cursor;`Array (List.rev reversed),start,start+1) else (
    let item=value (if expected=ManifestArray then ManifestItem else AnyValue) (path^"["^string_of_int index^"]") (depth+1) in space ();
    if !cursor=length then eof (path^"["^string_of_int index^"]");
    match text.[!cursor] with
    |']'->incr cursor;`Array (List.rev (item::reversed)),start,start+1
    |','->let comma= !cursor in incr cursor;space ();if !cursor=length then error (path^"["^string_of_int (index+1)^"]") comma "Expected start of a property name or value, but instead reached end of data.";if !cursor<length && text.[!cursor]=']' then error (path^"["^string_of_int (index+1)^"]") !cursor "The JSON array contains a trailing comma at the end which is not supported in this mode. Change the reader options.";items (index+1) (item::reversed)
    |c->error (path^"["^string_of_int (index+1)^"]") !cursor ("'"^String.make 1 c^"' is invalid after a value. Expected either ',', '}', or ']'.")) in items 0 []
  |('t'|'f'|'n' as first)->let literal,node=if first='t' then "true",`Other else if first='f' then "false",`Other else "null",`Null in
   let rec read index=if index=String.length literal then () else if !cursor>=length then error path !cursor ("'"^String.sub text start (!cursor-start)^"' is an invalid JSON literal. Expected the literal '"^literal^"'.") else if text.[!cursor]<>literal.[index] then error path !cursor ("'"^String.sub text start (length-start)^"' is an invalid JSON literal. Expected the literal '"^literal^"'.") else (incr cursor;read (index+1)) in
   read 0;node,start,!cursor
  |('-'|'0'..'9')->
   let digit ()= !cursor<length && text.[!cursor]>='0' && text.[!cursor]<='9' in
   let required context=
    if !cursor=length then error path !cursor "Expected a digit ('0'-'9'), but instead reached end of data."
    else if not (digit ()) then error path !cursor ("'"^String.make 1 text.[!cursor]^"' is invalid within a number, immediately after "^context^". Expected a digit ('0'-'9').") in
   if text.[!cursor]='-' then (incr cursor;required "a sign character ('+' or '-')");
   if text.[!cursor]='0' then (incr cursor;if digit () then error path !cursor ("Invalid leading zero before '"^String.make 1 text.[!cursor]^"'."))
   else while digit () do incr cursor done;
   if !cursor<length && text.[!cursor]='.' then (incr cursor;required "a decimal point ('.')";while digit () do incr cursor done);
   if !cursor<length && String.contains "eE" text.[!cursor] then (incr cursor;if !cursor<length && String.contains "+-" text.[!cursor] then incr cursor;required "a sign character ('+' or '-')";while digit () do incr cursor done);
   `Other,start,!cursor
  |c->invalid path !cursor c)
 in
 space ();if !cursor=length then error "$" !cursor "The input does not contain any JSON tokens. Expected the input to start with a valid JSON token, when isFinalBlock is true.";
 let root,_,stop=value ManifestArray "$" 0 in space ();if !cursor<>length then error (match root with `Array values->"$["^string_of_int (List.length values)^"]"|_->"$") !cursor ("'"^String.make 1 text.[!cursor]^"' is invalid after a single JSON value. Expected end of data.");
 let conversion path stop target=error path stop ("The JSON value could not be converted to "^target^".") in
 match root with `Null->None|`String _|`Object _|`Other->conversion "$" stop "Program+BatchManifestItem[]"|`Array entries->Some (List.mapi (fun index (entry,_,stop)->let path="$["^string_of_int index^"]" in
 match entry with `Null->None|`String _|`Array _|`Other->conversion path stop "Program+BatchManifestItem"|`Object fields->
 let values=List.fold_left (fun values (name,(node,_,stop))->if not (List.mem name ["kind";"name";"source";"output"]) then values else
 let text=match node with `Null->""|`String value->value|`Array _|`Object _|`Other->conversion (path^"."^name) stop "System.String" in
 (name,text)::List.remove_assoc name values) [] fields in
 let field name=Option.value ~default:"" (List.assoc_opt name values) in
 let kind=field "kind" in let name=field "name" in let source=field "source" in let output=field "output" in Some {kind;name;source;output}) entries)
