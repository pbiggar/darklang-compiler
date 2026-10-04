(* Bounded JSON documents retaining original element text for diagnostics. *)
[@@@warning "-4"]
type kind=Object|Array|String|Number|Boolean|Null
type t={raw:string;value:value}
and value=ObjectValue of (string*t) list|ArrayValue of t list|StringValue of string|NumberValue of string|BooleanValue of bool|NullValue
let rawText element=element.raw
let kind element=match element.value with ObjectValue _->Object|ArrayValue _->Array|StringValue _->String|NumberValue _->Number|BooleanValue _->Boolean|NullValue->Null
let fields element=match element.value with ObjectValue fields->fields|_->[]
let items element=match element.value with ArrayValue items->items|_->[]
let kindName=function Object->"Object"|Array->"Array"|String->"String"|Number->"Number"|Boolean->"Boolean"|Null->"Null"
let require expected element=invalid_arg ("The requested operation requires an element of type '"^kindName expected^"', but the target element has type '"^kindName (kind element)^"'.")
let string element=match element.value with StringValue text->text|_->require String element
let boolean element=match element.value with BooleanValue value->value|_->require Boolean element
let tryInt32 element=match element.value with
 |NumberValue text when not (String.exists (function '.'|'e'|'E'->true|_->false) text)->Option.map Int32.to_int (Int32.of_string_opt text)
 |NumberValue _->None|_->require Number element
let parse ?(maxDepth=512) source=
 (* Validate with Yojson, preserving literal spelling. A preceding bounded scan
    keeps the library parser from descending into unbounded response payloads. *)
 let depth=ref 0 and quoted=ref false and escaped=ref false in
 String.iter (fun c->if !quoted then (if !escaped then escaped:=false else if c='\\' then escaped:=true else if c='"' then quoted:=false) else match c with '"'->quoted:=true|'['|'{'->incr depth;if !depth>maxDepth then failwith ("The maximum configured depth of "^string_of_int maxDepth^" has been exceeded.")|']'|'}'->decr depth|_->()) source;
 let parsed=Yojson.Raw.from_string source in
 let cursor=ref 0 in
 let length=String.length source in
 let space ()=while !cursor<length && List.mem source.[!cursor] [' ';'\t';'\r';'\n'] do incr cursor done in
 let skipString ()=incr cursor;let escaped=ref false and ended=ref false in while !cursor<length && not !ended do let c=source.[!cursor] in incr cursor;if !escaped then escaped:=false else if c='\\' then escaped:=true else if c='"' then ended:=true done in
 let consume c=space ();if !cursor>=length || source.[!cursor]<>c then failwith "Invalid standard JSON" else incr cursor in
 let rec annotate parsed=
  space ();let start= !cursor in
  let value=match parsed with
   |`Assoc fields->consume '{';let fields=List.mapi (fun index (name,value)->if index>0 then consume ',';space ();skipString ();consume ':';name,annotate value) fields in consume '}';ObjectValue fields
   |`List values->consume '[';let values=List.mapi (fun index value->if index>0 then consume ',';annotate value) values in consume ']';ArrayValue values
   |`Stringlit text->skipString ();let text=match Yojson.Basic.from_string text with `String text->text|_->assert false in StringValue text
   |`Intlit text|`Floatlit text->if text="NaN" || text="Infinity" || text="-Infinity" then failwith "Invalid standard JSON";cursor:= !cursor+String.length text;NumberValue text
   |`Bool value->cursor:= !cursor+(if value then 4 else 5);BooleanValue value
   |`Null->cursor:= !cursor+4;NullValue in
  {raw=String.sub source start (!cursor-start);value}
 in let document=annotate parsed in space ();if !cursor<>length then failwith "Invalid standard JSON";document
