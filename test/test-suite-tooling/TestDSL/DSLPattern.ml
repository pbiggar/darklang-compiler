(* Match only the regex constructs used by fixture parsers; preserve .NET order and classes. *)
[@@@warning "-4"]
open Dark_compiler
type characterClass=Dot | Digit | Space | Characters of string | Except of string
type token=Literal of string | Repeat of characterClass * int * bool | Capture of token list | Alternatives of token list list
let literal text=Literal text
let space=Repeat (Space,0,true)
let spaces=Repeat (Space,1,true)
let digits=Repeat (Digit,1,true)
let any=Repeat (Dot,1,true)
let matched tokens source=
 let units=Text.scalars source in
 let length=Array.length units in
 let substring first ending=Text.ofScalars (Array.sub units first (ending-first)) in
 let classMatches kind value=match kind with
 |Dot->value<>10|Digit->Text.isDigit value
 |Space->Uchar.is_valid value && Uucp.White.is_white_space (Uchar.of_int value)
 |Characters chars->Array.exists ((=) value) (Text.scalars chars)
 |Except chars->not (Array.exists ((=) value) (Text.scalars chars)) in
 let rec run tokens at captures continuation=match tokens with
 |[]->continuation at captures
 |Literal text::rest->
  let expected=Text.scalars text in
  let count=Array.length expected in
  if at+count<=length && Array.for_all (fun i->units.(at+i)=expected.(i)) (Array.init count Fun.id) then run rest (at+count) captures continuation else None
 |Repeat (kind,minimum,greedy)::rest->
  let ending=ref at in while !ending<length && classMatches kind units.(!ending) do incr ending done;
  let rec attempt n=if n<at+minimum || n> !ending then None else match run rest n captures continuation with Some _ as found->found|None->attempt (n+if greedy then -1 else 1) in
  attempt (if greedy then !ending else at+minimum)
 |Capture inner::rest->run inner at captures (fun ending values->run rest ending (values@[substring at ending]) continuation)
 |Alternatives choices::rest->
  let rec choose=function []->None|choice::tail->match run (choice@rest) at captures continuation with Some _ as found->found|None->choose tail in choose choices in
 run tokens 0 [] (fun at captures->if at=length || at=length-1 && units.(at)=10 then Some (Array.of_list (substring 0 at::captures)) else None)
let integer width signed source=
 let isSpace=function ' '| '\t'|'\n'|'\r'|'\011'|'\012'->true|_->false in
 let rec withoutNuls n=if n>0 && source.[n-1]='\000' then withoutNuls (n-1) else n in
 let length=withoutNuls (String.length source) in
 let rec left n=if n<length && isSpace source.[n] then left (n+1) else n in
 let rec right n=if n>=0 && isSpace source.[n] then right (n-1) else n in
 let first=left 0 and last=right (length-1) in
 let start=if first<=last && (source.[first]='+' || source.[first]='-') then first+1 else first in
 let rec valid n=n>last || source.[n]>='0' && source.[n]<='9' && valid (n+1) in
 if start>last || not (valid start) then None else
 let number=Z.of_string_base 10 (String.sub source first (last-first+1)) in
 let bound=Z.shift_left Z.one (width-if signed then 1 else 0) in
 let lower=if signed then Z.neg bound else Z.zero in
 if Z.compare number lower<0 || Z.compare number bound>=0 then None else Some number
let int64 text=Option.map Z.to_int64 (integer 64 true text)
