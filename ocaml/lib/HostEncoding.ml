(* Match replacement fallback at .NET's UTF-8 host text boundaries. *)
let utf8 text=
 let length=String.length text in let output=Buffer.create length in
 let byte index=Char.code text.[index] in
 let continuation value=value>=0x80 && value<=0xbf in
 let replace ()=Buffer.add_string output "\239\191\189" in
 let rec loop index=if index<length then (
  let first=byte index in
  if first<0x80 then (Buffer.add_char output text.[index];loop (index+1)) else
  let width,minimum,maximum=
   if first>=0xc2 && first<=0xdf then 2,0x80,0xbf
   else if first=0xe0 then 3,0xa0,0xbf
   else if first>=0xe1 && first<=0xec || first>=0xee && first<=0xef then 3,0x80,0xbf
   else if first=0xed then 3,0x80,0x9f
   else if first=0xf0 then 4,0x90,0xbf
   else if first>=0xf1 && first<=0xf3 then 4,0x80,0xbf
   else if first=0xf4 then 4,0x80,0x8f
   else 0,0,0 in
  if width=0 || index+1>=length || byte (index+1)<minimum || byte (index+1)>maximum then (replace ();loop (index+1)) else
  let rec prefix consumed=if consumed=width then consumed else if index+consumed<length && continuation (byte (index+consumed)) then prefix (consumed+1) else consumed in
  let consumed=prefix 2 in
  if consumed=width then Buffer.add_substring output text index width else replace ();
  loop (index+consumed)) in
 loop 0;Buffer.contents output
