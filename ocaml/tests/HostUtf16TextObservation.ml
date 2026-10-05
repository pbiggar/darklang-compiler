(* Exhaustive BMP casing/whitespace and supplementary UTF-16 host strings. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let observe source=
 let bucket=int_of_string source in
 let row units=let text=HostText.ofUtf16Units (Array.of_list units) in J.tuple [J.string text;J.string (HostText.trim text);J.string (HostText.lowerInvariant text)] in
 let bmp=List.init 4096 (fun index->let unit=index*16+bucket in `List (List.map row [[unit];[9;32;unit;32;0xa0];[0xd800;unit;0xdc00]])) in
 let scalarUnits value=let offset=value-0x10000 in [0xd800+(offset lsr 10);0xdc00+(offset land 1023)] in
 let supplemental=List.init 128 (fun index->let value=0x10000+(index*16+bucket)*512 in row (scalarUnits value)) in
 let cases=[[0xd800;0xdc00];[0xd801;0xdc00];[0xdbff;0xdfff];[0xd800;65;0xdc00];[0xd800;0xd800;0xdc00];[0xdc00;0xd800;0xdc00];[0x0130;0x0131;0x0049;0x212a;0x1c89;0xa7cb];[0x03a3;0x03c2;0x0307];[0x00a0;0x1680;0x2000;0x2028;0x2029;0x202f;0x205f;0x3000];[]] in
 J.tuple [`List bmp;`List supplemental;`List (List.map row cases)]
