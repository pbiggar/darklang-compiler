(* HostUtf16TextObservation.ml - Historical probe name; observe native Unicode text. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let observe source=
 let bucket=int_of_string source in
 let row scalar=let text=HostText.ofScalars [|scalar|] in
  J.tuple [J.string text;J.string (HostText.trim text);J.string (HostText.lowerInvariant text)] in
 let scalars=List.init 4096 (fun index->index*16+bucket) |> List.filter Uchar.is_valid in
 let supplementary=List.init 128 (fun index->0x10000+(index*16+bucket)*512) in
 J.tuple [`List (List.map row scalars);`List (List.map row supplementary)]
