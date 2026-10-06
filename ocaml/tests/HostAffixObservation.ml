(* Compare complete ICU prefix/suffix results over UTF-16 boundaries. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let observe source =
  let bucket=int_of_string source in
  let patterns=["";"\"";"\"\"\"";"(";")";"def ";"a";"\204\129"] in
  let row units=
    let s=HostText.ofScalars (Array.of_list units) in
    `List (List.map (fun p -> J.tuple [`Bool (HostText.startsWith s p);`Bool (HostText.endsWith s p)]) patterns) in
  let bmp=List.init 4096 (fun index -> let u=index*16+bucket in `List (List.map row [[u];[97;u;97];[0xd800;u;0xdc00]])) in
  let cases=[[];[34];[34;34];[34;34;34];[34;0x301];[0x301;34];[97;0x301];[0x301;97];[0;97;0];[0xd800;0xdc00]] in
  J.tuple [`List (List.map J.string patterns);`List bmp;`List (List.map row cases)]
