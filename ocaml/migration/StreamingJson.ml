(* Emit complete migration observations without a buffer proportional to the wire row. *)
let rec to_channel channel (value : Yojson.Basic.t) = match value with
 | `Assoc fields ->
   output_char channel '{';
   List.iteri (fun index (name, value) ->
    if index <> 0 then output_char channel ',';
    output_string channel (Yojson.Basic.to_string (`String name));
    output_char channel ':';
    to_channel channel value) fields;
   output_char channel '}'
 | `List values ->
   output_char channel '[';
   List.iteri (fun index value -> if index <> 0 then output_char channel ','; to_channel channel value) values;
   output_char channel ']'
 | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as atom -> output_string channel (Yojson.Basic.to_string atom)
