(* Utf8.ml - Decode host UTF-8 with the library's malformed-input replacement. *)
let utf8 text =
  if String.is_valid_utf_8 text then text else
  let output=Buffer.create (String.length text) in
  Uutf.String.fold_utf_8 (fun () _ decoded ->
    Uutf.Buffer.add_utf_8 output (match decoded with `Uchar character -> character | `Malformed _ -> Uchar.rep)) () text;
  Buffer.contents output
