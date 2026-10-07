(* HostText.ml - UTF-8 text, Unicode scalar properties and standard segmentation. *)
let fold f initial text =
  Uutf.String.fold_utf_8 (fun state _ -> function
    | `Uchar character -> f state character
    | `Malformed _ -> invalid_arg "Malformed UTF-8 host text") initial text
let scalars text =
  fold (fun reversed character -> Uchar.to_int character :: reversed) [] text
  |> List.rev |> Array.of_list
let ofScalars characters =
  let buffer = Buffer.create (Array.length characters) in
  Array.iter (fun scalar -> Uutf.Buffer.add_utf_8 buffer (Uchar.of_int scalar)) characters;
  Buffer.contents buffer
let length text = fold (fun count _ -> count + 1) 0 text
let first text = if text = "" then None else
  match String.get_utf_8_uchar text 0 with
  | decoded when Uchar.utf_decode_is_valid decoded -> Some (Uchar.utf_decode_uchar decoded)
  | _ -> invalid_arg "Malformed UTF-8 host text"
let contains text pattern =
  let length = String.length pattern in
  let rec scan index = index + length <= String.length text &&
    (String.sub text index length = pattern || scan (index + 1)) in
  scan 0
let trim text =
  let first = ref (String.length text) and last = ref 0 in
  Uutf.String.fold_utf_8 (fun () offset -> function
    | `Malformed _ -> invalid_arg "Malformed UTF-8 host text"
    | `Uchar character ->
        if not (Uucp.White.is_white_space character) then begin
          first := min !first offset;
          last := offset + Uchar.utf_8_byte_length character
        end) () text;
  if !last = 0 then "" else String.sub text !first (!last - !first)
let mapCase mapping text =
  let buffer = Buffer.create (String.length text) in
  fold (fun () character -> match mapping character with
    | `Self -> Uutf.Buffer.add_utf_8 buffer character
    | `Uchars mapped -> List.iter (Uutf.Buffer.add_utf_8 buffer) mapped) () text;
  Buffer.contents buffer
let lowerInvariant = mapCase Uucp.Case.Map.to_lower
let caseFold = mapCase Uucp.Case.Fold.fold
let normalize text =
  fold (fun () _ -> ()) () text;
  Uunf_string.normalize_utf_8 `NFC text
let property predicate scalar = Uchar.is_valid scalar && predicate (Uchar.of_int scalar)
let isLetter = property (fun character -> match Uucp.Gc.general_category character with
  | `Lu | `Ll | `Lt | `Lm | `Lo -> true | _ -> false)
let isDigit = property (fun character -> Uucp.Gc.general_category character = `Nd)
let isUpper = property (fun character -> Uucp.Gc.general_category character = `Lu)
let graphemeClusters text =
  fold (fun () _ -> ()) () text;
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun reversed cluster -> cluster :: reversed) [] text |> List.rev
let firstGrapheme text =
  let exception First of string in
  try ignore (Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun () cluster -> raise (First cluster)) () text); None
  with First cluster -> Some cluster
let startsWith text prefix = String.starts_with ~prefix text
let endsWith text suffix = String.ends_with ~suffix text
let tryParseInt32 text =
  let whitespace = function ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true | _ -> false in
  let length = String.length text in
  let rec left index = if index < length && whitespace text.[index] then left (index + 1) else index in
  let rec right index = if index >= 0 && whitespace text.[index] then right (index - 1) else index in
  let start = left 0 and stop = right (length - 1) in
  let digits = if start <= stop && (text.[start] = '+' || text.[start] = '-') then start + 1 else start in
  let rec valid index = index > stop ||
    (text.[index] >= '0' && text.[index] <= '9' && valid (index + 1)) in
  if digits > stop || not (valid digits) then None
  else Int32.of_string_opt (String.sub text start (stop - start + 1))
