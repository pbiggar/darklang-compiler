(* HostText.ml - Retain Unicode and UTF-16 semantics at host text boundaries. *)
let foldUtf8 f initial text =
  Uutf.String.fold_utf_8
    (fun state _ -> function
      | `Uchar character -> f state character
      | `Malformed _ -> invalid_arg "Malformed UTF-8 host text") initial text
let lowerInvariant text =
  let buffer = Buffer.create (String.length text) in
  foldUtf8
    (fun () character ->
      (* The frozen Linux oracle uses ICU 74 / Unicode 15.1. Newly assigned
         Unicode 16/17 letters must retain their oracle casing behavior. *)
      let mapping = match Uucp.Age.age character with
        | `Version (major, minor) when (major, minor) <= (15, 1) -> Uucp.Case.Map.to_lower character
        | _ -> `Self
      in
      let lower = match mapping with
        | `Uchars [lower] -> lower
        | `Self | `Uchars _ -> character
      in
      Uutf.Buffer.add_utf_8 buffer lower)
    () text;
  Buffer.contents buffer
let trim text =
  let first = ref (String.length text) in
  let last = ref 0 in
  Uutf.String.fold_utf_8
    (fun () offset -> function
      | `Uchar character when not (Uucp.White.is_white_space character) ->
          first := min !first offset;
          last := offset + Uchar.utf_8_byte_length character
      | `Uchar _ -> ()
      | `Malformed _ -> invalid_arg "Malformed UTF-8 host text") () text;
  if !last = 0 then "" else String.sub text !first (!last - !first)
let contains text pattern =
  let length = String.length pattern in
  let rec scan index =
    index + length <= String.length text &&
    (String.sub text index length = pattern || scan (index + 1))
  in scan 0
let tryParseInt32 text =
  (* Integer parsing uses only the .NET NumberStyles.Integer whitespace set. *)
  let whitespace = function ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true | _ -> false in
  let rec withoutNuls length =
    if length > 0 && text.[length - 1] = '\000' then withoutNuls (length - 1) else length in
  let length = withoutNuls (String.length text) in
  let rec left index = if index < length && whitespace text.[index] then left (index + 1) else index in
  let rec right index = if index >= 0 && whitespace text.[index] then right (index - 1) else index in
  let start = left 0 and stop = right (length - 1) in
  let digits = if start <= stop && (text.[start] = '+' || text.[start] = '-') then start + 1 else start in
  let rec valid index = index > stop ||
    (text.[index] >= '0' && text.[index] <= '9' && valid (index + 1)) in
  if digits > stop || not (valid digits) then None
  else Int32.of_string_opt (String.sub text start (stop - start + 1))
let utf16Units text =
  foldUtf8 (fun units character ->
    let value = Uchar.to_int character in
    if value < 0x10000 then value :: units
    else let value = value - 0x10000 in
      (0xdc00 lor (value land 0x3ff)) :: (0xd800 lor (value lsr 10)) :: units)
    [] text |> List.rev |> Array.of_list
let ofUtf16Units units =
  let buffer = Buffer.create (Array.length units) in
  let rec append index =
    if index < Array.length units then begin
      let value = units.(index) in
      if value >= 0xd800 && value <= 0xdbff && index + 1 < Array.length units &&
         units.(index + 1) >= 0xdc00 && units.(index + 1) <= 0xdfff then begin
        let scalar = 0x10000 + ((value - 0xd800) lsl 10) + units.(index + 1) - 0xdc00 in
        Uutf.Buffer.add_utf_8 buffer (Uchar.of_int scalar);
        append (index + 2)
      end else begin
        if value >= 0xd800 && value <= 0xdfff then invalid_arg "Unpaired UTF-16 surrogate";
        Uutf.Buffer.add_utf_8 buffer (Uchar.of_int value);
        append (index + 1)
      end
    end
  in append 0; Buffer.contents buffer
let normalize text = Uunf_string.normalize_utf_8 `NFC text
