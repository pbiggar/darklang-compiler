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
  (* UTF-8 at file boundaries; WTF-8 internally retains isolated UTF-16 units
     in diagnostic strings rather than silently replacing source evidence. *)
  let rec decode offset reversed =
    if offset >= String.length text then List.rev reversed |> Array.of_list else
    let byte index = Char.code text.[index] in
    let first = byte offset in
    let count, mask = if first < 0x80 then 1, 0x7f
      else if first land 0xe0 = 0xc0 then 2, 0x1f
      else if first land 0xf0 = 0xe0 then 3, 0x0f
      else if first land 0xf8 = 0xf0 then 4, 0x07
      else invalid_arg "Malformed UTF-8 host text" in
    if offset + count > String.length text then invalid_arg "Malformed UTF-8 host text";
    let value = ref (first land mask) in
    for index = 1 to count - 1 do
      let next = byte (offset + index) in
      if next land 0xc0 <> 0x80 then invalid_arg "Malformed UTF-8 host text";
      value := (!value lsl 6) lor (next land 0x3f)
    done;
    if !value > 0x10ffff || (count > 1 && !value < (match count with 2 -> 0x80 | 3 -> 0x800 | _ -> 0x10000)) then
      invalid_arg "Malformed UTF-8 host text";
    let reversed = if !value < 0x10000 then !value :: reversed
      else let value = !value - 0x10000 in
        (0xdc00 lor (value land 0x3ff)) :: (0xd800 lor (value lsr 10)) :: reversed in
    decode (offset + count) reversed
  in decode 0 []
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
        if value >= 0xd800 && value <= 0xdfff then begin
          Buffer.add_char buffer (Char.chr (0xe0 lor (value lsr 12)));
          Buffer.add_char buffer (Char.chr (0x80 lor ((value lsr 6) land 0x3f)));
          Buffer.add_char buffer (Char.chr (0x80 lor (value land 0x3f)))
        end else Uutf.Buffer.add_utf_8 buffer (Uchar.of_int value);
        append (index + 1)
      end
    end
  in append 0; Buffer.contents buffer
let normalize text =
  let units = utf16Units text in
  let rec validate index =
    if index < Array.length units then
      let value = units.(index) in
      if value = 0xfffe then invalid_arg "String contains invalid Unicode code points"
      else if value >= 0xd800 && value <= 0xdbff then
        if index + 1 < Array.length units && units.(index + 1) >= 0xdc00 && units.(index + 1) <= 0xdfff then validate (index + 2)
        else invalid_arg "String contains invalid Unicode code points"
      else if value >= 0xdc00 && value <= 0xdfff then invalid_arg "String contains invalid Unicode code points"
      else validate (index + 1)
  in validate 0; Uunf_string.normalize_utf_8 `NFC text
let isLetterUnit value =
  if not (Uchar.is_valid value) then false else
  let character = Uchar.of_int value in
  let category = match Uucp.Age.age character with
    | `Version version when version <= (16, 0) -> Uucp.Gc.general_category character
    | _ -> `Cn in
  match category with
  | `Lu | `Ll | `Lt | `Lm | `Lo -> true
  | _ -> false
let isDigitUnit value =
  if not (Uchar.is_valid value) then false else
  let character = Uchar.of_int value in
  match Uucp.Age.age character with
  | `Version version when version <= (16, 0) -> Uucp.Gc.general_category character = `Nd
  | _ -> false
let isUpperUnit value =
  if not (Uchar.is_valid value) then false else
  let character = Uchar.of_int value in
  match Uucp.Age.age character with
  | `Version version when version <= (16, 0) -> Uucp.Gc.general_category character = `Lu
  | _ -> false
let graphemeClusters text =
  (* StringInfo's frozen algorithm implements GB3..GB13 without the GB9c Indic
     conjunct rule added by newer uuseg releases. Keep those boundaries while
     obtaining character properties from Uucp, rather than copying its tables.
     Reference: dotnet/runtime System/Text/Unicode/TextSegmentationUtility.cs. *)
  let characters = Uutf.String.fold_utf_8
    (fun reversed offset -> function
      | `Uchar character -> (offset, character) :: reversed
      | `Malformed _ -> invalid_arg "Malformed UTF-8 host text") [] text |> List.rev in
  let property character = match Uucp.Age.age character with
    | `Version version when version > (16, 0) -> `XX
    | `Version _ | `Unassigned -> Uucp.Break.grapheme_cluster character in
  let control category = List.mem category [`CN; `CR; `LF] in
  let rec scan previous regionalCount emojiRun emojiZwj start reversed = function
    | [] -> List.rev (if start < String.length text then String.sub text start (String.length text - start) :: reversed else reversed)
    | (offset, character) :: rest ->
        let current = property character in
        let pictographic = Uucp.Emoji.is_extended_pictographic character in
        let boundary = match previous with
          | None -> false
          | Some previous ->
              if previous = `CR && current = `LF then false
              else if control previous || control current then true
              else if previous = `L && List.mem current [`L; `V; `LV; `LVT] then false
              else if List.mem previous [`LV; `V] && List.mem current [`V; `T] then false
              else if List.mem previous [`LVT; `T] && current = `T then false
              else if List.mem current [`EX; `ZWJ; `SM] || previous = `PP then false
              else if previous = `ZWJ && emojiZwj && pictographic then false
              else if previous = `RI && current = `RI && regionalCount mod 2 = 1 then false
              else true in
        let reversed, start = if boundary then String.sub text start (offset - start) :: reversed, offset else reversed, start in
        let nextEmojiZwj = current = `ZWJ && emojiRun in
        let nextEmojiRun = pictographic || (current = `EX && emojiRun) in
        let nextRegional = if current = `RI then regionalCount + 1 else 0 in
        scan (Some current) nextRegional nextEmojiRun nextEmojiZwj start reversed rest
  in scan None 0 false false 0 [] characters

external startsWithUnits : int array -> int array -> bool = "dark_starts_with_current_culture"
let startsWithCurrentCulture text prefix=startsWithUnits (utf16Units text) (utf16Units prefix)
