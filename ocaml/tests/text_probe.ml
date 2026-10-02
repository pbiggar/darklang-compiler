(* text_probe.ml - Observe every Unicode scalar without truncating comparisons. *)
open Dark_compiler
let output json = print_endline (Yojson.Basic.to_string json)
let () =
  for scalar = 0 to 0x10ffff do
    if scalar < 0xd800 || scalar > 0xdfff then begin
      let buffer = Buffer.create 4 in
      Uutf.Buffer.add_utf_8 buffer (Uchar.of_int scalar);
      let text = Buffer.contents buffer in
      let lower = HostText.lowerInvariant text in
      let whitespace = HostText.trim text = "" in
      if lower <> text || whitespace then
        output (`List [`Int scalar; `String lower; `Bool whitespace]);
      if HostText.ofUtf16Units (HostText.utf16Units text) <> text then
        failwith "UTF-16 roundtrip mismatch"
    end
  done;
  List.iter (fun text ->
    output (`List [`String text; match HostText.tryParseInt32 text with
      | None -> `Null | Some value -> `String (Int32.to_string value)]))
    [""; "0"; "+1"; "-1"; " 1\t"; "1_0"; "0x10"; "2147483647";
     "2147483648"; "-2147483648"; "-2147483649"; "\194\1601";
     "1\000"; "1\000\000"; "1 \000"; "1\000 "; "\0001";
     "000000000000000000000000001"; "\0111\012"]
