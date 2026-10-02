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
      let normalized = try `String (HostText.normalize text) with Invalid_argument _ -> `Null in
      if lower <> text || whitespace || normalized <> `String text then
        output (`List [`Int scalar; `String lower; `Bool whitespace; normalized]);
      let context = "a" ^ text ^ "a" in
      let clusters = HostText.graphemeClusters context in
      if clusters <> ["a"; text; "a"] then
        output (`List [`String "scalarGrapheme"; `Int scalar; `List (List.map (fun text -> `String text) clusters)]);
      if HostText.ofUtf16Units (HostText.utf16Units text) <> text then
        failwith "UTF-16 roundtrip mismatch"
    end
  done;
  let scalars = [0x61; 0xd; 0xa; 0; 0x301; 0x600; 0x903; 0x1100; 0x1160; 0x11a8;
    0xac00; 0xac01; 0x200d; 0xfe0f; 0x1f1e6; 0x1f1e7; 0x1f3fb; 0x1f469; 0x1f468;
    0x915; 0x94d; 0x937; 0x11f02; 0x11f36; 0x113b8; 0x113d0] in
  let scalarText value =
    let buffer = Buffer.create 4 in Uutf.Buffer.add_utf_8 buffer (Uchar.of_int value); Buffer.contents buffer in
  List.iter (fun left -> List.iter (fun right ->
    let text = scalarText left ^ scalarText right ^ scalarText left in
    let segments = HostText.graphemeClusters text in
    output (`List [`String "graphemes"; `String text; `List (List.map (fun segment -> `String segment) segments)])) scalars) scalars;
  for unit = 0 to 0xffff do
    let letter = HostText.isLetterUnit unit and digit = HostText.isDigitUnit unit in
    if letter || digit then output (`List [`Int unit; `Bool letter; `Bool digit; `Bool (HostText.isUpperUnit unit)])
  done;
  List.iter (fun text ->
    output (`List [`String text; match HostText.tryParseInt32 text with
      | None -> `Null | Some value -> `String (Int32.to_string value)]))
    [""; "0"; "+1"; "-1"; " 1\t"; "1_0"; "0x10"; "2147483647";
     "2147483648"; "-2147483648"; "-2147483649"; "\194\1601";
     "1\000"; "1\000\000"; "1 \000"; "1\000 "; "\0001";
     "000000000000000000000000001"; "\0111\012"]
