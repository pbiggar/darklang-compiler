(* host_text_regression.ml - Protect native string ordering and lookup allocation. *)
open Dark_compiler

let sign value = Int.compare value 0
let require condition message = if not condition then failwith message

let () =
  let strings =
    List.map
      (fun scalar -> Text.ofScalars [| scalar |])
      [
        0;
        65;
        0x7f;
        0x80;
        0x7ff;
        0x800;
        0xd7ff;
        0xe000;
        0xffff;
        0x10000;
        0x10ffff;
      ]
    @ [ ""; "a\000" ]
  in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          require
            (sign (String.compare a b) = sign (StringOrder.compare a b))
            "Native string ordering differs")
        strings)
    strings;
  (* All valid scalar widths, including their ordering against
     the BMP/supplementary boundary where native byte ordering differs from UTF-16. *)
  for scalar = 0 to 0x10ffff do
    if Uchar.is_valid scalar then begin
      let text = Text.ofScalars [| scalar |] in
      List.iter
        (fun pivot ->
          require
            (sign (String.compare text pivot)
            = sign (StringOrder.compare text pivot))
            "Scalar order differs")
        [ ""; "a"; "\238\128\128"; "\240\144\128\128" ]
    end
  done;
  Gc.full_major ();
  let before = (Gc.quick_stat ()).Gc.minor_words in
  for _ = 1 to 100000 do
    ignore
      (StringOrder.compare "Stdlib.Dict.__hamtLookup" "Stdlib.Dict.__hamtInsert")
  done;
  require
    ((Gc.quick_stat ()).Gc.minor_words -. before < 100.)
    "Identity comparison allocates per lookup";
  print_endline "Native string ordering and allocation regressions passed"
