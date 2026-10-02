(* float_probe.ml - Compare binary64 shortest representations, including extremes. *)
let () =
  let observe bits =
    let value = Int64.float_of_bits bits in
    print_endline (Yojson.Basic.to_string (`List [`String (Printf.sprintf "%016Lx" bits); `String (Dark_compiler.HostFloat.roundTrip value)])) in
  List.iter observe [0L; Int64.min_int; 1L; Int64.max_int; -1L; 0x7ff0000000000000L; 0xfff0000000000000L];
  List.iter (fun value -> observe (Int64.bits_of_float value)) [0.1; 1e-4; 1e-5; 1e16; 1e17; 1e18; 1.2345678901234567; 1e-300; 1e300];
  let bits = ref 0x9e3779b97f4a7c15L in
  for _ = 1 to 10000 do
    bits := Int64.logxor !bits (Int64.shift_left !bits 13);
    bits := Int64.logxor !bits (Int64.shift_right_logical !bits 7);
    bits := Int64.logxor !bits (Int64.shift_left !bits 17);
    observe !bits
  done
