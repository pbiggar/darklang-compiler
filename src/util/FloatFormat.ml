(* FloatFormat.ml - Shortest binary64 digits with valid source-literal spelling. *)
let roundTrip value =
 match Float.classify_float value with
 | FP_zero -> if Int64.bits_of_float value < 0L then "-0" else "0"
 | FP_nan -> "NaN"
 | FP_infinite -> if value < 0. then "-Infinity" else "Infinity"
 | FP_normal | FP_subnormal ->
   let text = Dtoa.shortest_string_of_float value in
   if String.starts_with ~prefix:"." text then "0" ^ text
   else if String.starts_with ~prefix:"-." text then "-0" ^ String.sub text 1 (String.length text - 1)
   else text
let structural value =
 match Float.classify_float value with
 | FP_nan -> "nan"
 | FP_infinite -> if value < 0. then "-infinity" else "infinity"
 | FP_zero | FP_normal | FP_subnormal ->
   let text = roundTrip value in
   if String.exists (function '.' | 'e' | 'E' -> true | _ -> false) text then text else text ^ ".0"
