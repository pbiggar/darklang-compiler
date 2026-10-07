(* FloatFormat.ml - Shortest binary64 digits with valid source-literal spelling. *)
let roundTrip value =
 match Float.classify_float value with
 | FP_zero -> if Int64.bits_of_float value < 0L then "-0" else "0"
 | FP_nan -> "NaN"
 | FP_infinite -> if value < 0. then "-Infinity" else "Infinity"
 | FP_normal | FP_subnormal ->
   let rec shortest precision =
    let text = Printf.sprintf "%.*g" precision value in
    if precision = 17 || Int64.bits_of_float (float_of_string text) = Int64.bits_of_float value then (
     match String.index_opt text 'e' with
     | None -> text
     | Some index ->
      let exponent = int_of_string (String.sub text (index + 1) (String.length text - index - 1)) in
      if exponent < -20 || exponent > 20 then text else
      let fixed = Printf.sprintf "%.*f" (max 0 (precision - 1 - exponent)) value in
      if String.length fixed <= String.length text then fixed else text)
    else shortest (precision + 1) in
   shortest 1
let structural value =
 match Float.classify_float value with
 | FP_nan -> "nan"
 | FP_infinite -> if value < 0. then "-infinity" else "infinity"
 | FP_zero | FP_normal | FP_subnormal ->
   let text = roundTrip value in
   if String.exists (function '.' | 'e' | 'E' -> true | _ -> false) text then text else text ^ ".0"
