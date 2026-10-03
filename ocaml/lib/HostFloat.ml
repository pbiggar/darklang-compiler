(* HostFloat.ml - Retain .NET R-format digits and fixed/scientific thresholds. *)
let roundTrip value =
  match Float.classify_float value with
  | FP_nan -> "NaN"
  | FP_infinite -> if value < 0. then "-Infinity" else "Infinity"
  | FP_zero -> if Int64.bits_of_float value < 0L then "-0" else "0"
  | FP_normal | FP_subnormal ->
      let negative = value < 0. in
      let magnitude = Float.abs value in
      let rec shortest precision =
        let candidate = Printf.sprintf "%.*g" precision magnitude in
        if precision = 17 || Int64.bits_of_float (float_of_string candidate) = Int64.bits_of_float magnitude then candidate
        else shortest (precision + 1) in
      let candidate = shortest 1 in
      let mantissa, exponent = match String.split_on_char 'e' candidate with
        | [mantissa; exponent] -> mantissa, int_of_string exponent
        | [mantissa] -> mantissa, 0
        | _ -> Crash.crash "Unexpected shortest float format" in
      let whole, fraction = match String.split_on_char '.' mantissa with
        | [whole; fraction] -> whole, fraction
        | [whole] -> whole, ""
        | _ -> Crash.crash "Unexpected shortest float mantissa" in
      let digits = whole ^ fraction in
      let rec first index = if index < String.length digits && digits.[index] = '0' then first (index + 1) else index in
      let first = first 0 in
      let exponent = exponent + String.length whole - first - 1 in
      let digits = String.sub digits first (String.length digits - first) in
      let rec last index = if index > 0 && digits.[index] = '0' then last (index - 1) else index in
      let digits = String.sub digits 0 (last (String.length digits - 1) + 1) in
      let result =
        if exponent < -4 || exponent >= 17 then
          let leading = String.sub digits 0 1 in
          let fraction = if String.length digits = 1 then "" else "." ^ String.sub digits 1 (String.length digits - 1) in
          leading ^ fraction ^ "E" ^ (if exponent < 0 then "-" else "+") ^ Printf.sprintf "%02d" (abs exponent)
        else
          let point = exponent + 1 in
          if point <= 0 then "0." ^ String.make (-point) '0' ^ digits
          else if point >= String.length digits then digits ^ String.make (point - String.length digits) '0'
          else String.sub digits 0 point ^ "." ^ String.sub digits point (String.length digits - point)
      in (if negative then "-" else "") ^ result

(* FSharp.Core sformat.fs uses invariant g10 and appends .0 to integral output. *)
let structural value =
 match Float.classify_float value with
 | FP_nan -> "nan"
 | FP_infinite -> if value < 0. then "-infinity" else "infinity"
 | FP_zero | FP_normal | FP_subnormal ->
   let text = Printf.sprintf "%.10g" value in
   if String.for_all (fun character -> character >= '0' && character <= '9' || character = '-') text then text ^ ".0" else text
