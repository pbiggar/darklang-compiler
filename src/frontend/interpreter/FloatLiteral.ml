(* FloatLiteral.ml - Validate source precision against exact binary64 bits. *)
type error = Overflow | Underflow | Precision | Invalid

let diagnostic = function
  | Overflow -> ("PARSE-FLOAT-OVERFLOW", "Float literal overflows binary64")
  | Underflow ->
      ("PARSE-FLOAT-UNDERFLOW", "Nonzero float literal underflows to zero")
  | Precision ->
      ( "PARSE-FLOAT-PRECISION",
        "Float literal loses precision specified in the source" )
  | Invalid -> ("PARSE-FLOAT-INVALID", "Invalid float literal")

let digits text = String.for_all (fun char -> char >= '0' && char <= '9') text

let trimLeadingZeros text =
  let rec first index =
    if index < String.length text && text.[index] = '0' then first (index + 1)
    else index
  in
  let start = first 0 in
  if start = String.length text then "0"
  else String.sub text start (String.length text - start)

(* Exponents can contain arbitrarily many digits. A finite nonzero binary64
   bounds the effective exponent by the mantissa's source length plus 324;
   saturating beyond that bound avoids allocating an enormous power of ten. *)
let exponent limit text =
  let length = String.length text in
  let start, sign =
    if length > 0 && text.[0] = '-' then (1, -1)
    else if length > 0 && text.[0] = '+' then (1, 1)
    else (0, 1)
  in
  let rec loop index value =
    if index = length then Some (sign * value)
    else if text.[index] < '0' || text.[index] > '9' then None
    else
      let digit = Char.code text.[index] - Char.code '0' in
      let value =
        if value > (limit - digit) / 10 then limit else (value * 10) + digit
      in
      loop (index + 1) value
  in
  if start = length then None else loop start 0

let binaryRatio value =
  let bits = Int64.bits_of_float (Float.abs value) in
  let encodedExponent =
    Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7ffL)
  in
  let fraction = Int64.logand bits 0x000fffffffffffffL in
  let significand, power =
    if encodedExponent = 0 then (fraction, -1074)
    else (Int64.logor fraction 0x0010000000000000L, encodedExponent - 1075)
  in
  let significand = Z.of_int64 significand in
  if power >= 0 then (Z.shift_left significand power, Z.one)
  else (significand, Z.shift_left Z.one (-power))

let preservesPrecision value coefficient power =
  let numerator, denominator = binaryRatio value in
  let numerator, denominator =
    if power >= 0 then (numerator, Z.mul denominator (Z.pow (Z.of_int 10) power))
    else (Z.mul numerator (Z.pow (Z.of_int 10) (-power)), denominator)
  in
  (* Compare with half a unit at the last specified decimal place. Equality
     is accepted only when the source coefficient is the even tie winner. *)
  let distance = Z.abs (Z.sub numerator (Z.mul coefficient denominator)) in
  let comparison = Z.compare (Z.shift_left distance 1) denominator in
  comparison < 0 || (comparison = 0 && not (Z.testbit coefficient 0))

let normalized whole fraction power =
  let allDigits = whole ^ fraction in
  let point = String.length whole + power in
  let whole, fraction =
    if point <= 0 then ("0", String.make (-point) '0' ^ allDigits)
    else if point >= String.length allDigits then
      (allDigits ^ String.make (point - String.length allDigits) '0', "0")
    else
      ( String.sub allDigits 0 point,
        String.sub allDigits point (String.length allDigits - point) )
  in
  (trimLeadingZeros whole, if fraction = "" then "0" else fraction)

let parse source value =
  let source =
    if String.starts_with ~prefix:"-" source then
      String.sub source 1 (String.length source - 1)
    else source
  in
  let parts =
    String.split_on_char 'e'
      (String.map (fun char -> if char = 'E' then 'e' else char) source)
  in
  let decimal =
    match parts with
    | [ mantissa ] -> Some (mantissa, "0")
    | [ mantissa; power ] -> Some (mantissa, power)
    | _ -> None
  in
  match decimal with
  | None -> Error Invalid
  | Some (mantissa, powerText) -> (
      let parts =
        match String.split_on_char '.' mantissa with
        | [ whole ] -> Some (whole, "")
        | [ whole; fraction ] -> Some (whole, fraction)
        | _ -> None
      in
      match parts with
      | None -> Error Invalid
      | Some (whole, fraction)
        when whole ^ fraction = "" || not (digits whole && digits fraction) ->
          Error Invalid
      | Some (whole, fraction) -> (
          let coefficientText = trimLeadingZeros (whole ^ fraction) in
          match Float.classify_float value with
          | FP_nan -> Error Invalid
          | FP_infinite -> Error Overflow
          | FP_zero when coefficientText <> "0" -> Error Underflow
          | FP_zero -> Ok ("0", "0")
          | FP_normal | FP_subnormal -> (
              match exponent (String.length source + 400) powerText with
              | None -> Error Invalid
              | Some power ->
                  let coefficient = Z.of_string coefficientText in
                  if
                    preservesPrecision value coefficient
                      (power - String.length fraction)
                  then Ok (normalized whole fraction power)
                  else Error Precision)))
