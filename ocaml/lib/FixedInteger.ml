(* FixedInteger.ml - Preserve sized F# negation, including MinValue wrapping. *)
let wrapUnsigned bits value = Z.extract value 0 bits
let wrapSigned bits value =
  let unsigned = wrapUnsigned bits value in
  if Z.testbit unsigned (bits - 1) then Z.sub unsigned (Z.shift_left Z.one bits) else unsigned
let negate bits value = wrapSigned bits (Z.neg value)
let negateInt8 value = negate 8 (Z.of_int value) |> Z.to_int
let negateInt16 value = negate 16 (Z.of_int value) |> Z.to_int
let negateInt128 value = negate 128 value
