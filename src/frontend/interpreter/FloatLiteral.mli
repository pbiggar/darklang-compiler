(* FloatLiteral.mli - Exact decimal precision validation and exponent-free normalization. *)
type error = Overflow | Underflow | Precision | Invalid

val diagnostic : error -> string * string
val parse : string -> float -> (string * string, error) result
