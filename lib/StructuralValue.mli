(* Typed structural descriptions for opaque compiler diagnostics. *)
type value = Scalar of string | Text of string | Union of string * value list | Sequence of value list | Array of value list | Tuple of value list | Record of (string * value) list
