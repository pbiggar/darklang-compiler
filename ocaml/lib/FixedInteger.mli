(* FixedInteger.mli - Explicit two's-complement host arithmetic boundaries. *)
val wrapSigned : int -> Z.t -> Z.t
val wrapUnsigned : int -> Z.t -> Z.t
val negateInt8 : int -> int
val negateInt16 : int -> int
val negateInt128 : Z.t -> Z.t
