(* HostStructuralFormat.mli - Typed F# structural layouts used in stable identities. *)
type value = StructuralValue.value = Scalar of string | Text of string | Union of string * value list | Sequence of value list | Array of value list | Tuple of value list | Record of (string * value) list
val format : value -> string
val semanticType : AST.semanticType -> string
val binOp : AST.binOp -> string
val semanticValue : AST.semanticType -> value
