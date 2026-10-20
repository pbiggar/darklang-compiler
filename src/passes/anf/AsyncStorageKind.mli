(* AsyncStorageKind.mli - Source descriptors shared by transport graphs and physical layout resolution. *)
type t =
  | TupleStorage
  | RecordStorage of ANF.recordDescriptor
  | ListStorage of AST.semanticType
  | DictStorage of AST.semanticType * AST.semanticType
  | SumStorage of AsyncSumStorage.layout

val compatible : t -> t -> bool
val sourceFields : t -> AST.semanticType -> AST.semanticType list
