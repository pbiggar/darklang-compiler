(* AsyncListStorage.mli - Identify typed skew-list allocation components before async rewriting. *)
type slot = Element of AST.semanticType | Link of AST.semanticType
type t

val create : ANF.functionDef -> t
val root : t -> ANF.atom -> (ANF.tempId * AST.semanticType) option
val slot : t -> ANF.atom -> ANF.atom -> (ANF.tempId * slot) option
