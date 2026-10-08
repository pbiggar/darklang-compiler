(* Immutable source-checking state: symbol allocation and flexible type constraints. *)
type t

val create : CheckedAST.symbols -> t
val symbols : t -> CheckedAST.symbols
val resolve : t -> AST.semanticType -> AST.semanticType
val resolveExpression : t -> CheckedAST.expr -> CheckedAST.expr
val constrain : AST.semanticType -> AST.semanticType -> t -> (t, string) result

val freshenTypes :
  StringOrder.Set.t -> AST.semanticType list -> t -> AST.semanticType list * t

val allocateBinding : string -> t -> AST.bindingId * t
val internFunction : string -> t -> AST.functionId * t
val internType : string -> t -> AST.typeId * t
val internField : string -> string -> int -> t -> AST.fieldId * t
val internConstructor : string -> string -> int -> t -> AST.constructorId * t
val nextBindingOrdinal : t -> int
val constructorInfo : AST.constructorId -> t -> (string * string) option
