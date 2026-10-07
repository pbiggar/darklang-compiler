(* FunctionIdMap.mli - Sparse function tables with unsigned scalar ordinal keys. *)
type 'a t

val empty : 'a t
val isEmpty : 'a t -> bool
val count : 'a t -> int
val add : AST.functionId -> 'a -> 'a t -> 'a t
val remove : AST.functionId -> 'a t -> 'a t
val change : AST.functionId -> ('a option -> 'a option) -> 'a t -> 'a t
val tryFind : AST.functionId -> 'a t -> 'a option
val containsKey : AST.functionId -> 'a t -> bool
val find : AST.functionId -> 'a t -> 'a
val ofSeq : (AST.functionId * 'a) Seq.t -> 'a t
val ofList : (AST.functionId * 'a) list -> 'a t
val ofArray : (AST.functionId * 'a) array -> 'a t
val toSeq : 'a t -> (AST.functionId * 'a) Seq.t
val toList : 'a t -> (AST.functionId * 'a) list
val keys : 'a t -> AST.functionId Seq.t
val values : 'a t -> 'a Seq.t

val fold :
  ('state -> AST.functionId -> 'a -> 'state) -> 'state -> 'a t -> 'state

val iter : (AST.functionId -> 'a -> unit) -> 'a t -> unit
val map : (AST.functionId -> 'a -> 'b) -> 'a t -> 'b t
val filter : (AST.functionId -> 'a -> bool) -> 'a t -> 'a t
val exists : (AST.functionId -> 'a -> bool) -> 'a t -> bool
val forall : (AST.functionId -> 'a -> bool) -> 'a t -> bool
val maxKeyValue : 'a t -> AST.functionId * 'a
val merge : 'a t -> 'a t -> 'a t
