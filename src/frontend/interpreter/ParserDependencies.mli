(* ParserDependencies.mli - Share nonempty lists while preserving the interpreter parser API. *)
type 'a neList = 'a NonEmptyList.t = { head : 'a; tail : 'a list }

val ofList : 'a -> 'a list -> 'a neList
val toList : 'a neList -> 'a list
val ofListWithDefault : 'a -> 'a list -> 'a neList
