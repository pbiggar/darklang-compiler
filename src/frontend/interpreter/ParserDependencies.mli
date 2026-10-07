(* ParserDependencies.mli - Preserve shared frontend definitions. *)
type 'a neList = { head : 'a; tail : 'a list }

val ofList : 'a -> 'a list -> 'a neList
val toList : 'a neList -> 'a list
val ofListWithDefault : 'a -> 'a list -> 'a neList
