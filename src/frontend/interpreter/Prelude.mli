(* Prelude.mli - Preserve shared frontend definitions. *)
type 'a neList = 'a ParserDependencies.neList

module Map : sig
  val values : (string * 'v) list -> 'v list
end
