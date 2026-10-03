(* Complete closure environments, captures, inference, and name allocation. *)
val observe : string -> Yojson.Basic.t

val observeComparisons : string -> Yojson.Basic.t
val observeLiftExpressions : string -> Yojson.Basic.t
val observeLiftFunctions : string -> Yojson.Basic.t
