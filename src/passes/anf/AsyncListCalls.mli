(* AsyncListCalls.mli - Classify list builders whose payloads retain independent async layouts. *)
type argument = ListInput | ElementInput
type builder = { callee : string; arguments : argument list }

val builder : string -> builder option
