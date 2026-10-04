(* Complete cache equality, producer identity, summary merging and overlays. *)
val observe : string -> Yojson.Basic.t

val summary : Dark_compiler.CompilationCacheIdentity.functionSummary -> Yojson.Basic.t
