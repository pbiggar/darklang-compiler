(* Complete structured type errors, including contextual resolution evidence. *)
val typeError : Dark_compiler.CheckingDiagnostics.typeError -> Yojson.Basic.t
val qualified : Dark_compiler.NameResolution.qualifiedName -> Yojson.Basic.t
val identity : Dark_compiler.NameResolution.symbolIdentity -> Yojson.Basic.t
val provenance : Dark_compiler.NameResolution.candidateProvenance -> Yojson.Basic.t
