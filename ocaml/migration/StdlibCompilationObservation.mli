(* Compare complete reusable stdlib and native library compilation boundaries. *)
val observe : string -> Yojson.Basic.t
val base : ((Dark_compiler.CompilationContexts.stdlibResult,string) result * string list) Lazy.t
