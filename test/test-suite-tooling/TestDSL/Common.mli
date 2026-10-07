(* Common.mli - Shared section-delimited test DSL parsing and text escapes. *)
type section = string * string
type testFile = { sections : string Dark_compiler.StringOrder.Map.t }

val parseSections : string -> section list
val parseTestFile : string -> testFile
val getRequiredSection : string -> testFile -> (string, string) result
val getOptionalSection : string -> testFile -> string option
val stripCommentsAndEmpty : string -> string list
val normalizeLineEndings : string -> string
val parseEscapedText : string -> (string, string) result
