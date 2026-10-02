(* FormattingRoundtripFormat.mli - Parser for focused roundtrip fixture files. *)
type formattingRoundtripCase = {name : string; source : string; sourceFile : string}
val parseFormattingRoundtripFile : string -> (formattingRoundtripCase list, string) result
