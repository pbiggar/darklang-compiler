(* FormattingRoundtripTests.mli - Run the original parser/pretty roundtrip fixtures. *)
val tests : string array -> (string * (unit -> (unit, string) result)) list
