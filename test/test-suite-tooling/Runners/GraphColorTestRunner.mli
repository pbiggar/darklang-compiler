(* Execute typed coloring fixtures and preserve failure evidence. *)
val runGraphColorTest : GraphColorFormat.graphColorTest -> TestOutcome.t

val loadGraphColorTests :
  string -> (GraphColorFormat.graphColorTest list, string) result

val tests : string array -> (string * (unit -> (unit, string) result)) list
