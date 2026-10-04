(* Original graph-color fixture parsing and execution tests. *)
type testResult=(unit,string) result
val testParsesAndRunsMultipleGraphCases : unit -> testResult
val testRejectsUnknownVerticesInEdges : unit -> testResult
val tests : (string * (unit -> testResult)) list
