(*
   StdlibSourceTests.mli - Source-level invariants for maintained stdlib files.
   These checks catch stdlib definitions that are easy to shadow accidentally
   before the compiler accepts a misleading or unreachable implementation.
*)
type testResult = (unit, string) result

val testStdlibHasNoDuplicateDefinitions : unit -> testResult
val tests : (string * (unit -> testResult)) list
