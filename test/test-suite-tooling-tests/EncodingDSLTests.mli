(*
   EncodingDSLTests.mli - Unit tests for ARM64 and x64 encoding fixture extensions.
   Tests parser and runner behavior that cannot safely be asserted by their own DSL files.
*)
type testResult = (unit, string) result

val testParsesAndRunsARM64ExpectedEncodingError : unit -> testResult
val testParsesMultipleX64EncodingCases : unit -> testResult
val testRunsX64DeferredFixupExpectation : unit -> testResult
val tests : (string * (unit -> testResult)) list
val displayCases : X86_64EncodingFormat.x64EncodingTest list -> string
