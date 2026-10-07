(*
   RCReleaseDSLTests.mli - Tests for semantic reference-release fixture parsing.
   Covers typed shape parsing, invalid placement, and executable release behavior.
*)
open Dark_compiler
type testResult=(unit,string) result
val testParsesNestedManagedShape : unit -> testResult
val testRejectsPreserveWithoutRootRegister : unit -> testResult
val testRejectsInvalidShapeArity : unit -> testResult
val testRunsNestedReleaseCase : Platform.target -> unit -> testResult
val tests : Platform.target -> (string * (unit -> testResult)) list
