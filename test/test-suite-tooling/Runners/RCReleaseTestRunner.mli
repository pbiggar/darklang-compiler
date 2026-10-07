(*
   RCReleaseTestRunner.mli - Executes semantic managed-graph release fixtures.
   Builds canonical LIR heap graphs and requires final release to leave no leaks.
   The fixture constructs a boxed root. The extra payload
   case keeps String sums boxed even when nullable two-case
   String sums use the payload pointer directly.
*)
open Dark_compiler

val buildProgram :
  RCReleaseFormat.rCReleaseTest ->
  (LIR.program * RCReleaseFormat.preservedRegister list, string) result

val runRCReleaseTest :
  Platform.target -> RCReleaseFormat.rCReleaseTest -> (unit, string) result

val loadRCReleaseTests :
  string -> (RCReleaseFormat.rCReleaseTest list, string) result

val tests :
  Platform.target ->
  string array ->
  (string * (unit -> (unit, string) result)) list
