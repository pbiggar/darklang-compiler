(* SyntaxTestRunner.mli - Execute canonical source syntax fixtures. *)
val runSyntaxTest : SyntaxFormat.syntaxTest -> TestOutcome.t
val loadSyntaxTests : string -> (SyntaxFormat.syntaxTest list, string) result
val tests : string array -> (string * (unit -> (unit, string) result)) list
