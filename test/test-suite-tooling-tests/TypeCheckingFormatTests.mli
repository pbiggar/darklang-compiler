(* Original type checking fixture parser tests. *)
type testResult = (unit, string) result
val testParsesSlashSlashInsideStringLiteral : unit -> testResult
val testParsesTypeKeywordsWithCultureInvariantCasing : unit -> testResult
val testParsesEscapedQuoteBeforeSlashSlashInsideStringLiteral : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
