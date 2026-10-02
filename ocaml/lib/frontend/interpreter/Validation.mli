(* Validation.mli - Enforce structural and file-purpose boundaries before lowering. *)
type mode = Script | Package | Test
type issueCode = DuplicateBinder | OrBindingMismatch | EmptyOrPattern | RecoveryHole
  | EmptyLambda | EmptyMatch | AnonymousRecord | EmptyRecordUpdate
  | PackageExpression | TestAssertion | DBMode | DBShape | TestMode
module IssueCode : sig val toString : issueCode -> string end
type issue = {
  range : WrittenTypes.range; code : issueCode; message : string;
  related : (WrittenTypes.range * string) list; hint : string option;
}
type validatedSourceFile
module ValidatedSourceFile : sig
  val mode : validatedSourceFile -> mode
  val toWrittenTypes : validatedSourceFile -> WrittenTypes.sourceFile
end
val validateStructure : WrittenTypes.sourceFile -> issue list
val validatePurpose : mode -> WrittenTypes.sourceFile -> issue list
val validate : mode -> WrittenTypes.sourceFile -> (validatedSourceFile, issue Prelude.neList) result
