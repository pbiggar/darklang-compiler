(* NamedPackageTests.mli - Separate validated package tests from executable declarations. *)
type test = {
  modules : string list;
  qualifiedName : string;
  declaration : WrittenTypes.testDecl;
}

type package = {
  declarations : Validation.validatedSourceFile;
  tests : test list;
}

(* Explicit runner boundary: ordinary source checking still rejects DTest.
   Bodies and expected errors are retained without evaluation or rewriting. *)
val partition : Validation.validatedSourceFile -> (package, string) result

(* Select exactly one declared test as a Script entry at its original lexical
   position. Other tests are removed; ordinary declarations and the selected
   test's expected outcome are preserved for the runner. *)
val executable :
  Validation.validatedSourceFile ->
  string ->
  (Validation.validatedSourceFile * test, string) result
