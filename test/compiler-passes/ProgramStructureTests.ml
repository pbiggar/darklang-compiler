(*
   ProgramStructureTests.fs - Whole-program source-unit, overlay, and entry validation probes.
   A library unit that declares a function the stdlib already carries under a
   non-Stdlib name (darklang/dark's LanguageTools modules are in both). The two
   lowerings differ, and the merge used to crash on the name instead of keeping
   the prebuilt copy as it does for Stdlib.* names.
   A library unit's generic function instantiates a stdlib generic only once it
   is itself specialized by the executable unit. In TestExpression mode the
   stdlib specialization was never requested ('Missing specialization for
   Stdlib.List.flatten<str>').
*)
[@@@warning "-4-42"]
open Dark_compiler
module C=CompilationContexts
module O=CompilerOptions
module P=NameSyntax.SourceUnitPurpose
type testResult=(unit,string) result
let source name purpose text:C.sourceUnit={C.name;purpose;source=text}
let compile stdlib mode sources=CompilerLibrary.compile {C.context=C.StdlibOnly stdlib;mode;sources=AST.NonEmptyList.fromList sources;allowInternal=false;verbosity=0;options=O.defaultOptions;packageValues=C.emptyPackageValueCatalog;packageManager=None;passTimingRecorder=None;session=None}
let execute (report:O.compileReport) binary=E2ETestRunner.executeBinaryForTarget report.O.target binary
let expectCompileError expected (report:O.compileReport)=match report.O.result with
 |Error error when Text.contains error expected->Ok ()
 |Error error->Error ("Expected compile error containing '"^expected^"', got: "^error)
 |Ok _->Error ("Expected compile error containing '"^expected^"', but compilation succeeded")
let expectExecution description outputDescription expected report=match report.O.result with
 |Error error->Error error|Ok binary->match execute report binary with
  |Error error->Error (description^" did not execute: "^error)
  |Ok output->if output.O.exitCode=0 && output.O.stdout=expected then Ok () else Error (Printf.sprintf "Unexpected %s output: exit=%d; stdout=%s; stderr=%s" outputDescription output.O.exitCode output.O.stdout output.O.stderr)
let testOrderedSourceComposition stdlib ()=compile stdlib O.TestExpression [source "library.dark" P.Library "let answer (x: Int64): Int64 = x + 1L";source "entry.dark" P.Executable "answer 41L"] |> expectExecution "Multi-unit program" "multi-unit" "42\n"
let testDuplicateFunctionAcrossSourceUnitsRejected stdlib ()=compile stdlib O.TestExpression [source "first.dark" P.Library "let overlaid (x: Int64): Int64 = x + 1L";source "second.dark" P.Library "let overlaid (x: Int64): Int64 = x + 2L";source "entry.dark" P.Executable "overlaid 40L"] |> expectCompileError "Duplicate function 'overlaid'"
let testNestedModuleUsesInterpreterRelativeCandidates stdlib ()=compile stdlib O.TestExpression [source "parent.dark" P.Library "module Darklang.Relative.Parent\n\nlet answer (x: Int64): Int64 = x + 1L";source "child.dark" P.Library "module Darklang.Relative.Parent.Child\n\nlet callParent (x: Int64): Int64 = Parent.answer x";source "entry.dark" P.Executable "Darklang.Relative.Parent.Child.callParent 41L"] |> expectExecution "Relative-name program" "relative-name" "42\n"
let testDependencyEntryRejected stdlib ()=compile stdlib O.FullProgram [source "dependency.dark" P.Package "1"] |> expectCompileError "must contain declarations only"
let testMissingEntryRejected stdlib ()=compile stdlib O.FullProgram [source "library.dark" P.Executable "let identity (x: Int64): Int64 = x"] |> expectCompileError "exactly one entry expression; found 0"
let testMultipleEntriesRejected stdlib ()=compile stdlib O.FullProgram [source "one.dark" P.Executable "1";source "two.dark" P.Executable "2"] |> expectCompileError "exactly one entry expression; found 2"
let testFileEntryTypeRejected stdlib ()=compile stdlib O.FullProgram [source "file.dark" P.Executable "\"render only in eval mode\""] |> expectCompileError "File entry expression must return Unit, Int, or Int64"
let testLibraryRedeclaresStdlibCarriedFunction stdlib ()=compile stdlib O.TestExpression [source "library.dark" P.Library (String.concat "\n" [
 "module Darklang.LanguageTools.PackageManager";"";"let countMatchingPrefix (a: List<String>) (b: List<String>) : Int =";"  match (a, b) with";"  | (aHead :: aTail, bHead :: bTail) ->";"    if aHead == bHead then";"      1 + (countMatchingPrefix aTail bTail)";"    else";"      0";"  | _ -> 0"]);
 source "entry.dark" P.Executable "Darklang.LanguageTools.PackageManager.countMatchingPrefix [\"a\", \"b\", \"c\"] [\"a\", \"b\", \"x\"]"] |> expectExecution "Redeclaring program" "redeclaring" "2\n"
let testLibraryGenericReachesStdlibSpecialization stdlib ()=compile stdlib O.TestExpression [source "library.dark" P.Library (String.concat "\n" ["type Located<'a> = { entity: 'a, modules: List<String> }";"";"let fullPathOf<'a> (item: Located<'a>): String =";"  [[\"root\"], item.modules] |> Stdlib.List.flatten |> Stdlib.String.join \".\""]);source "entry.dark" P.Executable "fullPathOf (Located { entity = 5L, modules = [\"a\", \"b\"] })"] |> expectExecution "Generic library program" "generic library" "\"root.a.b\"\n"
let tests stdlib=[
 "compose ordered named source units",testOrderedSourceComposition stdlib;
 "duplicate function across source units rejected",testDuplicateFunctionAcrossSourceUnitsRejected stdlib;
 "nested modules use interpreter relative candidates",testNestedModuleUsesInterpreterRelativeCandidates stdlib;
 "a library may redeclare a function the stdlib carries",testLibraryRedeclaresStdlibCarriedFunction stdlib;
 "a library generic reaches its stdlib specializations",testLibraryGenericReachesStdlibSpecialization stdlib;
 "dependency entry is rejected",testDependencyEntryRejected stdlib;
 "missing entry is rejected",testMissingEntryRejected stdlib;
 "multiple entries are rejected",testMultipleEntriesRejected stdlib;
 "file entry type is restricted",testFileEntryTypeRejected stdlib]
