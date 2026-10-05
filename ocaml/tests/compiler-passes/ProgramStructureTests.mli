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
open Dark_compiler
type testResult=(unit,string) result
val testOrderedSourceComposition : CompilationContexts.stdlibResult -> unit -> testResult
val testDuplicateFunctionAcrossSourceUnitsRejected : CompilationContexts.stdlibResult -> unit -> testResult
val testNestedModuleUsesInterpreterRelativeCandidates : CompilationContexts.stdlibResult -> unit -> testResult
val testDependencyEntryRejected : CompilationContexts.stdlibResult -> unit -> testResult
val testMissingEntryRejected : CompilationContexts.stdlibResult -> unit -> testResult
val testMultipleEntriesRejected : CompilationContexts.stdlibResult -> unit -> testResult
val testFileEntryTypeRejected : CompilationContexts.stdlibResult -> unit -> testResult
val testLibraryRedeclaresStdlibCarriedFunction : CompilationContexts.stdlibResult -> unit -> testResult
val testLibraryGenericReachesStdlibSpecialization : CompilationContexts.stdlibResult -> unit -> testResult
val tests : CompilationContexts.stdlibResult -> (string * (unit -> testResult)) list
