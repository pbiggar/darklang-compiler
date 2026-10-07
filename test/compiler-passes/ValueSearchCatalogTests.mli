(*
   ValueSearchCatalogTests.mli - native integration tests for the AOT package-value catalog boundary.

   Catalog data is compilation input rather than Dark source, so these tests
   compile and execute complete programs through CompilerLibrary instead of the
   line-oriented E2E DSL.
   The upstream Int8 probe reads Darklang.Test.Values.int8Value. The
   interpreter resolves that package global to 5y; AOT receives the same value
   as an immutable catalog evaluator, never through live package lookup.
*)
open Dark_compiler
type testResult=(unit,string) result
val testCatalogParity : CompilationContexts.stdlibResult -> unit -> testResult
val testCatalogRejectsIllTypedAvailableValue : CompilationContexts.stdlibResult -> unit -> testResult
val testInt8PackageProbeParity : CompilationContexts.stdlibResult -> unit -> testResult
val tests : CompilationContexts.stdlibResult -> (string * (unit -> testResult)) list
