# Traits — implementation checkpoint

Compiler baseline: `d8e2d7161ecf6892079906cdc5d797d6aa87e7fb`.
Interpreter reference: darklang/dark `a9d93efc853486244630dad693fe79f6f4882cf6`.
Reference grammar: `backend/src/LibParser/{Parser,WrittenTypes}.fs`.
Reference semantics: `backend/src/LibExecution/Traits.fs` and
`packages/darklang/stdlib/traits.dark` at that revision.

This is an unfinished implementation checkpoint. M0–M5 are implemented.
M6 package breadth and M7 remain unfinished; this is not a readiness claim.

| Behavior | Checkpoint implementation | Executed evidence / remaining validation |
| --- | --- | --- |
| Trait and impl grammar, method aliases, multiple methods | Range-bearing syntax and source-unit flattening | `test/fixtures/syntax/traits.syntax` |
| Single, multiple, parameterized and type-declaration bounds | Parsed, retained and enforced on function calls and type uses | `test/fixtures/syntax/traits.syntax` |
| Concrete named method calls | Select an implementation and emit ordinary checked function calls | `test/fixtures/e2e/traits.e2e` |
| Method aliases | Validate against the referenced ordinary function signature | `test/fixtures/e2e/traits-alias.e2e` |
| Implementation methods as ordinary functions | Public `<Type>.<Trait>.<method>` spellings resolve to the existing method FunctionId when unique | `traits-upstream-shapes.e2e` |
| Trait method function values | Existing generic function references select static method targets before specialization planning | `traits-upstream-shapes.e2e`, `traits-method-values.e2e` |
| Identically shaped trait declarations | Coalesce identities using upstream hash-relevant fields; declaration/parameter names are cosmetic, binder names and method order are significant | `traits-content-identity.e2e` |
| Trait-aware sorting | Select `Compare.lessThan` statically for concrete list elements; retain structural fallback and stable equal ordering | `traits-sort.e2e` |
| Unknown trait, missing impl, incomplete method set, wrong signature | Frontend compile errors | `traits-{unknown,missing-impl,method-set,wrong-method}.e2e` |
| Competing identical implementations | Compile-time ambiguity when selecting a call | `traits-ambiguous.e2e` |
| Traits as value types | Traits are excluded from the nominal type catalog | `test/fixtures/e2e/traits-as-types.e2e` passed |
| Bounded generic specialization | Typed calls resolve after substitution, before ordinary dependency planning | `traits-generic.e2e`, `traits-multiple-bounds.e2e`, `traits-generic-values.e2e`, `traits-bound-failure.e2e` passed |
| Bounds on generic types | Check construction, signatures and declaration field types | `traits-type-bounds.e2e` passed |
| Generic/conditional implementations | Directional argument matching, recursive obligations, blanket fallback and static specialization | `traits-conditional.e2e`, `traits-concrete-arguments.e2e`, `traits-generic-alias.e2e`, `traits-patterns.e2e`, `traits-shared-alias.e2e` passed |
| Receivers and operators | Static receiver calls, record-field precedence, custom and bounded-generic operators; primitive fast paths preserved | `traits-receivers.e2e`, `traits-receiver-parameters.e2e`, `traits-receiver-ambiguity.e2e`, `traits-operators.e2e` |
| Upstream stdlib traits and hosted packages | Unchanged trait declarations/utilities; native primitive aliases; hosted trait/impl/bound rendering and dependency closure | `traits-stdlib.e2e`, `traits-stdlib-identities.e2e`, `trait_packages_regression.py`; package discovery limitations below |
| Full parity and readiness gates | Not executed (M7) | Required |

Intentional scoped differences: selection must finish at compilation; equally
applicable competing implementations are errors. Trait objects, runtime lookup,
newest-wins arbitration, trait effect ceilings and persistence/history pinning
are outside the work order. Existing ordinary function effect checks remain.

Validation:

- Prescribed toolchain retry passed: OCaml 5.5.1, Dune 3.24.2, all 98 locked
  packages, native compilation, Unix processes, temporary files, GMP and SQLite.
- Baseline `./build --ai` passed. The baseline runner confirmed unsupported trait
  parsing; its suite context failed on the concrete trait preamble and consequently
  invalidated the other tests. This is failure evidence, not a baseline pass.
  The baseline used the same corrected trait-aware E2E loader, without the
  compiler/parser implementation changes.
- `env -u LD_PRELOAD ./build --ai` passed after strict-warning fixes.
- `./run-tests --ai --filter=traits`: **12/12 passed** (15.6 seconds).
- `./run-tests --ai --filter=syntax`: **82/82 passed** (14.3 seconds).
- `./run-tests --ai`: **13,394/13,394 passed** (128.9 seconds), native
  Linux x86-64 host suite.
- `dune runtest`: **passed** on the current source, including HTTP/TLS, package
  caches, native runtime, process/terminal and differential-tool regressions.
- `git diff --check` passed; changed OCaml files were formatted with pinned
  ocamlformat. Syntax-only checks are superseded by the successful full build.
- `./benchmarks/run_benchmarks.sh --verify-parent full`: **blocked before
  measurement**, exit 1: `snapshot workload contract digest is incompatible`.
  The same command on the compiler baseline checkout fails with the same
  diagnostic. No timing comparison or performance pass is claimed. The stored
  baseline needs reconciliation; this task has not reset it.

The upstream generic `Stdlib.toString` is now loaded and statically specialized.

M3 focused validation: rebuilt successfully; `./run-tests --ai --filter=traits`
passed **22/22** (15.7 seconds). The full host and regression reruns are in
the records below. Static obligations are retained in the immutable checked catalog;
method identities are canonical ordinary functions. Specialization rewrites
trait applications before dependency planning, and final preparation returns a
compile error for unresolved obligations. There is no trait-specific backend ABI.

M3 full host rerun passed **13,404/13,404** (134.7 seconds); `dune runtest`
passed. M4 focused suite passed **40/40** (17.5 seconds), including method-local
generics and conditional proofs inside generic bodies. M4 full host passed
**13,422/13,422** (156.5 seconds); regression checks passed. Implementation entries retain independent
identities even when two impls alias the same function.

The blanket fixtures use `for ('a)` to preserve the pinned lexer grammar's
apostrophe context handling; the pinned upstream lexer does not treat a bare
apostrophe after `for` as a type variable. No lexer extension was introduced.

M5 focused suite passed **66/66** (16.8 seconds). Full-host validation exposed
diagnostic regressions for invalid primitive operators and stream ordering;
these cases retain their previous diagnostics. Full host passed **13,448/13,448**
(126.3 seconds).
`dune runtest` passed, including package caches, HTTP/TLS, host/runtime and
differential-tool regressions. `StdLib/traits.dark` is byte-identical to the
pinned upstream source.

M6 focused checks passed **87/87** (17.6 seconds). Primitive implementations
reuse the existing native functions for integer arithmetic, comparisons,
bitwise operations and rendering; Float, String, Char, Bool, DateTime and Uuid
receive their upstream trait surfaces. String ordering now uses the upstream
Compare implementation, so the prior rejection test now expects `false`.
The hosted-package native E2E reproduced a missing trait dependency before
implementation and then passed. Its current pipeline-format extension also
reproduced a renderer failure before its fix. The full-host rerun passed
**13,469/13,469** (150.0 seconds); `dune runtest` passed after the pipeline and
offline-cache fixes.

Package imports fetch `trait` and `impl` kinds by name/hash alongside the
existing kinds, preserve source names with location/hash, follow trait and
method dependencies, and render bounds on functions, types, traits and methods.
The E2E uses a local HTTP package service and executes a bounded imported
function and pipeline against an imported implementation. This does not
establish support by a deployed upstream service: the pinned server source
under `later/packages` exposes the earlier type/value/function routes.
The importer loads implementations reached by qualified implementation-method
references and dependencies; it does not enumerate unrelated active impls
from a remote server. Broader implementation discovery remains M6 work.

Stored `EFnName.boundImpls`, `EPipeFnCall.boundImpls` and trait implementation
choice/history metadata are not preserved as persistence semantics by the
source renderer. Compilation selects against the imported static catalog.
This is a deferred persistence-semantic gap, not a claim to preserve upstream
saved-call pinning. Upstream trait-corpus adoption and reusable-catalog boundary
coverage remain M7 work. The cache-context fixture passed **2/2** and confirms
that a caller-local specific implementation does not reuse the blanket result.

M7 coverage is in progress. The unchanged current upstream trait corpus is
stored at `test/fixtures/upstream-reference/language/traits.dark` and compares
byte-for-byte with the pinned source. Attempting to execute it exposed a test
infrastructure mismatch: its named `test` and `=> raises` syntax and
`Stdlib.Test` assertions are newer than this compiler's fixture loader. No
claim is made that this reference file executes. Adapted expectations in
`traits-upstream-shapes.e2e` cover its concrete, conditional, callback,
receiver and public implementation-function behaviors. They reproduced
unsupported method names and method values before their fixes.

M7's focused suite passed **111/111** (17.8 seconds), including receiver
partial-application boundary cases. It also reproduced a false receiver-owner
ambiguity caused by an unsatisfied conditional bound on a parameterized trait;
owner filtering now discharges those known bounds. Trait-method partial values
infer their self type and lower to ordinary captured lambdas. Identical local
trait signatures share the upstream content identity; existing negative tests
now use different method names where they require genuinely different traits.
The coalescing key follows `LibSerialization/Hashing/Canonical.fs.writeTrait`
at the pinned revision, including effect ceilings as identity data without
enforcing them. Sorting uses a frontend-only static marker that is replaced
by the chosen ordinary method call or existing structural comparison before
ANF; no trait-specific runtime or backend operation is introduced.

Remaining coverage gaps include full execution of the
new upstream test format and broad remote implementation discovery. These
are unfinished work, rather than approved semantic differences. The performance
gate remains blocked before measurement by the baseline digest mismatch.
The final full-host and regression reruns after these M7 fixes are pending.
