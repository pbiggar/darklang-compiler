# Traits — implementation checkpoint

Compiler baseline: `d8e2d7161ecf6892079906cdc5d797d6aa87e7fb`.
Interpreter reference: darklang/dark `a9d93efc853486244630dad693fe79f6f4882cf6`.
Reference grammar: `backend/src/LibParser/{Parser,WrittenTypes}.fs`.
Reference semantics: `backend/src/LibExecution/Traits.fs` and
`packages/darklang/stdlib/traits.dark` at that revision.

M0–M6 are implemented for the scoped static model. M7 compatibility coverage
includes the pinned upstream trait corpus. Readiness remains blocked by the
parent performance baseline contract described below.

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
| Readiness gates | Host and regression checks pass; parent performance gate blocks before measurement | Validation below; named trait tests execute with approved AOT overrides |

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
function and pipeline against an imported implementation. The importer also uses the pinned upstream `/:location` module browser, whose
inventories include complete located traits and implementations. It follows
child modules and loads active implementations without requiring an explicit
implementation-method reference. Required missing dependencies remain compile
errors. Legacy services without module browsing retain name/hash lookup.
The local service models this documented upstream endpoint; this is not an
execution claim against a deployed remote service.

Stored `EFnName.boundImpls`, `EPipeFnCall.boundImpls` and trait implementation
choice/history metadata are not preserved as persistence semantics by the
source renderer. Compilation selects against the imported static catalog.
This is a deferred persistence-semantic gap, not a claim to preserve upstream
saved-call pinning. Saved-call history remains deliberately deferred. The cache-context fixture passed **2/2** and confirms
that a caller-local specific implementation does not reuse the blanket result.

The pinned upstream trait corpus is archived unchanged at
`test/fixtures/upstream-reference/language/traits.dark`. The enabled copy at
`test/fixtures/e2e/upstream/language/traits.dark` differs only by nine approved
`#compileerror=` expectations for failures the interpreter raises at runtime.
The fixture loader supports named tests and `=> raises`, preserves declaration
and test expressions, and uses a fixture-only `Stdlib.Test.equal`/`all` adapter.
The adapter returns ordinary sum values; it does not implement runtime test
counters or reflective mismatch rendering. All **36/36** upstream trait tests
passed (16.6 seconds). Their first loader execution failed 36 cases; subsequent
execution exposed missing caller constructor identities in stdlib helper
specialization. Caller type metadata now composes into that preparation without
importing caller implementation catalogs.

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

The parent performance gate remains blocked before measurement by the stored
baseline digest mismatch. When a nominal leaf shadows a module, the importer uses the pinned
`/search?modules=...&searchDepth=direct&entityTypes=module` endpoint to discover
child modules, then reads complete trait/impl browser leaves. The hosted
regression covers this shape. Alias-only implementations
have no method child module. The importer probes their upstream-defined
`<module>[.<Type>].<Trait>` locations for traits in the static catalog; the
hosted regression covers an external method alias hidden beneath a nominal
leaf. Inventories and browser leaves retain their canonical hashes/locations.


The M7 identity/sorting/method-value checkpoint passed the full host suite
**13,493/13,493** (147.5 seconds) and `dune runtest`. A subsequent package
boundary test reproduced attempts to fetch an already loaded stdlib trait;
package preparation now receives the checked catalog's known trait names
without adding traits to the ordinary type namespace. The hosted E2E then
passed with a stdlib-bounded imported function and verifies that the package
service receives no fetch for that stdlib trait.

Primitive trait boundary checks reproduced an out-of-heap failure in 128-bit
powers: converting an unbounded power to a fixed-width result allocated the
unbounded intermediate. Signed and unsigned trait powers now use wrapping
repeated squaring, as upstream's bounded power implementation does. The focused
suite passed **125/125** (18.0 seconds) before four further exponent boundary
cases were added. The final build passed; the full host suite passed
**13,511/13,511** (143.7 seconds), including all four additional exponent
boundaries, and `dune runtest` passed. The added `traits-sort-cache-context.e2e`
passed **2/2** (17.2 seconds): a caller-local Compare implementation changes
sorting even when the same concrete sort had already used structural order.
No extra cache invalidation machinery was necessary for that tested boundary.

Final commands used the pinned activation file and an external Dune build
directory, `/workspace/scratch/a30f2ed4b872/toolchains/traits-build-packages-final`:

- `env -u LD_PRELOAD ./build --ai -- --build-dir=...`: passed.
- `./run-tests --ai --build-dir=...`: passed, **13,511/13,511**.
- `PORT_VM_DUNE_BUILD_DIR=... dune runtest --build-dir=...`: passed, including
  hosted trait HTTP imports, stdlib bounds, native runtime/host/TLS, caches,
  standalone executable packaging and differential-tool regressions.
- `./benchmarks/run_benchmarks.sh --verify-parent full`: blocked, exit 1,
  before measurement because the stored baseline workload digest is
  incompatible. The baseline was not reset and no performance pass is claimed.
- Pinned ocamlformat, `git diff --check`, upstream trait-definition byte
  comparison and unchanged upstream reference-corpus byte comparison: passed.

Package discovery was tested first against a local service exposing trait and
implementation inventories but no dedicated trait/impl lookup routes. The
compiler initially rejected its missing trait dependency. After discovery,
the bounded function, pipeline and stdlib-bound function execute natively,
without requesting an already loaded stdlib trait. The new caller-sum helper
fixture adds three native equality/membership boundary checks.

The hosted regression additionally passes direct trait calls, compile-time
ambiguity and missing-implementation cases, and a second native compilation
from cached module inventories after the server shuts down. The ambiguity
fixture imports independently named implementations; no stamp or ordering
arbitration is used.

Current M7 validation uses the pinned activation and external Dune directory
`/workspace/scratch/a30f2ed4b872/toolchains/traits-build-corpus2`:

- `env -u LD_PRELOAD ./build --ai -- --build-dir=...`: passed.
- `./run-tests --ai --build-dir=...`: **13,552/13,552 passed** (147.6 seconds),
  including the 36 enabled upstream named tests and three caller-sum helper cases.
- `python3 scripts/check_compiled_leaks.py`: **58/58 clean**, covering quick
  and full workloads for all 29 canonical benchmarks. The launcher used a copy
  of the current external-build compiler in its generated default output path.
- Parent full performance verification still exits 1 before measurement with
  `snapshot workload contract digest is incompatible`; no baseline reset or
  performance comparison is claimed.
- Pinned ocamlformat and diff checks pass. The enabled upstream trait corpus
  differs only by nine AOT directives; the archived corpus and stdlib trait
  definitions match the pinned upstream source byte-for-byte.

After the final package-discovery and imported-identity changes, the full host
suite passed **13,552/13,552** (145.3 seconds), `dune runtest` passed, and the
compiled-leak gate passed **58/58** again. The performance command was retried
against the final checkpoint and remains blocked before measurement by the same
baseline contract mismatch. M7 implementation/tests/documentation are complete
for the scoped model; final readiness is not claimed while that gate is blocked.

Imported trait methods now retain `permissionCeiling` as content-identity
metadata. A hosted negative test first selected an implementation belonging to
another trait because the renderer had discarded this distinction; it now
requires a missing-implementation diagnostic for the distinct trait. No check
compares implementation effects against trait ceilings.

Final executed commands (after sourcing the pinned toolchain), with
`B=/workspace/scratch/a30f2ed4b872/toolchains/traits-build-corpus2`:

```bash
env -u LD_PRELOAD ./build --ai -- --build-dir="$B"
./run-tests --ai --build-dir="$B"
PORT_VM_DUNE_BUILD_DIR="$B" dune runtest --build-dir="$B"
python3 scripts/check_compiled_leaks.py
./benchmarks/run_benchmarks.sh --verify-parent full
```

Build, host, regressions and leaks passed. Benchmark verification exited 1
before measurement. Generated executables needed their execute permissions
restored after the VM's file synchronization; the checks were then rerun.
The final hosted regression includes discovery, alias-only impl leaves,
canonical identity metadata, missing/ambiguous selection, stdlib bounds,
pipelines and offline inventory reuse. All selected calls still become
ordinary checked function references before executable lowering.

Additional upstream adoption uses selected named tests copied without changes
from the same pinned revision's stdlib integer, Float and Char files, under
`test/fixtures/e2e/upstream/trait-adoption/stdlib`. Each file records its origin;
this is a selected-test inventory, not a claim that the entire source file runs.
There are 45 generic utility tests (`abs`, `max`, `min`, `clamp`, `sum`) and 14
rendering tests. The generic utility spellings are unavailable in the pre-trait
M0 compiler (`c9116bfe8feca093fcac0504f8d9c31706be82d4`, based on the recorded
main baseline); controlled local HTTP 404 lookup confirms unknown-function
errors for representative upstream calls to all five helpers.

The first selected-corpus execution passed 56/59. Three Float tests exposed
an unrelated constant-summary invariant comparing NaN with ordinary equality.
The driver now shares the optimizer's bitwise float operand comparison, retaining
NaN payload and signed-zero distinctions. The added `float-callee-constants.e2e`
provides native NaN-helper and negative-zero regression coverage.
