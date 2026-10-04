# Faithful OCaml compiler replacement

Purpose: specify the accepted migration requirements, comparison strategy,
delivery sequence, and task-specific workflow.

Status: revised following user review, 2026-10-01. Implementation is in progress;
see the recorded migration checkpoints below.

## Scope and baseline

Replace the F# compiler with OCaml in the same repository. Keep F# beside the
port as a temporary reference until the complete replacement is accepted.
The eventual rewrite in Darklang remains a goal.

Reference: latest main when planning began,
`27edbf054b400623f1856803fa0aef051f443cf8`. Freeze the reference compiler,
stdlib, and fixture inputs at this revision. Do not build a package-response
capture or replay system for this migration.
No upstream development is expected during the port. If that changes, agree
on a new baseline explicitly; do not silently compare different revisions.

The compiler inventory at this revision contains 273 F# files under
`src/DarkCompiler/`, about 5.6 MB of source. The scope includes all compiler
modules, test tooling, DSL fixtures, unit tests, CLI paths, package resolution,
stdlib integration, and existing backend capabilities. No public compiler
library API or old serialized-cache compatibility is required.

Implementation is a faithful translation of the existing algorithms. Do not
redesign the parser, improve optimizations, or fix unrelated bugs during the
port. Diagnose behavior differences; do not add artificial bug emulation.
Record discovered bugs and optimization opportunities for future work. Do not
expand this port into fixing them or deliberately recreating incidental bugs.
Resolve any effect on required output parity explicitly.

## Acceptance contract

| Area | Required result |
|---|---|
| Language | All current features work; retain compile-time validation boundaries |
| Native output | Entire generated executable is byte-for-byte identical for the same input, target, options, stdlib, and dependencies |
| Compiler CLI | Preserve flags, output, exit codes, diagnostics, and locations |
| Internal crashes | OCaml-native exception text/backtraces may replace F# ones |
| Tests | Port every unit test and test-tooling test; run every applicable DSL case without changing any existing DSL-based test or .e2e file; new tests may verify correct behavior |
| Compiler performance | Runtime and memory regressions are permitted; no performance acceptance gate |
| Profiling | The native OCaml compiler must be instrumentable with Valgrind for instruction counts and profiles |
| Packaging | Compiler may link to any needed libraries; generated Darklang executables must retain their dependency-free native output |
| Final state | OCaml replaces F#; delete F# implementation/test-runner sources after acceptance, retaining them only in Git history; no F# bridge or runtime dependency |
| Scope exclusions | Fuzzing and external-service testing, including hosted package-manager tests, postponed; no generated-program 0.1% performance gate |

Byte comparison must include executable headers, sections, padding, metadata,
runtime helpers, stdlib chunks, symbols, and instruction bytes. Comparing
stdout, disassembly, or a normalized executable alone is insufficient.

Timing fields naturally vary between runs. Their format and meaning stay
compatible; numerical duration values are not exact-text comparison inputs.
Test-runner flags, test IDs, output order, progress display, summaries, and
report formats must match as closely as possible. Deviate only where tooling
prevents matching, and document each such deviation.

## OCaml conventions

Use the latest stable upstream OCaml at toolchain selection, then pin its exact
version and dependency set. Confirm package compatibility and the ability to use Valgrind
instrumentation before locking the environment.

Use Dune and opam. Jane Street libraries and other open-source dependencies are
allowed. Proposed starting choices: Base/Stdio for host utilities, Zarith for
arbitrary-precision integers, and Unicode/JSON/SQLite/HTTP libraries selected
after semantic compatibility checks. Core is available when needed. Use Async,
not Lwt, and limit its use to necessary host I/O boundaries. Do not introduce
parallelism into the compiler; it does not need it.

Provide explicit `.mli` interfaces for each component immediately before its
implementation, rather than scaffolding every module upfront.
Preserve module responsibilities and directory organization as closely as
OCaml permits. Do not create a disconnected whole-project skeleton with
placeholder success values. Unimplemented components must be clearly absent.

Preserve functional compiler algorithms, explicit result-based recoverable
errors, semantic options, and closed types that exclude invalid states.
Exceptions are allowed for impossible internal states. Keep the translation
as close to the original as possible, including existing localized mutation
where present. Do not rewrite the lexer or other algorithms merely to remove
mutation; use the equivalent idiomatic OCaml tools.

## Exactness hazards to resolve early

1. Source text: use established OCaml Unicode libraries and suitable internal
   representations. There is no requirement to implement UTF-16 strings or
   indexing internally. Preserve language semantics, normalization, escapes,
   grapheme handling, and externally visible diagnostics as closely as possible.
2. Numbers: preserve each signed/unsigned width, wraparound, overflow checks,
   shift semantics, division/modulo behavior, and floating-point bits.
   OCaml machine integers are not substitutes for F# fixed-width integers.
3. Collections: use idiomatic equivalents, such as OCaml maps for F# maps.
   Preserve algorithms and relevant ordering/comparison behavior; in-memory
   representations need not match. Investigate any ordering differences that
   change generated bytes.
4. Evaluation: preserve operand evaluation order, optimizer behavior, register
   allocation, and program semantics. OCaml's random generator need not match
   F#'s random sequence. Internal identities may differ when they do not affect
   observable behavior or required output bytes.
5. Host formatting: preserve number/string formatting, JSON semantics,
   path behavior, warning order, and diagnostics.
6. Dependencies: port required functionality without building extra dependency
   capture infrastructure. Defer external-service testing, including the hosted
   package manager. Do not contact live services as part of parity tests yet.
7. Native emission: exactly preserve endianness, relocations, offsets,
   alignments, padding, binary headers, runtime instruction generators, and
   entire generated executable bytes. Generated code has no external library
   dependencies; this restriction does not apply to the OCaml compiler itself.
8. Baseline determinism: compile identical inputs repeatedly with F# first.
   If bytes differ, investigate before treating hashes as the oracle. The user
   reports that the current suite passes; do not presume or waive failures.

## Verification design

Treat every existing DSL-based test file and every existing `.e2e` file as
read-only. Do not edit, delete, rename, or regenerate them to accommodate the
port. Verify their contents against the baseline.

New tests, including new DSL-based and `.e2e` files, may be added when useful
for the port. Their expectations must represent correct behavior. If the F#
compiler has a bug, do not add tests that encode that bug as expected behavior
or adjust new expectations to reproduce it. Record the discrepancy for future
work and resolve any conflict with output parity explicitly.

Maintain an inventory that maps every compiler file, unit test, fixture,
test runner, and externally visible CLI path to its ported owner. Completeness
is a checked inventory, not an estimate based on passing selected examples.

Before porting the lexer, define a deterministic comparison representation
for tokens, ranges, written ASTs, checked ASTs, and later IRs. This is temporary
migration instrumentation, not a public API or old-cache compatibility layer.
Compare semantic content and floating-point bits, allowing explicit mappings
for different internal representations or ephemeral IDs where necessary. Do
not mistake harmless host-representation differences for semantic differences.
Never normalize or rewrite generated binaries for the byte comparison.

For every component:

- Run its translated unit and DSL tests against unchanged expectations.
- Run the F# and OCaml implementations on the same corpus and compare that
  component's complete semantic outputs and diagnostics.
- Integrate the OCaml prefix with the remaining F# suffix when useful.
  Account for process/serialization overhead separately from native performance.
- Do not proceed to the next component until its known differences are resolved.

The final differential harness records the complete compilation request,
target, options, preambles, stdlib inputs, diagnostics,
exit status, and emitted bytes. Retain failing inputs, both artifacts, the
first differing byte offset, and the earliest differing pass.

Compare every executable-producing test invocation, including shared E2E
batches, isolated cases, test probes, leak instrumentation, and test-used
optimization settings. Existing unit and DSL tests remain independently
required; executable comparison does not replace them. Compilation-error
cases compare rejection phase, message, and source location.

Port the test runner early, including batching, session lifetimes, caching,
process behavior, and reporting. The full test suite is the primary workload
of interest. Compiler runtime and memory may regress; measurements are
informational. Use repeated controlled runs if comparing timings, accounting
for measurement noise. Generated bytes remain an exact acceptance gate.

The native OCaml compiler must be instrumentable with Valgrind for instruction
counts and profiles on Linux x86-64. Cachegrind and Callgrind are the intended
tools, with debug information for useful compiler-function/source attribution.
This is a capability requirement, not a requirement to run compilation or the
full test suite under Valgrind routinely. A focused compatibility check may
verify the capability; ongoing profiling is optional.

## Delivery sequence

| Phase | Work | Exit condition |
|---|---|---|
| 0 | Set up local checkout/toolchain, pin reference, establish baseline determinism, fixture integrity, test inventory, and Valgrind feasibility | Reproducible local development and verified ability to count instructions and profile with Valgrind |
| 1 | Dune/opam build, module naming, foundational interfaces and implementations, semantic number/text/collection helpers, early test-runner port, comparison tooling | Foundations, runner tooling, and comparisons pass their tests |
| 2 | Tokenizer, WrittenTypes, existing lexer/parser algorithms, validation, effects, written formatting/parsing | Frontend unit/DSL tests and complete token/AST/diagnostic comparisons pass |
| 3 | Name resolution, declarations, unification, checking, helper planning, CheckedAST, package preparation | Checked programs, generated declarations, diagnostics, and rejection phases match |
| 4 | Preparation, monomorphization, closure lifting, HIR/storage/ownership analysis and verification, ANF lowering | All relevant tests and ordered ANF/ownership artifacts match |
| 5 | ANF/SSA transforms, output insertion, reachability, specialization, escape analysis, reference counts, tail calls | Every scheduled transform matches, including optimizer options |
| 6 | MIR construction/verification/optimization, call-graph scheduling, LIR lowering/peepholes, allocation, tree shaking | IRs, clobber summaries, spills, register choices, and function order match |
| 7 | Linux x86-64 backend: runtime generators, instruction selection, encoding, resolution, native emission | Complete generated files match byte for byte on that target |
| 8 | Remaining target support, driver/session behavior, stdlib builds, package functionality, CLI, all remaining unit/test-tooling coverage | Complete replacement covers existing targets; external-service testing remains explicitly deferred |
| 9 | Adapt build/run scripts, Docker and CI; run final complete parity checks and Valgrind capability verification; switch default compiler and delete replaced F# sources | All applicable tests pass, generated binaries match exactly, Valgrind instrumentability verified, no F# dependency |

Interfaces precede implementations within each phase. Driver and test hooks
may be ported early where needed to exercise the current component; their
remaining production paths finish in phase 8. Targets may reach parity at
different times. Start with Linux x86-64, matching this VM, and retain existing
Linux ARM64 and macOS ARM64 support. Report actual target validation coverage;
Linux execution alone does not establish macOS execution parity.

Retain F# as an oracle until final acceptance, then delete the replaced F#
sources and keep them only in Git history. Do not leave a runtime bridge in
the shipped replacement. Unrelated benchmark reference implementations are
not part of this compiler-source deletion.

## Workflow for this task

User instructions override the Mac-specific worktree and integration workflow:

- Work in this session's writable workspace.
- Keep all lasting changes on a dedicated `codex/ocaml-port` branch.
- Commit locally and freely to that branch; never commit to main. No push is
  needed at this stage; GitHub write access is not a local development requirement.
- Do not create PRs or enqueue, land, merge, or deploy the branch.
- Record decisions and progress here. This document update does not begin
  implementation while the user is reviewing the plan.
- Sub-agents may translate or review independent work after interfaces are
  established. Preserve frontend-first component integration and keep the
  compiler itself sequential.

Retained rules, subject to the specific translation requirements above: useful file-purpose comments, bounded inspection,
warnings as errors, no fabricated defaults, explicit failure results, unchanged
test expectations, compile-time match validation, no shell calls hidden inside
Dark stdlib functions, and honest reporting of validation gaps.
Adapted rules: use Dune for OCaml builds and preserve the existing
build-before-test separation; use migration parity and Valgrind capability checks instead
of the existing merge-train/generated-program benchmark handoff.

## Development environment and current state

Observed directly on 2026-10-01: this session runs Linux x86-64 and has a
writable workspace at `/workspace/scratch/06af1dd4c39f`. Git is available.
OCaml, Dune, opam, .NET, and Valgrind were not found on PATH during inspection;
the development toolchain still needs setup. This is an observation about this
VM, not a claim that every ChatGPT cloud VM uses the same architecture.

The reference repository has been inspected through the GitHub connector;
a complete local Git checkout has not yet been established. A prior attempt
to create a remote branch received HTTP 403. That does not prevent local Git
branches and commits, and no remote write is currently requested.

The earlier requirements questions are answered. Toolchain installation,
baseline runs, and target-specific validation still require actual execution;
no implementation, test pass, or profiling success is claimed by this plan.

## Sources

- [Architecture](../compiler/overview.md)
- [Pipeline](../compiler/pipeline.md)
- [Source organization](../compiler/source-organization.md)
- [Test DSLs](../contributing/testing.md)
- [Existing verification policy](../contributing/verification.md)
- [Agent guidance](../../AGENTS.md)
- [Official OCaml releases](https://ocaml.org/releases)
- [Valgrind supported platforms](https://valgrind.org/)

The user's answers in this conversation define the port's acceptance contract.
Repository documents describe the reference implementation; their workflow
requirements do not override the user's task-specific instructions.


## Restart directives (2026-10-02)

The user authorized continuing to completion without clarification and pushing
every commit immediately to `codex/ocaml-port`. Never push main or land this
branch. These instructions replace the older no-push workflow above. Valgrind
support is deferred by the user.

The planning reference remains `27edbf054b400623f1856803fa0aef051f443cf8`.
Before restarting the translation, the user requested Docker and x64 E2E fixes.
The corrected, runnable F# oracle is frozen at
`df9dae7e1647275f6bc9104618f20ef84a7251be`: the x64 fixture identity allocation,
file-write leak accounting, erased list-pattern ownership, and portable
filesystem fixtures were fixed and tested before port implementation.
Those changes are explicit pre-port corrections, not migration accommodations.
Existing fixtures are read-only from this corrected oracle onward.

Oracle verification: 10,760/10,760 host tests and 58/58 compiled benchmark leak
checks passed. Generated-program performance gates do not apply to this port.
The VM has no Docker engine, Valgrind, or full x64 benchmark snapshot.

The VM now runs upstream stable OCaml 5.5.1, released 2026-09-05, and Dune
3.24.2. `ocaml/dependencies.lock` pins every installed library/build dependency.
Replay the VM setup with `scripts/vm/setup-port-toolchains`, followed by
`scripts/vm/setup-ocaml-dependencies`, both invoked with `bash` and an absolute
toolchain directory. Native builds must unset the CoreCLR compatibility preload.

Unicode compatibility is explicit: the frozen SDK's managed character tables
are Unicode 16, while invariant casing uses the VM's ICU 74 / Unicode 15.1.
Uucp/Uunf/Uuseg are pinned to 16.0.0. The host adapter retains the reference's
grapheme rules (without the newer Indic-conjunct rule), NFC rejection of U+FFFE,
UTF-16 indexing, and isolated surrogate units in diagnostic strings.

Completed native checkpoints: foundations, platform ABI selection, runner
arguments, complete tokens/trivia/lexer recovery, written syntax definitions
and normalization, validation, parser state/range/recovery helpers, match
patterns, binding patterns, source type parsing, typed parameters, effect rows,
and the complete parser (expressions, interpolations, declarations, module
scopes, assertions, post-syntax validation, and diagnostic rendering).
Source-name syntax and validated source-unit/module-scope handling are ported.
The complete semantic AST schema and its identity, constructor-tag, binder,
and collection helpers are ported. Compiler map ordering explicitly compares
UTF-16 units lexicographically; opaque function identities preserve uint64 order.
The native compiler entrypoint and remaining pipeline are not complete.

Current comparisons: 360 foundation observations, 10,016 binary64 formatting
observations, 1,713 complete tokenizer source observations, and 1,215 parser
support probes match F#. Complete match-pattern observations pass 1,239 probes;
source types and binding patterns each pass 1,262 probes, including ordered
diagnostics, literal widths, source ranges, generic closers, and recovery.
Typed parameter and effect-row observations each pass 1,281 probes, including
parameter documentation and malformed or unknown effect names.
Full parser observations match all 1,820 frozen corpus and probe sources.
Script/Package/Test validated results and rendered diagnostic text each match
all 1,820 sources, including complete trees and diagnostic ordering.
Complete source-name and source-scope observations each match all 1,858 sources,
including entry ownership, declaration flattening, and qualified name ordering.
AST helper observations (allocation, tags, binders, collisions, and identity
projections) match all 1,874 sources. Full AST and execution validation comparisons
also pass all 1,874 sources after the added ordering and binder probes.
Host text checks cover every Unicode scalar and every
UTF-16 unit, including NFC, classification, casing, and grapheme boundaries.
WrittenFormatter is complete, including the reference fingerprint, NFC acceptance,
and syntax-preserving parenthesis removal. All 1,874 complete formatter
observations match F#, including idempotence. Shared fixture parsing, syntax
fixture execution, and formatting-roundtrip execution are ported; DSL parser
observations match all 1,890 corpus/probe sources plus the frozen syntax files.
All 96 translated unit and fixture tests pass, including every focused ParserTests
case, all syntax fixtures, and all formatting-roundtrip fixtures.
Native executable bytes will be compared
after real backend translation; the F# oracle remains available until then.

`ocaml/inventory.json` records explicit filename mappings for F# namespaces
whose source files share a basename, preventing collisions in Dune's module
graph. These mappings do not change frozen source or fixture hashes.

Semantic resolution, checker diagnostics, closure free-variable analysis, and
sparse function identity maps are now ported. Resolver observations match all
1,890 corpus and probe inputs; diagnostic observations also match all 1,392 probes, including
literal widths, UTF-16 initial-character handling, runtime-failure detection,
and normalized fresh UUID identities. Free-variable observations match all
1,890 corpus and probe inputs, covering every expression/pattern case and three
lexical scopes. Function-table observations match all 1,890 inputs, including
uint64 boundaries, duplicate-key replacement, updates, folds, and overlays.
The full resolver corpus comparison passes.

Original source comments are retained alongside translated declarations.
`scripts/ocaml/port_comments.py --check` audits reference line-comment coverage
for every completed source owner, including the parser's split modules.
Large semantic comparisons now compare complete JSON observations as streams;
successful observations retain canonical SHA-256 audit rows, while a mismatch
retains both complete trees and the first differing field. This avoids exhausting
the VM's disk with repeated source evidence and does not weaken the comparison.

CheckedAST is fully translated with phase-safe private types and catalogs,
certified record completeness, explicit identity ordering, and all conversion
paths. Complete checked trees and private catalogs match all 1,890 corpus and
probe inputs; comparisons include every semantic type pair, literal widths,
rejected conversions, and 0/1/2/64/65-field record layouts. Instrumentation
appends typed observers to the exact production implementation in a separate
migration build; production catalogs remain opaque. Original comments are
preserved and audited against actual OCaml comment bodies.

CheckingTypes, TypeUnification, and the recursive expression-checking contract
are translated completely. Full type-resolution observations match 1,890 inputs,
including cyclic versus simultaneous substitution, every expression form, alias
arity and partial targets, nominal canonicalization, first-declared field indexes,
and exact legacy diagnostic text. Unification matches all 1,890 inputs across
every type pair, bidirectional reconciliation, inference conflicts, freshened
variables, ordered inferred arguments, and parameter-name diagnostics at int32
boundaries. Original comments remain audited; full semantic AST encoders retain
all typed evidence instead of reducing observations to pretty-printed types.

Comparison planning, equality expansion, ordering expansion, transitive helper
generation, and helper insertion are translated with complete interfaces and
original comments. Typed structural formatting preserves the F# layout used to
compute stable helper identities; its MIT provenance is recorded in the third
party notice. Structural formatting, comparison planning, and structural helper
expansion each match all 1,890 complete source observations. The build and all
96 currently translated unit tests pass; comment coverage has no missing lines.
Dependency graph and helper insertion parity remain in progress at this
checkpoint. Semantic comparisons now support restartable batches tied to an
exact corpus and source snapshot, retaining full row comparisons and audit hashes.

Declaration name resolution, deterministic recursive declaration grouping,
nominal declaration validation, registry summaries, and record literal checking
are completely translated. Interfaces precede implementations and original
comments are audited. Declaration observations cover every expression form,
recursive evidence, intrinsic visibility, alias cycles, duplicate declarations,
and full registries. Record observations include recursive checker call order,
source field order, generic inference, and legacy diagnostics. Initial full
observations pass; complete corpus comparisons remain in progress. Coverage is
46/391 source implementation/interface pairs, with 96/96 translated unit tests
passing. Restartable comparisons now copy both native executables and F# reference
scripts into immutable run snapshots so rebuilds do not interrupt comparisons.

Complete declaration resolution/validation, record literal checking, and helper
dependency generation each pass all 1,890 observations. Binary operation checking
also passes all 1,890 observations, including every operator, operand widths,
contextual retries, runtime errors, and recursive checker call order. The complete
intrinsic stdlib catalog passes all 1,890 observations: module ordering, 109 source
signature records, registration, exact lookup, and function types. Lambda and call
checking are fully translated with complete interfaces and preserved comments;
lambda initial observations pass, with full lambda and call comparisons pending.
The build and 96 translated unit tests pass. Coverage is 50/391 source components;
whole-compiler and full translated test acceptance remain outstanding.

Lambda checking and helper insertion pass all 1,890 complete observations.
Call checking passes all 1,890 observations, including builtins, declared and
intrinsic generics, partial applications, contextual propagation, legacy errors,
and ordered recursive checker requests. Generated inference identities are
validated as UUID v4 and compared under alpha renaming, preserving display names
and all shared/distinct identity relationships.

Match checking is fully ported and passes all 1,890 observations. Coverage includes
all pattern forms and literal widths, resolved-constructor reopening, first-pattern
bindings and alternative validation, guard normalization, body retries, generic
rechecking, and complete nested constructor/tuple/list exhaustiveness proofs.
Original comments remain audited. The full expression dispatcher is next.

Expression checking is fully ported, including context inference, explicit generic
calls, recursive lets, ordered literal checks, and every source-expression case.
Its complete typed results and diagnostics pass all 1,890 observations, including
the expanded empty-source expectation matrix. Function-body checking,
specialization, and ordered type-application collection also pass all 1,890.

The complete resolved-program checker and TypeChecking API now build. Initial
program-level parity passes 2/2 corpus rows, comparing entire checked programs,
type/function catalogs, checking registries, and all name-resolution indexes.
The full program-level corpus run remains pending at this checkpoint. Private
catalogs stay opaque in production: migration-only module aliases append typed
encoders to unchanged source. Snapshot identities now include C files, generation
scripts, Dune rules, and observation includes. Monotonic host timing replaces
Stopwatch for phase recording. Translated units remain 96/96; comment audit finds
zero missing reference comment lines; inventory coverage is 55/391 pairs.

The complete direct WrittenTypes checker now builds: contextual lambdas, builtin
and declared/indirect calls, constructors, records, ordered collections, guards,
exhaustiveness, continuation inference, recursive lets, declarations, recursive
groups, source batches, opaque retained environments, and later-pass registries.
All seven public entry points are implemented. Its migration observer compares
whole checked programs and retained private environments, including checking
against an earlier declaration batch. Initial complete corpus parity passes
450/1,890 rows; the empty-source 25-program matrix passes after correcting unknown
named-call dispatch. Full direct-checker and program-checker runs remain active.
The resolved-program checker has passed 900 rows at this checkpoint.

Written type-reference helpers pass all 1,890 observations. Pattern checking and
exhaustiveness have passed 300 rows, with their full run active. Specialization
identity and checked dependency collection are implemented; complete separate
specialization parity remains outstanding. The original type-checking fixture
parser, runner, and four tooling unit tests are translated. Native translated
units pass 100/100; inventory coverage is 61/391 complete implementation/interface
pairs, and the comment audit finds zero missing reference line comments.

Parity batches now freeze their Python driver alongside the native executable
and F# observer. Identical complete JSON wire rows are compared directly before
allocating parsed trees, retaining full-field comparison and identity validation
for other rows. Observation JSON permits ordinary Unicode escaping, with UTF-16
unit encodings retained for unpaired surrogates. Audit names permit independent
runs without overwriting earlier proof logs. These are migration tools only.

The direct WrittenTypes checker, written patterns/exhaustiveness, and checked
helper preparation now pass all 1,890 complete corpus observations. Checked
preparation also proves specialization identities, dependency summaries,
cross-catalog imports, and synthetic nullary normalization.

The complete MemoryModel and ANF data schemas and APIs are translated. Typed
migration encoders cover every record field and union constructor, checked
against frozen reflection case names and arities. Numeric boundaries, dense
type-table gaps/overlays, and wrapping 32-bit fresh identifiers are exercised.
All 1,890 ANF observations pass.

Preparation registries and memory planning each pass all 1,890 observations.
Registry comparisons cover aliases, sum case ordering, retained semantic
identities, borrowed list-head selection, and variable environments. Memory
planning comparisons cover recursive records/sums, transparent/nullable/spare
payload representations, root/storage classifications, and complete recursive
release plans. Interfaces remain complete and opaque where the reference is.

All 61 original type-checking fixture cases are now run as translated native
tests, bringing passing native units to 161/161. This checkpoint contains 66/391
complete source implementation/interface pairs, with zero missing reference
line comments. Whole-compiler executable parity and the remaining compiler
passes/backend/tests are still required; no final acceptance is claimed.

Lowering primitives, type substitution, lambda inlining, and both checked
expression diagnostic formats now pass all 1,890 complete corpus observations.
These comparisons include mangled type parsing, sum payload/layout selection,
intrinsic routing, simultaneous substitutions, lexical binding identities,
and private DU `ToString()` versus public `%A` output. Structural float output
also checks boundary values and randomized IEEE-754 bit patterns.

Closure analysis and closure comparison planning are translated with full
interfaces and source comments. Closure analysis observations cover free
variables, pattern binding types, branch reconciliation, complete lifted state,
name collisions/counter overflow, and return inference including invariant
failures. Their complete corpus proof and expression/function lifting remain
in progress. The native build passes 161/161 translated unit tests; coverage
is 71/391 complete pairs with zero missing reference line comments. This is a
migration checkpoint, not whole-compiler acceptance.

Expression-local lifting and whole-program wrapper generation are translated,
bringing coverage to 73/391 full pairs. Initial complete observations pass for
closure analysis, comparison planning, expression lifting, and function lifting
(2/2 each); full corpus audits are running. The lifted state, checked catalogs,
wrappers, comparisons, and invariant failures are included in comparisons.
Native units remain 161/161, with zero missing reference line comments.

Monomorphization and generic preparation entry points are translated, including
reachable specialization iteration, artifact catalog import, concrete and
polymorphic comparison/key intrinsics, empty dictionaries, registry errors,
and complete checked-expression rewrites. Initial complete observations pass
2/2, including all synthetic checked fixtures and intrinsic/type-argument
matrices. Full corpus proof remains required.

ANF lowering callbacks, representation-directed type inference, and structural
equality/operator lowering are translated with full interfaces and source
comments. The variable registry now aliases the checked binding map, matching
the shared F# Map type without changing ordering or keys. This build passes
161/161 native units; inventory coverage is 77/391 full pairs and the comment
audit reports zero missing reference lines. The new lowering passes' runtime
reference comparisons are next; this checkpoint is not final acceptance.

Closure analysis now passes all 1,890 complete corpus observations. The initial
representation-directed inference and operator proofs pass 2/2 observations
each, including all numeric/operator combinations and structural equality
layouts. The inference observer explicitly groups its conditional matrix
entries so every numeric case is included in the frozen F# comparison.

Aggregate lowering is translated with its complete interface and comments.
Its 2/2 initial observations compare skew-list forest/digit layouts at length
boundaries, all 34 semantic element types, retained binding prefixes, fresh
identifier overflow, tuple projections, duplicate bindings, complete variable
environments, incompatible patterns, and acceptance predicates. The clean
native build and 161/161 translated units pass; coverage is 78/391 complete
pairs with zero missing source comment lines. Whole-compiler acceptance and
remaining full corpus proofs are still outstanding.

Whole-program function lifting now passes all 1,890 complete corpus
observations. Atom lowering is fully translated, including source-local
legacy tree builders, and passes 2/2 initial complete observations. These
cover checked fixtures and source programs, full ANF atoms/binding prefixes,
recursive callback expressions/environments/order, wide integers, UTF-16
normalization, record field evaluation/layout order, sum representations,
closure application, concatenation, list construction, and identifier overflow.

HIR, OwnedIR, ListRegion, continuation substitution, value liveness, HIR and
ownership verification, list storage selection/elaboration/verification,
allocation accounting, native list-region lowering, call-graph reachability,
destruction contracts, and list-region extraction are fully translated.
Generic identity sets use OCaml functors with explicit comparators and typed
sets; function identity ordering remains unsigned. Source comments and
allocation/release distinctions remain intact, including the 256-byte runtime
small-array class. Full IR/reference proofs for these new passes remain due.

All original HIRVerificationTests (21) and OwnedHIRVerificationTests (5), plus
the complete TestIds fixture identity allocator, are translated and integrated
in the native runner. The clean warning-free build passes 187/187 translated
units. Coverage is 99/391 complete pairs with zero missing reference comment
lines. Closure-comparison full corpus proof is running; whole-compiler byte
parity and final replacement have not yet been accepted.

Uniqueness boundary inference, recursive-group inference, and deterministic owned
function SCC discovery are now complete native components. The twelve original
uniqueness and recursive inference tests pass, bringing the native suite to
199/199 and coverage to 104/391 complete pairs. The build is warning-free and
reference comment coverage has no gaps. Comment placement now recognizes generic
and functor-local declarations and retains each explanation in its owning
component, rather than matching an unrelated declaration with the same name.

The full closure-comparison audit matched its first 44 observations before its
process was killed while handling large wire rows. The audit now compares and
hashes the entire JSON in bounded chunks and spills rows to disk; differing rows
still undergo the complete structural comparison. Large Unicode rows, exact
hashes, late differences, successive rows, and EOF handling have been checked.
The full streamed audit is running; this is not yet a completed parity gate.

Whole-function ownership elaboration and owned-function-group inference are fully
translated, along with all original whole-function, grouping, and group-inference
units. The native suite passes 219/219 tests, including the 256-function graph and
ownership chains, branch-edge cleanup, live-result duplication, atomic recursive
convergence, missing contracts, and candidate tradeoffs. Coverage is 109/391
complete pairs; the warning-free build and comment audit pass.

The streamed full closure-comparison audit matched the same first 44 rows, then
the native observer stopped before Crypto.dark. Its JSON emitter now writes the
complete observation directly to its output channel, avoiding a second buffer
proportional to row size, and collects between requests. An isolated Crypto.dark
comparison is running. Full closure-comparison parity remains outstanding.

Ownership variant selection is complete, including canonical positional group
identities, stable preference ordering, exact borrowed-result sources, fallback
boundaries, and invalid-call diagnostics. All eleven original selection tests
pass. The native suite is now 230/230, coverage is 111/391 complete pairs, and the
warning-free build and comment checks pass.

The isolated Crypto.dark closure comparison now passes: its complete 750,240,244
byte JSON observation is identical between the frozen F# compiler and OCaml.
The full closure-comparison corpus audit has restarted with channel-based native
emission and bounded comparison; its completed gate is still pending.

Ownership variant materialization is complete. It clones whole verified recursive
candidates, preserves independent HIR contracts, validates source call sites and
boundaries, rejects symbol collisions, and re-verifies the resulting program.
All nineteen original materialization tests pass; the native suite is 249/249,
with 113/391 complete source pairs and no missing reference comment lines.
The clone-name SHA-256 helper passes 29 independent hashlib vectors covering
padding boundaries, binary input, a million-byte input, Unicode pairs, and
unpaired UTF-16 replacement. The warning-free build used a fresh absolute Dune
build directory because this VM's reused build directory missed new modules.
Batched parity audits now honor an explicitly supplied native executable when
creating their immutable snapshot. Full compiler and byte parity remain pending.

After the VM reset, the branch was cloned again and the pinned OCaml 5.5.1,
Dune dependencies, and .NET reference toolchains were rebuilt using the committed
setup scripts. The frozen F# host suite passes 10,760/10,760 tests. The code
recorded in the previous chat was replayed for ANF inlining utilities, ownership
variant scheduling and ANF materialization, ownership list-call fusion, HIR
construction, and the function ownership analysis coordinator. All components
have complete interfaces and preserved source comments.

The six original scheduling tests and ten original HIR construction tests are
translated and pass. A fresh native build compiled each recovered module;
265/265 translated native units and 29/29 independent SHA-256 vectors pass.
Coverage is 121/391 complete implementation/interface pairs, with zero missing
reference comment lines. The interrupted closure-comparison batch outputs were
lost with the VM; the full corpus gate must restart. The remaining compiler
passes and final executable-byte acceptance remain outstanding.

Typed ANF constant folding, scalar strength reduction, operand substitution,
effect/use tracking, function reachability, and tail-call detection are translated
with full interfaces and original comments. The release-plan fingerprint helper
now preserves the original UTF-16 hashing, compositional child order, and bounded
node traversal. All seven original tail-call tests pass, including cleanup motion,
closure calls, arity rejection, one-to-one owned transfer, and retained projections.
Native test diagnostics retain complete typed ANF descriptions.

A fresh warning-free build compiled the new production modules, translated tests,
and migration-only scalar observer; 272/272 native units pass. Coverage is 128/391
full source pairs and source-comment coverage has no missing lines. The observer
covers every ANF CExpr constructor with literal and temporary operands, scalar
boundary cases, option combinations, lexical renaming, SCCs, and reachability;
its reference comparison is pending while the restarted full closure-comparison
audit runs. Whole-compiler and executable-byte acceptance remain outstanding.

The scalar observer now matches the frozen F# reference across its complete
54,219,518-byte constructor and boundary-value observation. The lexical ANF
optimizer, intrinsic lowering, five accumulator transformations, optimization
coordinator, and accumulator-lowering coordinator are fully translated. Guarded
alternative patterns are split where OCaml and F# select alternatives differently.
All ten original ANF optimizer tests and all 27 original memory-shape tests pass,
including recursive release plans, fingerprint distinctions, and cache thresholds.

A fresh warning-free build passes 309/309 translated native units. Coverage is
135/391 complete implementation/interface pairs. The full closure-comparison
audit uses bounded batches, iterative reference requests, explicit collection,
and a sufficiently deep JSON encoder; its full corpus gate remains in progress.
Whole-compiler and executable-byte acceptance remain outstanding.

The shared escape-analysis destruction proof, list display lookup, and print
insertion pass are fully translated. The destruction proof preserves exact
recursive-cycle checks, type-argument substitution, and conservative rejection
of type-growing recursion. Print insertion preserves helper reachability,
branch/continuation allocation order, and both root-word probe modes. A fresh
warning-free build passes the existing 309 native units; coverage is now 138/391
complete pairs, with no missing reference comment lines. Complete output-planning
observations now match F# for five probe inputs. The fixed matrix checks primitive
and aggregate types, missing and invalid registry metadata, recursive and
type-growing record/sum cycles, both sum eligibility modes, descriptor field
proofs, full printed control flow and fresh identities, unsupported display
failures, duplicate/missing entry functions, and both root-word probe modes.

Pattern lowering, expression lowering, and the recursive expression/list-region
coordinator are fully translated, including typed projections, representation
checks before payload access, guarded alternatives, shared staged joins, and
the original allocation order. A fresh warning-free build passes all 309 existing
native units; coverage is 141/391 complete pairs. These newly translated handlers
still require direct lowering parity checks. The bounded closure-comparison
corpus run stopped after 250 matching inputs and remains incomplete.

The declaration-to-ANF conversion layer is fully translated, including complete
registry construction and overlay merging, recursive-member metadata, and
whole-function ownership scheduling/fusion/lowering. Its nested ownership
failure diagnostics retain complete structural layouts. Coverage is 142/391
complete pairs. The native build remains warning-free and passes 309/309 units.
The expression-lowering boundary matrix now matches F# for complete expression,
atom, and bound-atom results, including list/constructor guards and Int32 generator
overflow. All 1,392 expression-lowering probes now match the F# oracle. Declaration-
conversion parity and executable-byte acceptance remain outstanding.


Reference-count type facts, return analysis, shape planning, cleanup, expression
insertion, and function/program orchestration are fully translated. SSA ANF
construction preserves definition-site type recovery, sibling-definition
freshening, reserved backend IDs, and lexical joins. Coverage is 151/391 complete
implementation/interface pairs. The warning-free native build passes 321/321
translated units, including the original type-fact and join cleanup/interface
checks. A complete differential matrix matches F# for 31 type families × 28
expression/control-flow cases, two fresh-variable starting points (including
Int32 overflow), two function names, four ownership-contract parameter modes,
and both pre-RC and post-RC SSA construction. The matrix compares complete
expressions/functions, fresh IDs, type tables, SSA blocks, and diagnostics.
Full original RC test groups, SSA optimizers/elaboration, remaining backend
passes, native DSL/end-to-end integration, and executable-byte/Valgrind acceptance
remain outstanding; this checkpoint does not claim the port is complete.


SSA value liveness, SSA return-flow analysis, scalar replacement/block reuse,
ownership-safe tail-call detection, direct SSA RC insertion, and SSA simplification
are fully translated with their comments and source traversal/allocation order.
Coverage is 157/391 complete pairs. All 321 native units still pass. The differential
matrix also matches complete liveness/return facts, scalar replacement, tail calls
before and after RC elaboration, inserted ownership operations, and SSA optimization
with both default options and all optimization flags disabled. Direct-call and
higher-order specialization, SSA inlining, full original RC test groups, downstream
backend passes, and full native acceptance gates remain outstanding.


Direct-call facts, direct-call specialization, higher-order specialization, and
SSA inlining are fully translated with complete interfaces and original comments.
The original SSA fixture DSL and both fixture test groups are translated, and the
shared fixture identity table now includes all original names. A warning-free
native build passes 344/344 units, including all 23 original SSA inlining and
optimization fixtures. Coverage is 164/391 complete pairs. Full specialization
observations match F# for five probe inputs, covering literal/tuple clones,
indirect exposure, float-bit distinctions, captured/static/returned callbacks,
and generated-name collisions. Full-output inlining comparison, remaining RC
test groups, downstream backend passes, and final native acceptance remain
outstanding; the port is not yet complete.


The full-output SSA inlining matrix now matches F# across 672 combinations of
callee/control-flow shape, literal/variable arguments, local/external candidates,
exclusions, and seven option configurations. Complete CFGs and fresh type tables
are compared. MIR representation and scalar folding, parallel-move resolution,
MIR effect/purity analysis, direct-call graph scheduling, copy propagation,
root-reachable DCE, literal pools, and Mach-O/ELF container schemas are translated
with full interfaces and comments. The preparation wrapper was already fully
translated; its inventory owner mapping now points at that actual component.
Coverage is 175/391 pairs, with zero missing source comment lines. The warning-free
native build still passes 344/344 units. Complete MIR observations match F# for
all 66 instruction constructors with integer/float typing and register/literal
operands, 36,288 folding combinations, copy cycles/phi prefixes/typed sentinels,
DCE roots and dead phi cycles, scheduling with repeated IDs and recursion, purity
hazards, parallel moves, and UTF-16/string-byte and exact-float-bit literal pools.
MIR SSA and subsequent backend passes, full original RC/MIR test groups, complete
corpus checks, and final executable-byte/Valgrind acceptance remain outstanding.


MIR SSA construction is fully translated, including predecessor/dominator/frontier
analysis, bitset liveness, typed phi insertion, deferred phi updates, renaming,
float-register tracking, fresh-ID overflow, contextual diagnostics, and all six
timing phases. The five original SSA construction tests and their complete MIR
failure formatting are translated. SSA verification, natural-loop topology, and
all three CFG simplifications are also translated with full interfaces and
comments. Coverage is 180/391 pairs. The warning-free build passes 349/349 native
units. Complete differential observations match F# across 60 typed CFGs, including
unreachable blocks, duplicate edges, typed joins, loops, missing targets/entry,
invalid definitions and phi sources, chained merges, empty-block cycles, and
return-phi joins. The comparison includes complete per-phase SSA results, verifier
errors, loop sets, and transformed CFGs, rather than only final return values.
Further MIR optimizers, original RC/MIR test groups, backend passes, full corpus
checks, and final executable-byte/Valgrind acceptance remain outstanding.


Common-expression elimination and partial redundancy elimination are fully
translated, including exact structural keys, UTF-16 operand ordering, unsigned
function IDs, dominated-block availability, unreachable-block local CSE, safe
join-edge insertion, ownership and memory barriers, and bounded effect-free call
reuse. Coverage is 181/391 pairs. The warning-free build passes 349/349 native
units. Complete output matches F# for 1,800 normalized operand/key combinations
and 7,548 CFG/configuration combinations, each optimized twice. The matrix covers
all 66 instruction barriers, 17 scalar/managed/aggregate type families, local and
dominated reuse, path-complete joins, critical edges, join-local dependencies,
and unreachable paths. Remaining loop/SCCP passes, original RC/MIR fixture groups,
backend stages, full corpus checks, and final native acceptance are outstanding.


Induction strength reduction, counted-loop unrolling, and loop-invariant motion
are fully translated with complete interfaces, original line and block comments,
Int32 fresh-ID arithmetic, original discovery/clone order, canonical preheader
construction, and effect-free call rules. This checkpoint contains 184/391 pairs.
The warning-free build passes 349/349 native units. Complete differential output
matches F# for 1,530 typed loop shapes: three scale forms, three offset forms, 17
type families, and ten control-flow/dependency variants. It compares all three
passes, fresh IDs, and LICM with both empty and known effect-free function sets,
including the returned loop topology. Multiple entry edges, invariant phis,
extra scale uses, non-invariant bounds, non-unit steps, float instructions,
generated-label collisions, and fresh-ID overflow are included. SCCP and the
fixed-point optimizer scheduler, original RC/MIR tests, downstream backend stages,
full corpus checks, and final native acceptance remain outstanding.


SCCP and the full MIR optimization scheduler are translated with complete
interfaces and source comments. The lattice preserves exact float bits, typed
integer ranges, bounded aggregate identities and fields, derived Boolean and
comparison facts on edges, immutable worklist order, copy-aware rewrites, and
contextual missing-block errors. The scheduler preserves pass order, topology
reuse/invalidation, ten-iteration limit, effect analysis, constant-call-result
propagation, float-register bookkeeping, and traced phase order. Coverage is
186/391 pairs. The warning-free build passes 349/349 units. Full output matches
F# for 13,386 SCCP/simplification runs across 4,462 CFGs, including all 66 MIR
constructors, 19 type families, nested Boolean/range branches, 16/17-allocation
aggregate merges, signed zero and NaN payloads, IEEE arithmetic/conversions,
straight-line bypasses, and diagnostics. The scheduler additionally matches
64 CFG/options combinations covering all 16 flag combinations, complete
functions/programs, constant returns, and traced phase labels/nonnegative times.
Original MIR optimizer tests, printers, remaining RC groups, backend stages,
full corpus checks, and final executable-byte/Valgrind acceptance remain open.

The shared IR printing helpers and complete ANF/MIR printers are translated,
including original comments, every constructor, source diagnostics, structural
type formatting, float formatting, and .NET ordinal Unicode filtering. Complete
output matches the F# reference for 792 MIR programs, the full ANF constructor
fixture, summaries and external function names, all 1,470 pinned-runtime ordinal
casing mappings, and partial-surrogate searches. All 43 original MIR optimizer
tests are translated and registered with their original assertions and failure
messages. The warning-free build passes 392/392 native units. Coverage is now
190/391 pairs. Remaining ownership test groups, LIR and downstream backend
stages, full corpus checks, and final native acceptance remain outstanding.

Symbolic LIR is fully translated: its 120 instruction constructors, register
and operand types, carried backend helper requirements, deterministic block
layout, compact code-generation facts, RC memo keys, and complete printer.
Ordered structural release plans use explicit source-compatible union/type
comparisons; labels and fingerprint strings retain UTF-16 ordering. Printer
collection interpolation retains runtime tuple/option/list formatting,
including three-element truncation, raw tuple strings, and empty None text.
Complete output matches F# for every constructor and reflected case schema,
38 layouts/diagnostics, 3,144 fact/attachment cases, 66 metadata values and
their ordered memo-key set, whole-program attachment/coverage counting, and
924 printer/filter/summary cases. Original layout tests are translated with
their comments and assertions. The warning-free build passes 396/396 units;
coverage is 193/391 pairs. LIR tree shaking, allocation/lowering/backend stages,
remaining ownership groups, full corpus checks, and final acceptance remain open.

LIR dead-code elimination, user/stdlib function tree shaking, and target
register policy are fully translated with original comments and interfaces.
Complete source output matches for 1,360 instruction/helper/policy cases:
every instruction with five operand forms, all list-display type families,
both allocated and missing helper identities, and ARM64/x64 register orders.
Six call-graph shapes additionally cover recursive SCCs, unknown targets,
unsigned maximum identities, duplicate entry names, incomplete precomputed
graphs, all filtering entry modes, and ANF-derived stdlib roots. The clean
build passes 396/396 units. Coverage is 196/391 pairs; allocator models and
algorithms, lowering/backend emission, remaining ownership tests, full corpus
checks, and final native acceptance remain outstanding.

Allocator domains, register facts, combined integer/float liveness, caller-save
preparation, and interference construction are fully translated with complete
interfaces and original comments. Complete output matches F# for 15,120
instruction/register/operand/type combinations, all terminators, sparse and
multiword domains, union accumulator ownership, graph construction and coloring
queries, 252 CFG/entry-definition combinations, phi edges, loops, missing labels,
and nested/unmatched save/restore pairs. Empty-array physical identity is excluded
from the migration observation because OCaml represents empty arrays with a
shared atom; mutable-word aliasing and all accumulator cases are compared.
The warning-free native build passes 396/396 units, both prior LIR observations
still match, and the rebuilt F# host suite passes 10,760/10,760 tests. Coverage is
200/391 pairs. Coalescing/coloring, remaining allocation, lowering/emission,
remaining ownership tests, full corpus checks, and final acceptance remain open.

Register coalescing and graph coloring are fully translated with interfaces and
original comments. The full 235,861,843-byte observation matches F# exactly:
720 instruction collectors, copy chains, all 64 undirected graph shapes on four
vertices, empty/inactive domains, 65/129-vertex multiword graphs, 12,600 complete
coalescing/coloring/allocation combinations, MCS orders and profiles, synthetic
color expansion, explicit greedy orders, conflicting and missing precolors,
integer overflow, missing-vertex diagnostics, and timing phase invariants.
Stack slots, alignment and callee-save ordering match. The warning-free build
passes 396/396 native units; coverage is 202/391 pairs. Float allocation,
remaining allocation/lowering/emission, remaining ownership tests, full corpus
checks and final acceptance remain open.

Register-allocation orchestration and all 10 original phi/caller-save tests are
fully translated with interfaces and original comments. Complete output matches
F# for 4,392 public allocations across ARM64/x64, all instruction constructors,
mixed and invalid parameters, call-summary configurations, phi/loop graphs,
codegen facts and integer/float pressure through 65 values; another 1,098 timed
runs match function output and phase order with valid durations. The private
call-aware paths match 2,880 GP and 2,160 FP register permutations, including
save envelopes, missing allocations and clobber costs. Int32 cost-gap overflow
and exact failure diagnostics are preserved. The warning-free build passes
447/447 native unit/DSL checks. Coverage is 213/391 pairs. ANF-to-MIR and
MIR-to-LIR lowering are next; emission, remaining tooling/ownership tests,
full corpus checks and final acceptance remain open.

Float allocation is fully translated, including target register lists, literal
load scheduling, phi/copy coalescing, spill-slot reuse, rematerialization, exact
float bits, scratch selection, argument move cycles, and every instruction/block/
CFG repair path. Complete output matches F# for 19,200 constructor repairs across
physical, spilled, rematerialized, missing, fixed and physical float registers,
11,700 allocation combinations over 130 CFGs, both target register capacities,
three initial stack sizes, phi parameter precolors, 33/65-value register pressure,
and scheduling with duplicate loads and edge-only uses. The warning-free build
passes 396/396 native units. Coverage is 203/391 pairs. Integer spill helpers,
phi resolution, remaining allocation/lowering/emission, ownership tests, full
corpus checks and final acceptance remain open.

Integer spill operands and caller-save selection are fully translated with
interfaces and original comments. Complete output matches F# for 46,080
two-operand spill repairs across both targets, all physical registers, five
allocation families, missing and overflowed virtual IDs, every integer live
mask, every float caller-save register, and all 2,048 preserved-temporary
exclusion subsets. x64 R11 aliases, temporary order, and failure diagnostics
match exactly. The warning-free build passes 396/396 native units. Committed
coverage is 204/391 pairs; phi resolution and subsequent stages remain open.

Phi resolution is fully translated with original comments and complete interfaces.
Complete output matches F# for 540 float location/move cases, 800 CFG/allocation
combinations, and 2/65/129-value transitive phi chains. Coverage includes register,
stack and mixed cycles, rematerialized values, fixed/missing locations, unused
integer phis, physical phi roots, missing predecessor labels, all operand families,
tail-call predecessors, move order and failure order. The warning-free build
passes 396/396 native units. Coverage is 205/391 pairs. Instruction/block allocation
application, allocation orchestration, lowering/emission, remaining ownership
tests, full corpus checks and final acceptance remain open.

The complete instruction allocation pass is translated with all original cases,
interfaces and comments. Full output matches F# for 181,248 instruction rewrites:
all 120 constructors, all 256 destination/source allocation class combinations,
ARM64/x64, physical and R11-alias register roles, missing/overflowed IDs, six
operand families, integer/float types, ternary spill repair, variadic/binary
concat, distinct buffer-comparison operands, four-argument native calls, and
argument/tail-argument moves. The warning-free build passes 396/396 native units.
Coverage is 206/391 pairs. Block/caller-save allocation, orchestration and callee
clobber summaries, lowering/emission, remaining ownership tests, full corpus
checks and final acceptance remain open.

Block allocation is fully translated with original comments and complete interfaces.
Complete output matches F# for 23,616 block rewrites, 168 terminator rewrites,
48 CFG preparation/allocation combinations, and 40 precomputed save-snapshot
combinations. Coverage includes both targets, nested caller saves, all instruction
constructors, integer/float spills and rematerialization, missing allocations,
branch loads, truncated float liveness, and exact invalid-save diagnostics.
The warning-free build passes 396/396 native units. Coverage is 207/391 pairs.
Callee-write summaries and allocator orchestration are next; lowering/emission,
remaining ownership tests, full corpus checks and final acceptance remain open.

ARM64 and x64 callee-write summaries are fully translated with original comments
and interfaces. Complete output matches F# for 138,456 instruction summaries,
21 function catalogs (including duplicate IDs, mutual recursion, unresolved
callees and a 65-function propagation chain), 27,648 save-envelope/pruning
combinations, and four cache/known-summary configurations. Coverage includes
all physical destinations, virtual/return shuttles, immediate/offset boundaries,
argument setup, every GP/FP save subset, unmatched restores, full clobbers, x64
stack parity and relevant cache keys. The warning-free build passes 396/396
native units. Coverage is 209/391 pairs. Allocator orchestration also requires
the remaining LIR peephole pass, which is next; lowering/emission, ownership
tests, full corpus checks and final acceptance remain open.

The entire LIR peephole pass and all 30 original cleanup unit tests are ported
with original comments and explicit interfaces. Full output matches F# for all
120 constructors, 10,272 cleanup/arithmetic sequences, 1,920 branch-fold cases,
5,145 CFGs, 4,896 scalar/invalid diamonds, 2/65/129-block loop graphs, malformed
CFG diagnostics, and 324 numeric cases including every positive Int64 power
of two. All 11 unchanged lir-peepholes.liropt inputs/expectations also match
through typed fixture projections; the complete native DSL parser/runner remains
a separate pending inventory component. Float rounding policies, live-temp
restrictions, copy identities, last-use counts, hoisting order, select types and
iteration limits are preserved. The warning-free build passes 437/437 native
unit/DSL checks. Coverage is 211/391 pairs. Register-allocation orchestration
is next; lowering/emission, remaining tooling/ownership tests, full corpus
checks and final acceptance remain open.

ANF-to-MIR lowering and all nine original pass-local tests are fully translated
with explicit interfaces and original comments. Full observations match F# for
all 74 ANF operation constructors across 21 types and seven atom families,
21,756 expression lowerings, 10,584 additional binary/unary/CLI lowerings,
294 nested/terminal/join/self-tail expressions with coverage and SSA conversion,
16,384 ownership-transfer combinations, missing/refined type environments,
84 explicit SSA graph variants, canonical and inconsistent variant registries,
and 1,680 whole-program/functions-only/trace calls. Float phi materialization,
entry retains, caller metadata, tail-call captures, lexical scope, failure order,
registry projection and trace phase semantics match. The warning-free build
passes 456/456 native unit/DSL checks. Coverage is 215/391 pairs. MIR-to-LIR
instruction selection is next; emission, remaining tooling/ownership tests,
full corpus checks and final acceptance remain open.

MIR-to-LIR instruction selection is fully translated with explicit interfaces
and all original comments. Full observations match F# across both targets for
76,560 instruction selections covering every MIR constructor, 32,076 arithmetic
selections, temporary-counter overflow, type substitutions, all terminators,
110 CFG selections, division/modulo guards and phi predecessor remapping,
2,040 whole-program/functions-only/trace calls, mixed argument register banks,
250 float-argument selections and 100 nested/error printing cases. Exact error
diagnostics and trace phase semantics are preserved. The warning-free build
passes 456/456 native unit/DSL checks. Coverage is 216/391 pairs. Machine backend
types and emission are next; remaining tooling/ownership tests, full corpus
checks and final acceptance remain open.

Both machine instruction type modules and ARM64 symbolic instruction conversion
are complete with explicit interfaces and original comments. Full observations
match F# for all 94 ARM64 and 68 x64 constructors: 216,576 concrete ARM64
instructions with both individual/list symbolic conversions, 78,336 x64
instructions, all GP/FP registers, conditions and extensions, integer boundaries
and six label families. All 256 encodable ARM64 float immediates and adjacent
IEEE values (775 cases total), validated platform/syscall configurations and
x64 literal-label round trips also match. The warning-free build passes
456/456 native checks. Coverage is 219/391 pairs. Machine encoding is next;
remaining backend/tooling/ownership tests, full corpus checks and final
acceptance remain open.

The entire x64 instruction encoder is translated with its explicit interface
and original comments. Full typed instructions, bytes and diagnostics match F#
for 261,120 selections spanning all 68 constructors and every register pair,
294,912 indexed-address selections spanning all destination/base/index registers,
eight scale choices and displacement boundaries, 128 full-width immediates,
9,216 condition selections and 1,024 mixed GP/FP conversions. REX ordering,
SIB fields, compact displacement/immediate choices, invalid index/scale errors
and unresolved branch templates are preserved. The warning-free build passes
456/456 native checks. Coverage is 220/391 pairs. ARM64 encoding and backend
resolution/emission are next; remaining tooling/ownership tests, full corpus
checks and final acceptance remain open.

Complete ARM64 encoding and symbolic literal collection are translated with
explicit interfaces and original comments. Full observations match F# for
87,232 concrete instructions through word/list/prepared-chunk entrypoints,
12,096 rotated logical masks, 1,048 float-immediate selections, all 32 GP/FP
register encodings, 616 label resolutions including offset overflow and missing
labels, 11 chunk groups across pool/platform/leak configurations, six concrete
streams, and 200 leak-counter placements. Local/cross-chunk fixups, duplicate
labels, first-use literal order, signed zero/NaN bits, unresolved templates and
exact diagnostics match. The warning-free build passes 456/456 native checks.
Coverage is 222/391 committed pairs. x64 resolution verification and original
backend tests are next; remaining emission/tooling/ownership tests, full corpus
checks and final acceptance remain open.

Complete x64 label resolution and deferred data patching are translated with
explicit interfaces and original comments. Full records, bytes and diagnostics
match F# for 50 instruction streams (forward/backward branches, every fixup
kind, duplicate/undefined labels, repeated references and invalid instruction
ordering), deferred patches including partial mutation on errors, 120 data
layouts with 600 required-label queries, and 44 signed rel32 byte patches.
First-use string collection, the canonical empty buffer, fixup order, alias
labels, integer overflow and counter alignment are preserved. The warning-free
build passes 456/456 native checks. Coverage is 223/391 pairs. Original ARM64
encoding tests and their DSL parser are next; remaining emission/tooling/
ownership tests, full corpus checks and final acceptance remain open.

The complete ARM64 instruction/encoding fixture parsers and all 14 original
encoding unit tests are translated with explicit interfaces and original
comments. Full parsing observations match F# for 1,412 instruction inputs
(8,472 parses across single/program/error modes), register/condition validity,
Unicode whitespace/digits, lazy regex captures, immediate/offset overflow, hex
parsing, error sections and ASSERT-DIFFERENT handling. All 15 unchanged
.arm64enc fixtures parse identically and execute successfully through the
native encoder. The full pass-test runner remains a separate pending inventory
component. The warning-free build passes 485/485 native unit/DSL checks.
Coverage is 226/391 pairs. Whole ELF images and original binary tests are next;
remaining emission/runtime/tooling/ownership tests, full corpus checks and
final acceptance remain open.
