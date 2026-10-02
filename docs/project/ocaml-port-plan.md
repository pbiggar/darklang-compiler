# Faithful OCaml compiler replacement

Purpose: specify the accepted migration requirements, comparison strategy,
delivery sequence, and task-specific workflow.

Status: revised following user review, 2026-10-01. No OCaml implementation has started.

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
All 95 translated unit and fixture tests pass, including every focused ParserTests
case, all syntax fixtures, and all formatting-roundtrip fixtures.
Native executable bytes will be compared
after real backend translation; the F# oracle remains available until then.

`ocaml/inventory.json` records explicit filename mappings for F# namespaces
whose source files share a basename, preventing collisions in Dune's module
graph. These mappings do not change frozen source or fixture hashes.
