# Traits — implementation checkpoint

Compiler baseline: `d8e2d7161ecf6892079906cdc5d797d6aa87e7fb`.
Interpreter reference: darklang/dark `a9d93efc853486244630dad693fe79f6f4882cf6`.
Reference grammar: `backend/src/LibParser/{Parser,WrittenTypes}.fs`.
Reference semantics: `backend/src/LibExecution/Traits.fs` and
`packages/darklang/stdlib/traits.dark` at that revision.

This is an unfinished implementation checkpoint. M0–M2 are implemented and
focused checks pass. M3–M7 remain unfinished; this is not a readiness claim.

| Behavior | Checkpoint implementation | Executed evidence / remaining validation |
| --- | --- | --- |
| Trait and impl grammar, method aliases, multiple methods | Range-bearing syntax and source-unit flattening | `test/fixtures/syntax/traits.syntax` |
| Single, multiple, parameterized and type-declaration bounds | Parsed and retained; checking rejects unsupported bounds explicitly | `test/fixtures/syntax/traits.syntax` |
| Concrete named method calls | Select an implementation and emit ordinary checked function calls | `test/fixtures/e2e/traits.e2e` |
| Method aliases | Validate against the referenced ordinary function signature | `test/fixtures/e2e/traits-alias.e2e` |
| Unknown trait, missing impl, incomplete method set, wrong signature | Frontend compile errors | `traits-{unknown,missing-impl,method-set,wrong-method}.e2e` |
| Competing identical implementations | Compile-time ambiguity when selecting a call | `traits-ambiguous.e2e` |
| Traits as value types | Traits are excluded from the nominal type catalog | `test/fixtures/e2e/traits-as-types.e2e` passed |
| Bounded generic specialization and conditional impls | Not implemented (M3–M4) | Required |
| Receivers and operators | Not implemented (M5) | Required |
| Upstream stdlib traits and hosted packages | Not implemented (M6) | Required |
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

The representative work-order fixture uses `Stdlib.Int64.toString` to render
its `Int64` field: the current compiler does not define `Stdlib.toString`.
Activating the upstream generic `ToString` surface belongs to M6.
