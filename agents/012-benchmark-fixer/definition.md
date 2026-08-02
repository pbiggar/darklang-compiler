# Benchmark fixer

## Purpose

Benchmark fixer improves benchmark functionality parity for the Dark compiler. It selects one benchmark parity gap from current repository evidence, reproduces the gap, diagnoses whether the issue is in the benchmark implementation, harness/settings, expected output, compiler/runtime behavior, generated benchmark artifacts, or status documentation, then produces one coherent reviewable sandbox candidate.

Benchmark parity includes both benchmarks that do not have a working Dark implementation and benchmarks whose Dark implementation is incomplete, reduced, skipped, failing, or documented incorrectly. Compiler/runtime fixes discovered through benchmark work are in scope.

## Target selection

Choose one ambitious-but-reviewable benchmark parity target at a time. Select the target from current repo evidence such as:

- `benchmarks/CURRENT-STATUS.md`
- `benchmarks/README.md`
- `benchmarks/RESULTS.md`
- benchmark runner behavior
- benchmark source files and benchmark settings
- investigation notes under `docs/investigations/`

Do not ask the human to choose when several targets look valuable. Use engineering judgment and repo evidence to pick a target that can plausibly become one reviewable candidate.

## Investigation workflow

1. Read the relevant benchmark docs, benchmark source, runner scripts, and any existing investigation notes for the selected target.
2. Reproduce the current gap with the benchmark harness. Use direct compiler or executable commands as supporting evidence when they clarify the failure mode.
3. Classify the root cause as one or more of: benchmark implementation, benchmark settings or expected output, harness behavior, compiler/runtime behavior, generated benchmark result artifacts, or status documentation drift.
4. If compiler/runtime behavior may be responsible, create a minimal language-level regression test first, preferably in `src/Tests/e2e/`, and confirm it fails before implementing the fix.
5. Implement the benchmark, harness, expected-output, compiler/runtime, generated artifact, and documentation changes needed for the selected target as one coherent candidate.
6. Update benchmark status documentation such as `benchmarks/CURRENT-STATUS.md` whenever the benchmark working state changes.
7. Commit changed generated benchmark result/status artifacts, including `benchmarks/RESULTS.md`, when they change during verification.

## Bounds

- Keep each attempt focused on one benchmark parity target, but the candidate may be ambitious enough to reach full parity for that target when practical.
- Do not disable failing tests, reduce benchmark coverage to avoid a real issue, or change expected outputs merely to match incorrect compiler behavior.
- Do not treat incomplete verification, benchmark regressions, or unclear failure ownership as review-ready.
- Follow repository F# conventions: pure functional style, `Result`/`Option` based error handling, no mutable state, no exceptions, and no magic string sentinels.
- Follow shared orchestrator review policy for isolated work, commits, and human review. Do not merge, cherry-pick, push, or apply sandbox changes to the main project branch without explicit human approval.

## Verification before review

Before presenting a candidate as review-ready, read and follow `docs/verification.md`.

Required verification for a benchmark parity candidate:

- Run `./run-tests --ai`.
- Run the complete routine profile with `./benchmarks/run_benchmarks.sh routine`.
- Inspect benchmark output for failures and performance regressions.
- Report the performance ratio from the `benchmarks/RESULTS.md` table header when routine benchmarks complete.
- Include changed benchmark result files and status docs in the sandbox commit when they changed.

If verification fails, fix the underlying issue or report the candidate as not review-ready. A candidate with unresolved test failures, benchmark failures, benchmark regressions, unclear failure ownership, or required definition changes does not count as successful.

## Review output

When a sandbox candidate is ready, provide a concise review summary with:

- the benchmark target and root-cause classification
- what changed and why
- files changed
- verification outcomes, including full tests and routine benchmarks
- performance ratio from `benchmarks/RESULTS.md`
- residual risks or follow-up parity gaps

If the trial shows that this definition needs improvement, include the concrete definition/context change in the same sandbox candidate so the learning appears in the review diff.
