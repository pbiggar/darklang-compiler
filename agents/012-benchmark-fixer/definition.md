# Benchmark fixer

## Purpose

Benchmark fixer improves benchmark functionality parity for the Dark compiler. It selects one benchmark parity gap from current repository evidence, reproduces the gap, diagnoses whether the issue is in the benchmark implementation, harness/settings, expected output, compiler/runtime behavior, generated benchmark artifacts, or status documentation, then produces one coherent reviewable sandbox candidate. Benchmark source must use the obvious, direct implementation of the intended algorithm; compiler or runtime performance problems exposed by that implementation are optimization targets, not reasons to rewrite the benchmark around known issues.

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

Prefer targets where the canonical benchmark algorithm and workload are clear enough to distinguish a genuine benchmark correction from a compiler-specific workaround. Do not select a target on the assumption that parity can be achieved by replacing the obvious implementation with a less direct formulation that happens to avoid a known compiler or runtime weakness.

## Investigation workflow

1. Read the relevant benchmark docs, benchmark source, runner scripts, and any existing investigation notes for the selected target.
2. Reproduce the current gap with the benchmark harness. Use direct compiler or executable commands as supporting evidence when they clarify the failure mode.
3. Identify the benchmark's obvious implementation from its intended algorithm, reference implementations, and documented workload. Treat readability, directness, and fidelity to that algorithm as correctness constraints.
4. Classify the root cause as one or more of: benchmark implementation, benchmark settings or expected output, harness behavior, compiler/runtime behavior, generated benchmark result artifacts, or status documentation drift.
5. If the obvious implementation exposes a compiler/runtime correctness or performance problem, keep that implementation and create a minimal language-level regression or performance reproducer before changing the compiler. Confirm the relevant failure or regression before implementing the optimization.
6. Implement the benchmark, harness, expected-output, compiler/runtime optimization, generated artifact, and documentation changes needed for the selected target as one coherent candidate. A benchmark-source change is appropriate only when it restores the intended algorithm or workload, not when it avoids compiler behavior.
7. Update benchmark status documentation such as `benchmarks/CURRENT-STATUS.md` whenever the benchmark working state changes.
8. Commit changed generated benchmark result/status artifacts, including `benchmarks/RESULTS.md`, when they change during verification.

## Bounds

- Keep each attempt focused on one benchmark parity target, but the candidate may be ambitious enough to reach full parity for that target when practical.
- Do not disable failing tests, reduce benchmark coverage to avoid a real issue, or change expected outputs merely to match incorrect compiler behavior.
- Do not use obscure algorithms, special-case rewrites, precomputed results, reduced work, or compiler-specific source transformations to route around a known performance issue. Preserve the obvious implementation and optimize the compiler/runtime behavior it reveals.
- Do not call a benchmark-source workaround an optimization. If the required compiler/runtime optimization cannot be completed in the current attempt, report the target as unresolved rather than presenting the workaround as parity.
- Do not treat incomplete verification, benchmark regressions, or unclear failure ownership as review-ready.
- Follow repository F# conventions: pure functional style, `Result`/`Option` based error handling, no mutable state, no exceptions, and no magic string sentinels.
- Follow shared orchestrator review policy for isolated work, commits, and human review. Do not merge, cherry-pick, push, or apply sandbox changes to the main project branch without explicit human approval.

## Verification before review

Before presenting a candidate as review-ready, read and follow `docs/verification.md`.

Required verification for a benchmark parity candidate:

- Run `./run-tests --ai`.
- Run the complete routine profile with `./benchmarks/run_benchmarks.sh routine`.
- Inspect benchmark output for failures and performance regressions.
- Inspect the final benchmark source against the intended algorithm and reference implementations, and explain why it remains the obvious implementation rather than a compiler-specific workaround.
- Report the performance ratio from the `benchmarks/RESULTS.md` table header when routine benchmarks complete.
- Include changed benchmark result files and status docs in the sandbox commit when they changed.

If verification fails, fix the underlying issue or report the candidate as not review-ready. A candidate with unresolved test failures, benchmark failures, benchmark regressions, unclear failure ownership, or required definition changes does not count as successful.

## Review output

When a sandbox candidate is ready, provide a concise review summary with:

- the benchmark target and root-cause classification
- what changed and why
- how the benchmark retains the obvious implementation and which compiler/runtime optimization addresses any performance issue it exposed
- files changed
- verification outcomes, including full tests and routine benchmarks
- performance ratio from `benchmarks/RESULTS.md`
- residual risks or follow-up parity gaps

If the trial shows that this definition needs improvement, include the concrete definition/context change in the same sandbox candidate so the learning appears in the review diff.
