# Verification Policy

This policy applies to all agents when they verify a proposed commit, change, fix, workflow update, or integration step.

Verification means both, for the active development target:

- All tests pass.
- The full benchmark suite passes its aggregate and individual regression gates.

The merge train rejects candidates that change anything under
`benchmarks/problems/` unless a human approves an exact `benchmark-sources`
gate exception. Benchmark problem implementations and their vendored build
inputs are integration-controlled; ordinary task branches may change benchmark
infrastructure, profiles, and generated results.

The active target is the host unless the work explicitly declares another
target. For ordinary host-target compiler changes, the default verification
commands are:

```bash
./build --ai
./run-tests --ai
python3 scripts/check_compiled_leaks.py
./benchmarks/run_benchmarks.sh --verify-parent full
```

Full verification keeps terminal output concise so automated callers do not
consume context on repeated per-workload details. Full build and measurement
logs, the markdown report, and decision JSON remain in the reported results
directory. Use `--verbose` when interactive diagnosis needs streamed details.

The compiled leak check builds the canonical Dark benchmarks with leak
instrumentation and runs their quick and full workloads. It requires the
expected stdout, a zero exit code, and empty stderr for each run. The merge
train blocks a failing `leaks` gate unless a human approves an exception for
the exact candidate.

The merge train runs `./run-tests --ai` to check the complete already-built
host suite. Full-suite timing remains useful for diagnosis, but it is not a
blocking merge-train gate because contention can invalidate a comparison.

The benchmark command compares the retained measurements with the snapshot
stored by the task branch's upstream merge-base and reports the aggregate
`current/parent` ratio. Its default parent is
`git merge-base HEAD @{upstream}`. The snapshot is the parent's canonical
performance state, which integration keeps current by rejecting blocking
regressions and unrecorded improvements. If the workload contract changed, the
command fails instead of comparing incompatible measurements.

`./benchmarks/run_benchmarks.sh --verify full` instead compares with the
best-known compatible canonical snapshot. It is not the task-readiness gate and
agents must not report its ratio as the task branch's performance result.

The E2E runner compiles up to 8192 compatible value-equality checks together by
default, enough for every compatible contiguous group in the current corpus.
Each check remains a separately compiled function while the caller and
executable are shared. Use `--e2e-batch-size=1` for a singular diagnostic
baseline, or another value through 8192 for batch-size experiments. Timing JSON
records the configured size, logical/eligible test counts, physical executions,
batch count, batched logical tests, and largest observed batch so batch-size
comparisons do not confuse logical coverage with compiler invocations.

Agents may run narrower checks while developing a change, but a change is not verified until the full verification policy has passed or the agent explicitly reports why full verification could not be completed.

`./build --ai` bounds build diagnostics and retains a complete failed log under
`TestResults/ai/`. `./run-tests --ai` performs no build; it bounds test failure
summaries and retains complete failed test output in the same directory. Search
those artifacts for the relevant diagnostic instead of reading them in full.

Target support is intentionally allowed to advance independently. A feature
developed for one target may land after that target's applicable tests and
benchmarks pass; an architecture outside the declared scope is not an
integration blocker. Cross-target parity work must name every target in scope
and is a dated audit of those targets at that revision, not a permanent
requirement that future changes validate every architecture.

Calling two targets "at parity" never widens the default verification scope.
Each later change validates the target being developed; it validates another
architecture only when that architecture is explicitly part of the change.

On an ARM64 host, Linux x86_64 tests are explicit and execute generated ELF
binaries through the pinned QEMU installation:

```bash
./build --ai
./run-tests --ai --target=linux-x86_64
```

Omitting `--target` validates ARM64 only. Conversely, an x86_64-target change
must pass the x86_64 suite and its relevant x86_64 benchmark gate; a host ARM64
run is required only when ARM64 is also declared in scope.

Verification mode compares the full run with the compatible
architecture-specific canonical Dark snapshot, not `RESULTS.md`. It compares
products of positive instruction counts for the aggregate and exact integer
counts for each benchmark. An individual loss of 0.1% or more fails even when
the equal-weight geometric `current/baseline` ratio is below 1. A smaller loss
can pass only with an equal or improved aggregate; the task agent must establish
that such a loss cannot be avoided before handoff. Read-only verification does
not modify tracked benchmark files.

When a compiler change improves aggregate full-profile performance, run
`./benchmarks/run_benchmarks.sh full` in recording mode and commit the updated
Dark snapshot and generated `benchmarks/RESULTS.md`; commit
`benchmarks/BASELINES.md` only for an audited Rust refresh. Recording advances
only on aggregate improvement without a blocking individual regression and
leaves the stronger snapshot/results on regression.
Integration measures every queued candidate once with `--verify-deployed`.
The gate compares its 29 exact instruction counts with the last deployed head's
counts stored in the shared Git directory. A new regression fails unless the
exact candidate has a human-approved exception. Deployment promotes those
measured counts for the next candidate, including an approved regression. The
canonical best-known snapshot remains separate. A missing or incompatible
deployed baseline stops the gate; seed it from a complete measurement of the
actual deployed head with `deployed_baseline.py seed RESULTS_DIR`. Audited Rust
refreshes remain separate via `--refresh-baseline=rust`.

If a rebase conflicts in generated `benchmarks/RESULTS.md`, resolve the source
conflicts and run `./benchmarks/run_benchmarks.sh full` in recording mode. The
run is the only valid resolution: it must prove an aggregate improvement without
an individual regression of 0.1% or more, advance the canonical Dark snapshot,
and replace `RESULTS.md` with fully regenerated results. Never hand-merge or
select one conflicted version. If the
run fails or does not advance and regenerate the files, abort the recovery.

When reporting verification, include the exact commands run, whether they
passed or failed, and any residual risk. Report the task-parent comparison's
`current/parent` ratio as the branch performance result; do not relabel the
verification command's `current/baseline` ratio as branch-relative.

For Linux x86_64 benchmark validation on an ARM64 worker, use the
canonical `benchmarks/x86_64_check.py` quick track. DCB measures the exact base
with Dark and audited Rust, measures the candidate with Dark, and retains the
structured comparison outside either worktree. A `partial-*` decision is useful
diagnostic evidence but is never a verified win. Run both the host full gate
and the x86_64 QEMU gate only when both targets were explicitly included in the
change; each declared target must pass its own gate.
