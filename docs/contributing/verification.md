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
./benchmarks/run_benchmarks.sh --verify-parent full
```

Full verification keeps terminal output concise so automated callers do not
consume context on repeated per-workload details. Full build and measurement
logs, the markdown report, and decision JSON remain in the reported results
directory. Use `--verbose` when interactive diagnosis needs streamed details.

The test runtime measurement requires a clean committed worktree, then runs the
already-built full suite with affinity to two CPUs on Linux, preserving the
host CPU-count E2E test, and records its wall time in
`TestResults/ai/test-runtime.json`. It rejects a sample if competing work
consumed more than 3% of one CPU, then retries on another CPU pair up to
three times. The comparison uses an
uncontended measurement of the branch's merge-base with the supplied ref.
The first comparison for a parent builds and measures that parent in a temporary
worktree; later comparisons reuse the measurement cached in the common Git
directory. A candidate fails if it takes over 10% or over 60 seconds longer,
whichever limit is stricter. Missing, mismatched, and contended measurements
invalidate the timing comparison. The merge train runs both `./run-tests --ai`
and the separate blocking `test-runtime` gate. A candidate with a significant
increase or no valid uncontended sample remains blocked until fixed or an
operator approves an exact gate exception.

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
Integration reruns every queued candidate with `--verify-fresh`. A regression
fails the gate unless the exact candidate has a human-approved exception. When
a candidate improves on the integration parent's snapshot,
the read-only gate deliberately fails and the integrator rebases the owning
branch, runs the full suite in recording mode, commits the regenerated snapshot
and `RESULTS.md`, and retries that new exact commit. The retry must reproduce
the recorded result before deployment. An incompatible or missing snapshot
requires one successful `--reset-dark-baseline` full run; partial, targeted,
`all`, hyperfine, and failed runs cannot reset it. Audited Rust refreshes remain
separate via `--refresh-baseline=rust`.

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
