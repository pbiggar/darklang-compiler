# Verification Policy

This policy applies to all agents when they verify a proposed commit, change, fix, workflow update, or integration step.

Verification means both:

- All tests pass.
- Benchmarks do not regress.

For compiler repository changes, the default verification commands are:

```bash
./run-tests --ai
./benchmarks/run_benchmarks.sh --verify routine
```

The E2E runner compiles compatible value-equality checks in batches of 16 by
default. Each check remains a separately compiled function; only the caller and
executable are shared. Use `--e2e-batch-size=1` for a singular diagnostic
baseline, or a value through 32 for batch-size experiments. Timing JSON records
the configured size, logical/eligible test counts, physical executions, batch
count, batched logical tests, and largest observed batch so performance results
can be compared without confusing logical coverage with compiler invocations.

Agents may run narrower checks while developing a change, but a change is not verified until the full verification policy has passed or the agent explicitly reports why full verification could not be completed.

Verification mode compares the complete routine run with the compatible
architecture-specific canonical Dark snapshot, not `RESULTS.md`. The decision is
the exact comparison of the products of every positive instruction count; the
reported equal-weight geometric `current/baseline` ratio is below 1 for an
improvement and above 1 for a regression. Individual losses may be compensated
by larger gains. Equal and improved aggregate runs pass ordinary read-only
verification, regressions fail, and no tracked benchmark file is modified.

When a compiler change improves aggregate routine performance, run
`./benchmarks/run_benchmarks.sh routine` in recording mode and commit the updated
Dark snapshot and generated `benchmarks/RESULTS.md`; commit
`benchmarks/BASELINES.md` only for an audited Rust refresh. Recording advances
only on improvement and leaves the stronger snapshot/results on regression.
Integration uses `--verify-fresh` and stops if a known improvement has not been
recorded. An incompatible or missing snapshot requires
one complete successful `--reset-dark-baseline` routine run; partial, targeted,
`all`, hyperfine, and failed runs cannot reset it. Audited Rust refreshes remain
separate via `--refresh-baseline=rust`.

When reporting verification, include the exact commands run, whether they passed or failed, and any residual risk.

For changes limited to the Linux x86_64 backend on an ARM64 worker, use the
canonical `benchmarks/x86_64_check.py` quick track. DCB measures the exact base
with Dark and audited Rust, measures the candidate with Dark, and retains the
structured comparison outside either worktree. A `partial-*` decision is useful
diagnostic evidence but is never a verified win. Shared compiler/runtime
changes require both the host routine gate and the x86_64 QEMU gate; a
regression on either track rejects the aggregate result.
