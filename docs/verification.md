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

Agents may run narrower checks while developing a change, but a change is not verified until the full verification policy has passed or the agent explicitly reports why full verification could not be completed.

Verification mode compares full Dark instruction counts with the committed
`benchmarks/RESULTS.md` values without modifying tracked files. When a compiler
change intentionally changes performance, run
`./benchmarks/run_benchmarks.sh routine` in recording mode and commit the resulting
`benchmarks/RESULTS.md` and `benchmarks/HISTORY.md` updates before verifying it.

When reporting verification, include the exact commands run, whether they passed or failed, and any residual risk.
