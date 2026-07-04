# Verification Policy

This policy applies to all agents when they verify a proposed commit, change, fix, workflow update, or integration step.

Verification means both:

- All tests pass.
- Benchmarks do not regress.

For compiler repository changes, the default verification commands are:

```bash
scripts/run-in-container ./run-tests
scripts/run-in-container ./benchmarks/run_benchmarks.sh all
```

Agents may run narrower checks while developing a change, but a change is not verified until the full verification policy has passed or the agent explicitly reports why full verification could not be completed.

Benchmark runs may update generated result files such as `benchmarks/RESULTS.md` or `benchmarks/HISTORY.md`. Treat those files as commit material only when they are part of the reviewed change's accepted performance evidence or the human explicitly asks to keep them. For documentation-only cleanup, already-done evidence updates, and rejected experiments, report benchmark outputs as verification evidence and leave generated benchmark result files out of the repository change.

When reporting verification, include the exact commands run, whether they passed or failed, and any residual risk.
