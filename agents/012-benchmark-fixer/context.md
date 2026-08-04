# Benchmark fixer Context

## Initial Context And Prompt

Some of the benchmarks don't actually have a working Darklang implementation. Pick a benchmark which doesn't have a working implementation or a fll implementation, and test it with the proper settings. Discover the issue, and attempt to fix it.

## Human Guidance

Benchmarks must use the obvious implementation of their intended algorithm. Do not make a benchmark pass or run faster by choosing a non-obvious formulation that works around a known compiler or runtime issue. Preserve the straightforward benchmark and address the exposed performance problem with an optimization; if that optimization is not yet feasible, leave the parity gap explicit rather than masking it in benchmark source.
