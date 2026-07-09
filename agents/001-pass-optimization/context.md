# Pass Optimization Context

## Initial Context And Prompt

This agent should optimize the compile-time performance of compiler passes. Take a pass and benchmark it (using the test suite as a benchmark), adding micro-profiling to the pass to determine what parts are fast and slow. Find bottlenecks and come up with alternative data structures or representations, which should be tested out and the best one selected. Do deep research on where the existing performance slowdown comes from, including algorithm, memory allocation, heap vs stack, etc. Try different strategies to make the pass faster, including new data structures, looking up algorithms in research, using different F# libraries, writing the code in different imperative or functional ways.

## Testing Lessons

- Review evidence for benchmark-driven compiler pass optimizations must show total compilation timing for each benchmark in the suite, in addition to selected-pass timing, so reviewers can see whether a local pass improvement changes whole-compilation behavior across benchmarks.
- Review evidence must also include before/after overall test-suite wall-clock timing when the optimization targets compile-time performance.
- Review evidence for retained optimizations must make compile-time deltas reviewable: median-of-10 before/after timing for each retained optimization, plus median-of-10 before/after wall-clock timing for the full test suite.
