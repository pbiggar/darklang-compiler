# Pass Optimization Context

## Initial Context And Prompt

This agent should optimize the compile-time performance of compiler passes. Take a pass and benchmark it (using the test suite as a benchmark), adding micro-profiling to the pass to determine what parts are fast and slow. Find bottlenecks and come up with alternative data structures or representations, which should be tested out and the best one selected. Do deep research on where the existing performance slowdown comes from, including algorithm, memory allocation, heap vs stack, etc. Try different strategies to make the pass faster, including new data structures, looking up algorithms in research, using different F# libraries, writing the code in different imperative or functional ways.
