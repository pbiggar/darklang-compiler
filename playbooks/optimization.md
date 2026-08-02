# Implement one compiler optimization

The compiler has many documented optimization opportunities in `docs/investigations/benchmark-*.md`. Each investigation identifies specific performance gaps between Dark and Rust/OCaml, with recommended fixes categorized by complexity (Low, Medium, High).

You are going to implement EXACTLY ONE optimization.

2. Read all investigation files in `docs/investigations/benchmark-*.md`. Create a list of all optimization opportunities that have "Low" complexity. From this list, choose ONE optimization at random (do not pick deterministically or by perceived easiness).

3. PLAN THE IMPLEMENTATION. Before writing any code:

   - Read the relevant investigation file thoroughly
   - Identify the exact files to modify (usually in `src/DarkCompiler/passes/`)
   - Understand the current behavior by reading the existing code
   - Design the implementation approach
   - Identify edge cases and potential issues
   - Write your plan in a comment block before proceeding

4. CREATE A TEST that demonstrates the optimization:

   - Add a test file in the appropriate test directory
   - The test should dump IR (ANF, MIR, or LIR depending on where the optimization occurs)
   - The test should verify that the unoptimized pattern is NOT present in the output
   - Run the test BEFORE implementing the optimization to confirm it fails
   - Example: if removing redundant self-moves, the test should check that `Mov(X19, Reg X19)` patterns do not appear in the generated LIR

5. IMPLEMENT THE OPTIMIZATION:

   - Make minimal, focused changes to the compiler
   - Follow existing code patterns and style
   - Add comments explaining non-obvious logic
   - Do not refactor unrelated code

6. RUN THE TEST SUITE (`./run-tests --ai`). All tests must pass. If tests fail:

   - Fix the compiler, not the tests
   - The optimization may have exposed a latent bug - fix it
   - Repeat until all tests pass

7. RUN THE ROUTINE BENCHMARK PROFILE using `./benchmarks/run_benchmarks.sh routine`:

   - Benchmarks should complete with no failures
   - Compare results against baseline in RESULTS.md
   - Report the performance ratio from the RESULTS.md table header
   - The optimization should show measurable improvement in at least one benchmark
   - If no improvement is visible, investigate why (the optimization may not be triggering, or the benchmark may not exercise the optimized code path)

8. DOCUMENT YOUR FINDINGS:

   - Update the investigation file with implementation status
   - If you discovered anything unexpected, add it to the investigation
   - If a new investigation topic emerged, create a new file

9. WRITE A REPORT to the developer. Include:

   - Which optimization was implemented
   - What code changes were made (file names and brief description)
   - The test that was added (show the test code)
   - Before/after IR examples showing the optimization in action
   - Benchmark results (show the RESULTS.md diff if there are changes)
   - Any issues encountered or follow-up work identified

10. Prepare the change for human review under the shared orchestrator review workflow.
    Include the code, tests, and any accepted benchmark evidence in one coherent
    reviewable unit, but leave integration decisions to the shared workflow.

Include before/after benchmark data in the commit message:

```
Enable DCE + constant folding optimizations

Benchmark improvement:
- factorial: 7.35M → 6.92M instructions (-5.8%)
- fib: 1015M → 955M instructions (-5.9%)
- sum_to_n: 39M → 36M instructions (-7.7%)

Baseline: commit abc123
After: commit def456
```



## Policies for implementing optimizations

- Prefer correctness over performance. If an optimization might change semantics, do not implement it.
- Do not implement partial optimizations. If an optimization cannot be fully implemented, choose a different one.
- If the optimization requires changes to multiple passes, ensure they are coordinated correctly.
- If you discover the optimization is already implemented (perhaps partially), report this and choose a different optimization.
- If the optimization turns out to be Medium or High complexity despite being labeled Low, report this and choose a different one.

## Quick reference: Low complexity optimizations commonly found

These patterns frequently appear as "Low" complexity in investigations:

- **Redundant self-move elimination**: Remove `Mov(X, Reg X)` patterns after register allocation
- **Empty SaveRegs/RestoreRegs elimination**: Skip code generation for empty save/restore pairs
- **Dead code elimination improvements**: Remove unreachable blocks or unused assignments
- **Constant folding in specific passes**: Evaluate compile-time-known expressions

Read the actual investigation files for the authoritative list - this is just a hint of what to look for.
