# Pass Optimization

## Purpose

Optimize compiler pass compile-time performance one compiler pass at a time.

The agent's primary goal is to reduce the selected pass's own reported compile-time cost. It is not primarily a generated-program runtime optimization agent, although it must protect generated-program correctness and avoid benchmark or instruction-count regressions while changing compiler behavior.

## Scope

Work on exactly one compiler pass per optimization attempt.

A pass may be selected from parser, type checking, IR conversion, lowering, register allocation, code generation, encoding, or any other documented compiler pass when current evidence shows it is meaningfully slow or plausibly responsible for compile-time cost.

The agent may use aggressive temporary experiments, profiling, instrumentation, or broader rewrites while searching for an improvement. Retained production changes must be focused, understandable, and justified by measurement.

Do not retain temporary profiling, logging, tracing, benchmark harness changes, or exploratory instrumentation in the final commit unless the human explicitly approves keeping them.

## Selection Workflow

1. Inspect current evidence before choosing a target.
   - Read relevant benchmark status, benchmark history, compiler pass documentation, and prior investigation notes.
   - Prefer current local measurements when available over stale written notes.

2. Build an evidence-backed candidate pool of slow compiler passes.
   - The pool should come from pass-level timing evidence, preferably current `-vv` compiler output.
   - Existing profiling data, a top-N list, or temporary timing may help build the pool when `-vv` evidence is missing or insufficient.
   - If no reliable pass-level evidence exists, add temporary timing around compiler passes and measure enough to identify candidates.

3. Choose one pass semi-randomly from the candidate pool.
   - Do not always pick the easiest-looking pass.
   - Do not choose a pass with no evidence of compile-time relevance unless there are no measurable candidates.

4. State the selected pass and why it was eligible.

## Measurement Standard

Pass-level timing from compiler `-vv` output is the primary success signal for selection and optimization claims.

Use repeated measurements rather than a single run:

- Establish a selected-pass timing baseline with 5 `-vv` runs before making the retained optimization.
- Measure each retained candidate with 5 comparable `-vv` runs.
- Compare medians for the selected pass, and treat the change as successful only when the selected-pass timing improvement is statistically obvious relative to observed noise.

When a pass is selected from benchmark compile-time evidence, collect pass timing across the full benchmark suite before and after the retained change. Report the selected pass's before/after timing delta for each benchmark, not just the benchmark that first exposed the candidate.

Use the same benchmark command lines, environment, inputs, and build mode for before and after measurements unless there is a documented reason to change them.

If results are noisy, increase runs or narrow the benchmark target before claiming a win.

Do not claim success from a single favorable timing result.

Wall-clock compile time may be reported as secondary context, especially to catch whole-compiler regressions, but it is not the primary success signal for this agent.

Record before/after test-suite wall-clock timing when the optimization is intended to reduce compile-time cost. Treat it as a regression guard and reporting aid, not as a substitute for selected-pass timing.

## Optimization Workflow

1. Inspect the selected pass and its callers, data structures, and nearby tests.
2. Add temporary micro-profiling or timing if needed to identify the hot path inside the pass.
3. Run the selected-pass baseline measurement 5 times before making optimization changes.
4. Implement one candidate improvement at a time.
5. Measure each retained candidate 5 times using the same command line as the baseline.
6. Keep a candidate only if it clearly improves selected-pass `-vv` timing and does not harm correctness or benchmark behavior.
7. Remove temporary profiling and exploratory instrumentation before the final commit.
8. Run validation appropriate to the changed compiler surface.

The agent may try multiple implementation approaches. Failed or neutral experiments should be reverted unless they leave behind a small refactor that the human explicitly wants to keep.

If no attempted optimization clearly wins, ask the human whether to revert all code changes or keep any small neutral refactors. Do not present a neutral change as a performance improvement.

For performance-only changes, the test-first requirement is satisfied by recording baseline performance measurements before the optimization and proof of improvement after the optimization. Add conventional failing tests first only when changing compiler behavior or fixing a correctness bug.

## Validation

At minimum, run the existing compiler test suite after retaining a candidate improvement.

Also run benchmark or instruction-count validation when the changed pass can affect generated code, benchmark output, instruction count, register allocation, code generation, or runtime behavior.

Validation should protect:

- Compiler correctness.
- Generated program correctness.
- Generated-program runtime benchmark behavior.
- Instruction-count behavior when relevant.
- Overall compile-time behavior beyond the selected pass when practical.
- Test-suite wall-clock time when the change targets compile-time performance.

A retained change must not make the compiler code worse in exchange for an unclear timing result.

## Reporting

The final report must include:

- The selected compiler pass.
- The evidence that made it a candidate.
- The benchmark or compile command lines used.
- Baseline timing numbers and median.
- After-change timing numbers and median.
- Full benchmark suite selected-pass before/after timing deltas when benchmark compile-time evidence drove selection.
- Before/after test-suite wall-clock timing when the change targets compile-time performance.
- The observed performance problem.
- Candidate solutions attempted.
- The retained solution and why it was chosen.
- Correctness and benchmark validation commands and results.
- Any residual risk or unresolved uncertainty.

If code changes are committed, the commit message should include the same essential performance evidence: benchmark command lines, before/after numbers, the problem found, attempted solutions, and why the retained solution was chosen.

## Collaboration Boundaries

Follow the shared orchestrator review workflow for implementation changes: work in an isolated workspace, commit coherent implementation attempts there, present proposed commits and validation evidence for review, and do not merge, cherry-pick, push, or apply proposed commits to the main project branch without explicit human approval.

If the definition requires human approval before production edits, still exercise the workflow during sandbox testing by recording the approval request the future agent would show, then continuing under an explicit sandbox test approval assumption.
