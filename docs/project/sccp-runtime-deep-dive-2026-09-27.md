# Why sparse conditional simplification dominates test runtime

This investigation profiles the complete default ARM64 host suite at
`313ad16db5` on Linux `aarch64`, then probes the largest E2E batch with
additional counters and one temporary worklist-order experiment. The final
compiler source is unchanged. Raw traces, timing JSON, and the temporary
instrumentation patch are retained under `TestResults/ai/sccp-*` in this
worktree; they are ignored build artifacts. The temporary tracing build ran
`./run-tests --timings-json=TestResults/ai/sccp-full-timings.json` for the
full suite, then `./run-tests --filter=string.e2e` with
`--e2e-batch-size=N` and `--timings-json=PATH` for the batch-size comparisons.
The runs omitted `--ai` so the test host retained the diagnostic stream in
redirected files. The normal compiler was rebuilt after restoring the source.

## What is slow

The instrumented full suite passed **10,372/10,372** tests in 128.95 seconds.
Sparse conditional simplification (SCCP) took 67.14 seconds in this run.
Of that, 66.04 seconds (98.4%) was its dataflow analysis. Copy discovery
(0.057 seconds), copy-chain resolution (0.029), instruction and CFG rewrite
(0.423), and structural equality (0.222) are small by comparison. Instrumentation
and shared-host load affect absolute times: an earlier uninstrumented run of
the same revision measured 48.14 seconds in SCCP. The attribution and
worklist counts are the useful evidence here.

**The generated E2E batch caller accounts for 63.97 seconds, or 95.3% of
full-suite SCCP time.** It is named `__dark_compiler_program_entry`. The 341
batched physical executions shared 7,756 logical checks. Individual checks
within a batch are separate functions, but the caller combines their results
into masks. [E2ETestRunner.fs](../../src/Tests/test-suite-tooling/Runners/E2ETestRunner.fs)
builds one result binding and an `if result then bit else 0` expression per
check. The caller therefore contains a long sequence of independent branches.
The first check name in the table locates the batch; it is not an individually
slow test.

| E2E source and batch locator | Checks in batch | SCCP time |
| --- | ---: | ---: |
| `e2e/stdlib/string.e2e`, `Stdlib.String.append "hello" " world"` | 297 | 17.69 s |
| `e2e/upstream/stdlib/char.dark`, `Stdlib.Char.toLowercase 'A'` | 224 | 7.78 s |
| `e2e/upstream/stdlib/ints/int16.dark`, `Stdlib.Int16.divide -32768s -1s` | 149 | 3.52 s |
| `e2e/upstream/stdlib/date.dark`, date parse batch | 167 | 3.38 s |
| `e2e/optimizer-constant-folding.e2e`, `signedDiv2 (-8L)` | 145 | 2.60 s |

The five batches above used 34.97 seconds, 52.1% of all SCCP time. The
largest ten used 44.95 seconds, 66.9%. Setup and unit work used only about
0.78 seconds of SCCP time.

## What each fixed-point iteration does

[MIR_Optimize.fs](../../src/DarkCompiler/passes/mir/MIR_Optimize.fs) runs the
combined sparse conditional and copy simplification pass, followed by CSE,
loop passes, DCE, and CFG cleanup, until the CFG stops changing or ten
iterations run. SCCP builds a copy map, resolves aliases, then analyzes
reachable blocks and edges before rewriting the CFG. In the instrumented full
run, all MIR optimizations took 70.17 seconds: SCCP took 67.14, CSE took
1.52, DCE took 0.50, and loop and CFG cleanup passes were smaller. These
subpass totals include every fixed-point iteration and show that repeating
other passes is not the principal cost.

The full trace contains 58,964 SCCP iteration events. First iterations used
34.10 seconds; second iterations used 32.98 seconds. All later iterations
together used about 0.06 seconds. The second iteration is normally required
to prove the optimizer reached a fixed point, but it repeats the expensive
analysis even when its output is unchanged.

The largest string batch shows the repeated work precisely:

| Iteration | Input CFG | Block visits | Edge changes | Block-fact changes | Peak Boolean facts | SCCP time | Whole-optimizer result |
| --- | --- | ---: | ---: | ---: | ---: | ---: | --- |
| 1 | 893 blocks, 895 instructions | 177,008 | 177,013 | 133,057 | 297 | 12.07 s | changed; 892 blocks remain |
| 2 | 892 blocks, 895 instructions | 177,007 | 177,012 | 133,056 | 297 | 11.33 s | unchanged |

Each iteration also reevaluated about 182,770 instructions. Only one scan for
unresolved branches occurred per iteration and took about one millisecond.
The normal worklist consumed over 99% of analysis time. The tiny one-block
reduction after iteration 1 does not explain the second iteration's cost;
the analyzer starts over with empty facts on the nearly identical graph.

## Why the worklist repeats the graph

[SparseConditionalConstants.fs](../../src/DarkCompiler/passes/mir/optimization/SparseConditionalConstants.fs)
represents path facts as maps of known Boolean values and integer ranges.
`factsOnEdge` carries all facts from the source block and adds the current
branch condition. `refreshBlockFacts` merges facts from executable
predecessors and queues a block when the merge changes. The worklist prepends
new blocks (`label :: state.Worklist`), so it processes the newest successor
first.

For the batch caller, this depth-first order reaches later conditions while
only one side of an earlier branch has contributed facts. It propagates a
provisional Boolean fact through the remaining branch chain. When the other
side reaches the join, the fact is removed and downstream blocks are queued
again. Repeating that pattern across hundreds of independent conditions
causes about 177,000 visits for 893 blocks. Peak facts reach 297, the number
of checks. Only 1,754 value-lattice changes occurred in each large iteration;
133,000 block-fact changes drove most of the revisits.

The same mechanism appears across batch sizes. In a focused `string.e2e`
run, the normal implementation produced:

| Maximum batch size | Physical executions | Full focused run | SCCP | Batch-caller SCCP | Caller block visits |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | 331 | 7.00 s | 0.46 s | 0.02 s | 903 |
| 32 | 19 | 7.75 s | 0.90 s | 0.38 s | 38,478 |
| 64 | 14 | 12.81 s | 1.98 s | 1.28 s | 73,500 |
| 128 | 12 | 13.94 s | 3.91 s | 3.02 s | 139,016 |
| Default (8192) | 10 | 34.12 s | 24.13 s | 23.40 s | 354,027 |

Physical execution counts include unbatched checks. The wall times are
subject to shared-host contention, but worklist visits are deterministic.
Across full-suite batch callers with at least 30 blocks, observed SCCP time
rose roughly with block count to the 2.5 power; this is a descriptive fit,
not a complexity proof. The visit counts and source show the repeated
propagation directly.

## Diagnostic traversal-order experiment

I changed only worklist insertion from prepend to append, making this
particular list-backed worklist FIFO. This was a temporary experiment and
has been reverted. All 331 focused string checks and all 224 focused
upstream char checks passed under it.

For the same 297-check string batch, the caller fell from about 177,000 to
1,180 block visits per iteration and from 297 to one peak Boolean path fact.
The focused suite's SCCP time fell from 24.13 to 0.54 seconds; its total time
fell from 34.12 to 8.94 seconds. This strongly supports traversal order and
provisional path-fact propagation as the cause. List append is not a suitable
production queue because each append copies the pending list; the experiment
does not establish correctness across the full language corpus or benchmark
performance.

## Next implementation step

Use a real FIFO queue with amortized constant-time enqueue and the existing
pending-block deduplication. Keep SCCP's ahead-of-time compile behavior and
verify the full host suite and task-parent benchmark gate. Compare the generated
programs and compiler timings for the large batch callers. A further option
is to limit Boolean path facts to conditions that can still affect a
reachable downstream branch; that requires separate semantic proof. Skipping
the second fixed-point iteration alone would save time but would remove the
current proof that the other MIR passes have settled.
