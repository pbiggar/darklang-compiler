# Finding compiler slowdowns

Use this guide to form and test hypotheses about compiler cost. A suspicious
loop or collection operation is a lead; measure its contribution on the
current revision before changing it. Historical fixes illustrate patterns,
but do not establish today's bottleneck.

## Start with the workload and a baseline

Decide which behavior is slow: a fresh CLI compilation, repeated compilation
in one process, a particular large function, or the complete test suite.
These workloads exercise different setup costs, caches, and graph sizes.
Record the commit, dirty state, source or test selection, architecture, build
configuration, batch size, and cache state with the measurements. Keep raw
logs and temporary instrumentation in ignored artifacts.

Build first, then collect compiler phase timings across the host suite:

```sh
./build --ai
./run-tests --ai \
  --timings-json=TestResults/ai/compiler-timings.json \
  --codegen-profile-json=TestResults/ai/compiler-codegen.json \
  > TestResults/ai/compiler-profile.log 2>&1
```

Use `--filter=json`, for example, to narrow a second run to a subsystem.
The codegen output provides per-function ARM64 metrics and cache information;
it does not provide equivalent per-function coverage for every compiler phase
or backend. See the [profiling options](../../benchmarks/README.md#targeted-compiler-benchmarks).

For a single program, `./dark -vv program.dark -o /tmp/program` reports pass
timings. To measure fresh-process latency with repeated samples:

```sh
python3 benchmarks/targeted/compile-latency/measure.py \
  --output /tmp/compiler-latency.json
```

This probe uses its own fixed representative program and records both process
wall time and reported pipeline time. A large gap is a reason to investigate
startup, JIT, and standard-library setup outside the reported pipeline.
A batch test profile may hide these costs through shared prepared contexts.
Each compatible check remains a separate function; batching shares the generated
caller and executable. A large caller can therefore stress analyses that are
cheap for ordinary programs even while batching reduces repeated setup.
Changing batch size is also a way to discover overhead before you know which
phase is responsible. Run the same test selection with several sizes, for
example `--e2e-batch-size=1`, `--e2e-batch-size=64`, and
`--e2e-batch-size=8192`, retaining separate timing JSON and logs for each run.
Compare suite wall time, phase times, invocation counts, and the timing JSON's
actual physical executions and largest observed batch. Check that logical test
coverage is unchanged; incompatible tests may prevent the requested batch size
from being reached. See the [batching policy](verification.md) for the recorded
counts and supported sizes.

If larger batches reduce total time and a phase's invocation count, investigate
setup or analysis repeated per compilation. If larger batches increase time
in a phase despite fewer compilations, investigate scaling with the generated
caller, CFG, or combined program size. If little changes, check which work
still runs per test or function regardless of batching. These are leads to
verify with counters and repeated runs, not proof from one noisy comparison.
Per-test times divided across a batch do not identify which function was
expensive.

## Narrow a hot phase to the operation responsible

Rank phases by total time, then inspect invocation counts and time per call.
Many cheap calls suggest repeated work; a few expensive calls suggest large
inputs or poor scaling. Nested detail timings are included in their parents:
do not sum overlapping phases or equate them with total suite wall time,
especially when suites run concurrently.

Search for the measured phase name or its implementation with `rg`, then read
the relevant call sites and loops. Add narrowly scoped timers and counters
when the existing phase is too broad. Useful measurements include functions,
blocks, edges, instructions, catalog entries, iterations, cache hits/misses,
and the number of distinct bodies processed. Attribute expensive calls to a
function or input size where possible. For a hot fixed-point pass, separately
measure analysis, rewriting, equality checks, and iterations rather than
assuming the whole pass is expensive for one reason.

If phase timers cannot explain the cost, use native CPU sampling (for example
`perf` on Linux) and OCaml `Gc.Memprof` allocation sampling. Capture stacks around
the focused workload and identify compiler callers behind generic collection
operations. CPU samples show where execution spends time; allocation samples
show where objects are created. Neither alone proves how much elapsed time a
change will save. Measure allocation volume, GC time, and elapsed time
separately; lower allocation need not produce a proportional latency drop.

## Patterns to investigate

| Potential slowdown | What to look for | How to test the hypothesis |
| --- | --- | --- |
| Whole-catalog scans inside local work | A lookup, constructor query, or helper-name search filters/sorts every function or type for each expression or pass iteration. | Count scans and entries visited; vary catalog size while keeping the user program fixed. Check whether a direct lookup or index by owner can answer the same query. |
| Oversized analysis inputs | A small program copies or analyzes all stdlib/external summaries, symbols, or constructors although it uses only a few. | Compare input catalog size with declarations actually imported or callees referenced. Time setup separately from analysis of reachable bodies. |
| Repeated analysis of unchanged bodies | Ownership, lowering, or backend summary construction revisits the same function in materialization rounds or for each emitted binary. | Count visits by function and body version. Trace which dependency or body change invalidated each result; distinguish legitimate reanalysis from duplicate work. |
| Fixed-point or worklist churn | A pass repeatedly traverses the whole CFG, reconstructs maps, compares entire IRs, or revisits nodes whose facts did not change. | Record iterations, node visits, and fact changes; time equality checks separately. Try long copy chains, loops, joins, and large batch callers to expose scaling. |
| Analysis before applicability checks | A specialized rewrite builds predecessor, use, or dominator maps for functions with no relevant operation. | Count candidates versus successful rewrites and time rejected cases. Test a cheap necessary-condition check before building expensive supporting data. |
| Cache misses or expensive hits | Reused functions/plans are regenerated, cache keys are costly to construct, or hit paths still rebuild substantial metadata. | Measure hits, misses, key construction, and hit-path time separately. Compare cold and warm runs and inspect why equivalent requests get different keys. |
| Structural allocation and copying | Persistent maps/sets are rebuilt in nested loops; identities are converted repeatedly; immutable lists are repeatedly appended or concatenated. | Follow allocation stacks to their compiler callers and count collection sizes/copies. Vary graph or instruction count to distinguish linear work from quadratic growth. |
| Repeated representation queries | Lowering or ownership repeatedly derives the same layout or classification from a semantic type. | Count queries and distinct types within one analysis. Check whether results can be shared within that scope without crossing incompatible contexts. |
| IR growth | Inlining, specialization, helper generation, or batching greatly increases the function or instruction count before a hot pass. | Compare IR counts before and after the expanding stage and time downstream passes against those counts. Determine whether excess work starts at expansion or in a later algorithm. |

Examples of these patterns include looking up a sum's constructors by scanning
the complete constructor catalog, and recomputing a binary's register-clobber
summaries after the compilation pipeline already produced them. Use those as
questions to ask of new code, not as claims that those old costs remain.

For IR inspection, prefer `--dump-function=TEXT`, `--dump-ir-summary`, and
`--dump-ir-output=FILE` with the relevant dump flag. Keep verbose output in an
ignored artifact; full-program dumps often obscure the function responsible.

## Prove the cause and validate the change

Create a focused workload that preserves the suspected cause, then vary one
size dimension: catalog entries, copy-chain length, CFG edges, specializations,
or instructions. For example, hold a tiny user function fixed while increasing
unrelated declarations to test whether its lowering depends on catalog size.
A minimal example that removes the large graph or catalog may also remove the
slowdown. Compare repeated samples and operation counts to distinguish host
contention from algorithmic growth.

Change one cause at a time. Match the fix to the evidence: index repeated
queries, restrict analysis to relevant inputs, reuse valid summaries, or avoid
building analysis data for inapplicable rewrites. For any reuse, establish the
scope and invalidation rules: function identity alone may be insufficient if
the body, target, type arguments, or dependency facts change. Preserve semantic
validation and diagnostics, including ahead-of-time match checking.

Compare parent and candidate with the same inputs, configuration, batch size,
and cache conditions. Alternate repeated runs when host load varies, retain
individual samples, and report the measured phase plus end-to-end time. Do
not compare different test corpora as evidence for a particular improvement
or translate historical allocation totals into predicted latency savings.

Check that the improvement survives the broader workload and has not shifted
cost into another phase, setup, or memory retention. Compiler latency and the
speed of generated programs are separate outcomes: faster compilation can
still emit slower code. Follow the [verification policy](verification.md) for
compiler changes, including host correctness tests and the full parent
benchmark gate. Keep investigation findings and raw measurements in ignored
artifacts unless a durable report is explicitly requested.
