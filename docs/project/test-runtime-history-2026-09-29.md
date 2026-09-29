# Historical host-suite compile-time comparison (2026-09-29)

This comparison identifies the reproducible fast revisions and the work behind
the later runtime increase. It measures complete ARM64 host suites on Linux
`aarch64`, using the default E2E batch size of 8192. Each checkout was built
with `./build --ai`, then run with `./run-tests --ai` plus timing and ARM64
codegen JSON output. The runs used the same host and target, but the corpus and
compiler changed. Raw JSON and logs remain in ignored `TestResults/ai/`
directories in their historical worktrees.

## Verified revisions

| Revision | Suite | Physical E2E executions | Runner time | Historical role |
| --- | ---: | ---: | ---: | --- |
| `9ddd172a14977a8e516356a101dbeaa0714b55bd` | 10,150 | 1,367 | **46.00 s** | Reproduces the documented 44.92 s checkpoint |
| `313ad16db513f19cb1399f39ebda76396f2dbe0d` | 10,372 | 1,401 | **91.25 s** | Pre-FIFO SCCP; reproduces the earlier 94.17 s profile |
| `797ff13051527e748e14491246b95b7399e144b9` | 10,452 | 1,429 | **61.31 s** | First mainline merge containing FIFO SCCP |
| `6146e0b8804eea4495db9131764b3572b3d440a1` | 10,800 | 1,576 | **147.02 s** | Current task parent |

The `9ddd172a14` and `797ff13051` first runs took 87.2 and 70.8 seconds
respectively while other processes were active; the table uses their quiet
repeats. The current revision has also been observed near 200 seconds, so
147.02 seconds is a quiet-host sample, not a promise of stable wall time; a
later required run without timing JSON took 174.6 seconds. A
separate gate-era checkout, `3556b723393b079d4058895869d8788cf58efad9`,
ran the same 10,150-test corpus as `9ddd172a14` in 49.56 seconds. No exact
revision or comparable complete-suite measurement was found for the recalled
30-second run. An earlier mainline candidate was still running after three
minutes and was stopped; it is not a verified fast checkpoint.

## Phase comparison

Times below are cumulative pass timings in seconds. Parent and child phases
overlap; do not add rows. A dash means the pipeline stage was absent.

| Timed work | 46 s revision | Pre-FIFO | Post-FIFO | Current |
| --- | ---: | ---: | ---: | ---: |
| E2E suite | 39.45 | 84.66 | 52.31 | 122.60 |
| Native test execution | 3.85 | 3.90 | 4.64 | 6.30 |
| AST to ANF | 7.64 | 7.94 | 10.38 | 23.75 |
| Symbol import, within AST to ANF | 0.14 | 0.17 | 0.23 | **13.70** |
| Dependency conversion, within AST to ANF | 5.17 | 5.04 | 6.65 | 8.17 |
| ANF optimization | 4.03 | 1.86 | 2.36 | — |
| SSA optimization | — | — | — | **29.00** |
| Call graph compilation | — | — | — | **25.87** |
| MIR optimization | 4.98 | 48.84 | 5.42 | 6.34 |
| SCCP, within MIR optimization | 1.81 | **46.81** | 2.88 | 2.90 |
| Code generation | 2.19 | 2.43 | 3.15 | **19.17** |
| Standard-library build overhead | 2.78 | 3.32 | 4.40 | 8.75 |

The FIFO worklist change removed roughly 44 seconds of SCCP work between the
pre- and post-FIFO profiles. The surrounding suite gained 80 tests and 28
physical E2E executions, and the total fell by 29.94 seconds. SCCP remains
about three seconds today. The later rise is elsewhere: from post-FIFO to
current, physical E2E executions grew 10.3%, while runner time grew 139.8%.
Native test execution increased only 1.66 seconds. This is predominantly
compiler work, not time spent running HTTP tests.

## Unnecessary work identified

1. **Backend callee refinement repeats whole-program analysis.** The call
   graph pipeline computes final callee write summaries, yet ARM64 code
   generation calls `summaries` over the emitted LIR program again for each
   binary and then refines call-save envelopes. Temporary probes in a second
   complete current-suite run measured **20.84 seconds across 1,378 codegen
   calls**, out of 24.23 seconds in code generation. Function instruction
   generation used 2.02 seconds in that run; group remapping used 0.10 seconds.
   That instrumented run took 179.29 seconds overall, so its phase times
   should not be substituted into the 147.02-second table. Carrying the
   finalized summaries into output generation is the strongest measured
   opportunity, subject to preserving conservative behavior for unknown and
   recursive calls.

2. **Symbol import became a large per-compilation cost.** It ran 1,612 times
   in the post-FIFO profile and 1,712 times now, but rose from 0.23 to 13.70
   seconds. Its composition routine folds several symbol maps into a base
   catalog on every compilation. The invocation increase alone cannot
   explain the roughly sixtyfold time increase. Measure catalog sizes and
   avoid repeatedly composing an unchanged base before choosing a cache key;
   this profile establishes the hot boundary, not which particular map grew.

3. **SSA optimization scans the function-name catalog for fixed names.** The
   recent SSA expression optimizer resolves three string helper names by
   traversing the full catalog on every optimization iteration, including
   functions with no corresponding string operation. Another rewrite performs
   similar scans for matching calls. The full SSA stage costs 29.00 seconds;
   the catalog-scan share is not separately timed. Pre-resolving these stable
   names and using direct lookups is a focused experiment, not a measured
   29-second saving.

4. **The call graph has a smaller unexplained remainder.** Its 25.87 seconds
   include 6.34 seconds of MIR optimization, 3.10 seconds of register
   allocation, 3.09 seconds of call-aware allocation and save pruning, 3.02
   seconds of ANF-to-MIR conversion, and other timed subpasses. Those nested
   figures leave several seconds of scheduling, summary publication, and
   other work to attribute before changing the algorithm. The pipeline
   already asserts one native stage visit per scheduled function node;
   4,821 MIR pass invocations reflect the current emitted function corpus,
   not proof that a node was compiled twice.

HTTP and TLS additions plausibly increase the test corpus and the standard
library's compilation context. The measured standard-library build overhead
rose 4.35 seconds from post-FIFO to current, and the extra physical E2E
executions are 147. Neither accounts for the full 85.71-second quiet-run
increase. The repeated global analyses and per-compilation catalog work above
are better targets. The 46-second revision demonstrates a real historical
suite time, but its smaller corpus and older pipeline do not establish that
today's suite can reach 30 seconds without further changes.
