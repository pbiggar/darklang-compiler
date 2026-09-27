# Host test and compiler runtime profile (2026-09-27)

This profile measures the complete default ARM64 host suite at `313ad16d`
(`origin/main` at task start) on Linux `aarch64`. It is a baseline for deciding
where compiler and test runner work is repeated. The raw, ignored artifacts are
in `TestResults/ai/full-profile-timings.json`,
`TestResults/ai/full-profile-codegen.json`, and
`TestResults/ai/full-profile.log` in the profiling worktree.

## Method and result

```sh
./build --ai
./run-tests --ai \
  --timings-json=TestResults/ai/full-profile-timings.json \
  --codegen-profile-json=TestResults/ai/full-profile-codegen.json
```

All 10,372 tests passed. The runner measured 94.17 seconds; shell wall time was
96.07 seconds. The E2E suite occupied 87.76 seconds, while unit suites took
5.95 seconds and ran concurrently with E2E. Pass timings are nested: a detail
pass is included in its parent pass and must not be added to it.

| Measured work | Time | Invocations | Interpretation |
| --- | ---: | ---: | --- |
| MIR optimizations | 50.15 s | 1,738 | Largest compiler phase |
| MIR sparse conditional simplification | 48.14 s | 1,689 | Included in MIR optimizations; 96% of that phase |
| AST to ANF | 8.62 s | 1,253 | User compile phase; overlaps dependency and ownership details |
| AST to ANF dependency lookup | 5.56 s | 1,578 | Includes 5.52 s of conversion on misses |
| Ownership analysis | 4.98 s | 1,396 | Conversion detail across user, stdlib, and preamble work |
| Native test execution | 3.86 s | 1,252 | Generated processes, excluding compile time |
| ARM64 code generation | 2.57 s | 1,252 | 2.06 s in function generation |
| ARM64 emission | 0.98 s | 1,252 | Encoding and binary emission |
| LIR function tree shaking | 0.26 s | 3,756 | Multiple traversals, but a small measured cost |

The runner executed 8,816 logical E2E tests in 1,401 physical executions.
It batched 7,756 tests into 341 executions; its maximum batch was 297. This
already removes thousands of compiler invocations. The reported per-test
times for batched checks divide one physical compile and run across the checks,
so they are unsuitable for finding a slow compiler function.

A second full run with timing JSON but without codegen metrics also passed
10,372/10,372 tests. It took 146.17 seconds, with 77.14 seconds in sparse
conditional simplification. The large wall-time difference between runs on
this shared host prevents using them to estimate profiling overhead; the MIR
pass remained about half of runner time in both samples.

## Findings

1. **MIR simplification is the clear priority.** `MIR_Optimize.fs` invokes
   `applySparseConditionalSimplification` in a fixed-point loop capped at ten
   iterations. Each invocation builds a copy map, resolves every copy chain,
   analyzes conditional CFG edges and values, rewrites instructions and blocks,
   and compares the resulting CFG structurally with the input. In
   `CopyPropagation.fs`, `resolveCopyMap` calls `resolveCopy` independently for
   every destination, so long shared chains can be traversed repeatedly.
   The profile establishes the cost of the combined pass, not which of these
   internal steps dominates. Instrument those steps and iteration counts before
   changing the algorithm. Likely experiments are memoized copy resolution,
   an explicit change flag instead of full CFG equality, and avoiding a new
   analysis when the preceding iteration cannot affect its inputs. Verify
   language behavior with E2E tests and measure compiler time and generated
   code before adopting any change.

2. **Dependency conversion is the next compiler-sized cost.** Source preparation
   spends 5.56 seconds in dependency lookup, of which 5.52 seconds is actual
   conversion. Session counters report 319 ANF dependency hits and 934 misses;
   compiled dependency results have 24 hits and 340 misses. This is a high
   miss fraction, although different preamble contexts and generated
   specializations may genuinely require distinct results. Inspect cache keys
   and per-context miss attribution before broadening reuse. Ownership analysis
   contributes 4.98 seconds across conversion paths and feeds variant scheduling and
   lowering, so it cannot simply be removed.

3. **Reachability is computed in several forms.** `UserCompilation.fs` prunes
   user ANF functions before and after ANF optimization, builds a LIR call graph,
   filters user functions, then walks user functions again to include special
   `Builtin.pm*` roots. It separately queries reachable stdlib LIR functions.
   Different stages can create or remove calls, so the ANF and LIR boundaries
   have distinct purposes. The second LIR user closure appears combinable with
   the first filter by supplying all required roots together, but the complete
   LIR tree-shaking timing is only 0.26 seconds. This is cleanup, not a
   promising runtime optimization.

4. **MIR has two simplification routes.** With the normal combination of
   constant folding, CFG simplification, and copy propagation enabled,
   `MIR_Optimize.fs` uses the combined sparse conditional pass. Other flag
   combinations run separate constant folding, copy propagation, and branch
   simplification passes. The normal route skips those separate passes, so
   this is duplicate implementation rather than measured duplicate runtime.
   Keep the option-specific behavior in mind when consolidating the algorithms;
   a faster normal route must preserve the independently selectable passes.

5. **Backend caching is working and backend work is secondary.** The ARM64
   codegen session records 66,396 function cache hits and 16,604 misses; `_start`
   compilation has 1,246 hits and six misses. Code generation and emission
   together are under four seconds. The remaining codegen time is mostly
   function generation, but even eliminating it all would save much less than
   improving the MIR pass.

## Limits

This is a complete host profile plus a second full-suite check, not an uncontended
parent/candidate timing comparison. The suite's unit and E2E work overlap, and
nested compiler detail timings overlap their parent phases. The trace does not
attribute MIR simplification time to individual functions or internal SCCP
steps. No compiler code or benchmark result was changed by this investigation.
