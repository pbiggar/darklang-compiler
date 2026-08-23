---
format: 1
event-id: 01a02cfbc2cb71c29fbf6d4d0f6ddca0
entity-id: 019ffdef9256791abe50e9b22515dc4c
entity-kind: issue
event-type: trial-result
occurred-at: 2026-08-23T04:58:12.0438266+00:00
author: worker:65eb66d2e522:1481282:01a02c2a368573bc9b39df6a4ae424df
previous: 019ffdef92597634bde77c719f9e3514
attempt: 01a02cbd3775713c9fd18a50c7e54459
candidate: 8f09226d8380eb731adfaa8fb99ab7aac1e5e4d7
constraints-hash: bb5ae1cf020862dad9d5ae9dfa01563ea610fc889e444cd56419df1f161eb508
result: improved
revision: 019ffdef92597634bde77c719f9e3514
---
# Trial result

Invariant explicitly typed scalar loop phis now collapse to their outside value and are removed by existing DCE. Canonical routine benchmark: equal.

## Evidence

## What changed

Added scalar-only self-referential phi recognition in MIR copy propagation, direct MIR regression tests for scalar removal, multiple Float backedges, and changing/String ownership phi retention, plus classic-optimization documentation.

## How it works

The baseline left loop phis with one outside value and self-only backedges because all phi destinations were excluded from the copy map. The retained rule requires an explicit scalar type and exactly one non-self operand, so induction and ownership-carrying changes remain untouched. It is linear in phi source count; residual risk is limited to untested scalar MIR operand variants.

## Algorithm

1. Inspect an explicitly typed scalar phi. 2. Find its sole distinct non-self source. 3. Require every other source to be the phi destination itself. 4. Map the destination to that source. 5. Existing propagation rewrites uses and DCE removes the dead phi.

## Results

### Evidence gates

| Gate | Status |
| --- | --- |
| Focused measurement | passed |
| Exact-candidate repository verification | Passed |
| Canonical routine benchmark | equal |

### Optimization proof

| Case | Command | Property | Baseline | Candidate | Result |
| --- | --- | --- | ---: | ---: | --- |
| Scalar self-referential MIR phi removal | ./run-tests --ai --filter="MIR copy propagation" | A typed Int64 phi with outside source V0 and self backedge V1 is propagated so DCE removes the phi and returns V0; Float multi-backedge equivalent propagates; changing Int64 and String ownership phis remain. | Pinned baseline: 1 passed, 2 failed; scalar and multi-backedge self-referential phi were absent from the copy map. Scalar fixture retained 2 MIR instructions (phi plus return). | Candidate: 3/3 passed; scalar fixture has 1 MIR instruction (return only), removing one phi instruction. | improved |

### Benchmarks

| Benchmark | Metric / command | Before | After | Change | Result |
| --- | --- | ---: | ---: | ---: | --- |
| Canonical routine benchmark | ./benchmarks/run_benchmarks.sh --verify routine | Not measured | Not measured | Not measured | not required |
| ackermann | canonical routine instruction count | 10019036010 | 10019036010 | 0 (0%) | neutral |
| binary_trees | canonical routine instruction count | 649092988 | 649092988 | 0 (0%) | neutral |
| collatz | canonical routine instruction count | 81143030 | 81143030 | 0 (0%) | neutral |
| edigits | canonical routine instruction count | 4528733705 | 4528733705 | 0 (0%) | neutral |
| factorial | canonical routine instruction count | 63968 | 63968 | 0 (0%) | neutral |
| fasta | canonical routine instruction count | 514839063 | 514839063 | 0 (0%) | neutral |
| fib | canonical routine instruction count | 418050941 | 418050941 | 0 (0%) | neutral |
| leibniz | canonical routine instruction count | 850001456 | 850001456 | 0 (0%) | neutral |
| mandelbrot | canonical routine instruction count | 17459524 | 17459524 | 0 (0%) | neutral |
| matmul | canonical routine instruction count | 1968521077 | 1968521077 | 0 (0%) | neutral |
| merkletrees | canonical routine instruction count | 389936167 | 389936167 | 0 (0%) | neutral |
| nbody | canonical routine instruction count | 1394002414 | 1394002414 | 0 (0%) | neutral |
| nqueen | canonical routine instruction count | 221668099 | 221668099 | 0 (0%) | neutral |
| pisum | canonical routine instruction count | 95305 | 95305 | 0 (0%) | neutral |
| primes | canonical routine instruction count | 2045943 | 2045943 | 0 (0%) | neutral |
| quicksort | canonical routine instruction count | 222569985 | 222569985 | 0 (0%) | neutral |
| spectral_norm | canonical routine instruction count | 74939665 | 74939665 | 0 (0%) | neutral |
| sum_to_n | canonical routine instruction count | 71817 | 71817 | 0 (0%) | neutral |
| tak | canonical routine instruction count | 52360584 | 52360584 | 0 (0%) | neutral |

### Focused tests

| Test | Command | Result | What it proves |
| --- | --- | --- | --- |
| Baseline failing MIR cases | ./run-tests --ai --filter="MIR copy propagation" | failed | Pinned baseline produced 1 pass and 2 expected failures for scalar and multi-backedge invariant phis. |
| Candidate MIR phi tests | ./run-tests --ai --filter="MIR copy propagation" | passed | All 3 focused scalar, multi-backedge, changing/ownership cases passed. |
| MIR optimizer suite | ./run-tests --ai --filter="MIR Optimize Tests" | passed | All 21 MIR optimizer tests passed. |

### All tests

| Command | Result | Details |
| --- | --- | --- |
| ./run-tests --ai | Passed | 8450/8450 passed in 1598.0s |

### Trial work items

| Work item | Result | Summary | Evidence |
| --- | --- | --- | --- |
| Trial self-referential phi simplification | improved | Scalar invariant self-referential phis are eliminated after copy propagation and DCE. | Focused MIR proof improved from retained phi plus return to return only; multi-backedge Float propagates, while changing Int64 and String ownership phis remain. |

## Notes

The repository-wide suite and canonical routine benchmark were intentionally not run because the experiment instructions assign those authoritative checks to the worker. The focused deterministic MIR proof establishes one removed instruction and preserved negative cases.

Canonical routine benchmark (pre-review): equal; current/baseline ratio: 1.
