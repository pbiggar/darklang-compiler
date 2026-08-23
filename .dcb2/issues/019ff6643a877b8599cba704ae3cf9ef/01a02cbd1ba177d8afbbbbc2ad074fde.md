---
format: 1
event-id: 01a02cbd1ba177d8afbbbbc2ad074fde
entity-id: 019ff6643a877b8599cba704ae3cf9ef
entity-kind: issue
event-type: trial-result
occurred-at: 2026-08-23T03:49:46.0172766+00:00
author: worker:65eb66d2e522:1481282:01a02c2a368573bc9b39df6a4ae424df
previous: 019ff7d76bb87b5a84a532502768a56f
attempt: 01a02cb491287174a992e76920aae5f8
constraints-hash: c47b052f83d6afafc252bb19e846a447c9b1a182f0529fba2e72f8dfe7bf0e76
result: no-improvement
revision: 019ff7d76bb87b5a84a532502768a56f
---
# Trial result

The isolated precomputed-callee candidate regressed the focused fasta compilation measurement, so it was not retained or integrated.

## Evidence

## What changed

Experimented only in analyzeEffectFreeFunctions by pairing each locally effect-free candidate with one precomputed direct-callee set; no change was retained.

## How it works

Current main still rebuilt every candidate's direct-callee set during each fixed-point iteration. The candidate preserved the generated binary and passed the narrow recursive effect-free LICM test, but 10 comparable fasta compiler runs regressed both MIR Optimizations and total compilation medians. Per bounded-trial guidance, exhaustive suite, routine benchmark, and 10 full-suite timing matrices were not run after this negative signal.

## Algorithm

For each locally effect-free function, compute its direct-callee Set once; carry (function, callees) through fixed-point iterations; test that stored set against the current proven-name set instead of rescanning CFG instructions.

## Results

### Evidence gates

| Gate | Status |
| --- | --- |
| Focused measurement | no clear improvement |
| Exact-candidate repository verification | Not run |
| Canonical routine benchmark | missing |

### Benchmarks

| Benchmark | Metric / command | Before | After | Change | Result |
| --- | --- | ---: | ---: | ---: | --- |
| fasta | MIR Optimizations, 10 x ./dark --allow-internal -vv benchmarks/problems/fasta/dark/main.dark | raw ms [66.7,83.7,41.2,59.4,47.1,74.4,65.7,51.5,75.6,69.7]; median 66.20 | raw ms [72.9,82.0,88.5,90.4,74.2,65.9,64.7,78.1,67.7,80.4]; median 76.15 | +9.95 ms (+15.03%) | regressed |
| fasta | total compilation, same 10-run matrix | raw ms [612.7,697.6,380.4,567.9,437.0,687.7,574.7,493.7,711.2,636.0]; median 593.70 | raw ms [686.5,759.9,797.3,746.6,690.3,607.0,601.0,797.6,589.5,681.4]; median 688.40 | +94.70 ms (+15.95%) | regressed |
| routine benchmark profile | ./benchmarks/run_benchmarks.sh --verify routine | not run after focused regression | not run after focused regression | not applicable | not required |
| full test-suite timing | 10 x ./run-tests --ai wall-clock | not run; canonical repository-wide suite is worker-owned under inherited trial guidance | not run | not applicable | not required |

### Focused tests

| Test | Command | Result | What it proves |
| --- | --- | --- | --- |
| recursive effect-free LICM | ./run-tests --ai --filter=licm_hoists_recursive_effect_free_scalar_call | passed | Preserves classification needed to hoist a recursive effect-free direct call. |
| generated binary equality | cmp /tmp/dcb-fasta-baseline-1 /tmp/dcb-fasta-candidate-1 | passed | Baseline and candidate fasta binaries were byte-identical. |

### All tests

| Command | Result | Details |
| --- | --- | --- |
| ./run-tests --ai | Not run | The no-improvement trial left uncommitted changes, so no candidate was retained: M src/DarkCompiler/passes/3.5_MIR_Optimize.fs |

### Trial work items

| Work item | Result | Summary | Evidence |
| --- | --- | --- | --- |
| precompute-mir-effect-free-call-sets | regressed | Precomputed candidate callee sets but regressed fasta MIR optimization median by 15.03%. | 10-run fasta MIR median: 66.20 ms baseline versus 76.15 ms candidate; binary identical; focused LICM test passed. |

## Notes

Baseline profiling confirmed the repeated direct-callee CFG scans exist structurally. Raw timing logs are retained under .dcb/tool-artifacts/. The bounded focused timing result was clearly regressive; no temporary instrumentation remains and no commit was made.
