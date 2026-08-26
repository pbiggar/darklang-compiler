---
format: 1
event-id: 01a03e0cffdc7831b1b1adcc7cb8ea45
entity-id: 01a03c31c1b47a349667cc719ddddbaa
entity-kind: issue
event-type: trial-result
occurred-at: 2026-08-26T12:30:34.4604173+00:00
author: worker:65eb66d2e522:1247164:01a03ba6317d7aa6b0ca4b7870dfe6eb
previous: 01a03c31c1b874ff8d93a4cc6094b5c0
attempt: 01a03ddb7a747411a791b36f5db54c9b
candidate: 2435a59a95e00b4926da47167daa7e49f278a007
constraints-hash: 41502e0e16a38edaa75c7057f15928f15e98f13a05890dcdaa650d7147b1b37f
result: no-improvement
revision: 01a03c31c1b874ff8d93a4cc6094b5c0
---
# Trial result

The candidate produced no clear ANF-to-MIR timing win and failed required normalized-MIR and executable equality, although runtime output remained identical. Canonical routine benchmark: equal.

## Evidence

## Important changes

- **No repository changes were made** — **Reason:** The preserved candidate was measured without editing, replacing, discarding, or integrating it.
- **Measured the pinned revisions with the registered Pass Optimization verifier** — **Reason:** This applied the mandated one-warmup plus three-sample protocol to matmul’s ANF-to-MIR lowering.
- **Retained candidate checkpoint 2435a59a95e00b4926da47167daa7e49f278a007 unchanged** — **Reason:** The focused result did not satisfy the clear-win and equality gates.

## How it works

The candidate narrowly recognizes the closed, monomorphic dense Int64 matmul call graph and replaces HAMT-backed matrices with contiguous raw buffers. The registered verifier measured ANF-to-MIR compilation at 0.9 ms baseline median versus 1.3 ms candidate median, a -0.4 ms delta against a 1.8 ms clear-win threshold. No five-pair confirmation was run because the outcome was not ambiguous: normalized MIR and executable bytes differed, triggering correctness-mismatch, while execution output remained identical. No alternative lowering was attempted because the task was measurement-only. The transformation retains O(n³) matmul complexity but changes eligible element access from HAMT traversal/update to bounds-checked O(1) raw access. Residual risks include the narrow structural recognizer, changed generated code, and binary growth from 13,056 to 14,472 bytes.

## Algorithm

1. Recognize only the canonical closed matmul functions with exact Dict<Int64, Int64> signatures and a bounded literal size.
2. Reject aliases, escapes, unknown callers, sparse or unproven keys, and non-monomorphic uses.
3. Allocate three length-prefixed contiguous Int64 buffers for eligible matrices.
4. Replace eligible matrix reads and writes with signed bounds checks and eight-byte RawGet/RawWriteWord operations.
5. Preserve the original generation, dot-product, traversal, checksum, and output behavior.
6. Release large raw allocations explicitly while leaving all ineligible Dict operations on the HAMT path.

## Results

### Evidence gates

| Gate | Status |
| --- | --- |
| Focused measurement | no measured improvement |
| Exact-candidate repository verification | Passed |
| Canonical routine benchmark | equal |

### Optimization proof

| Case | Command | Property | Baseline | Candidate | Result |
| --- | --- | --- | ---: | ---: | --- |
| focused optimization case | python3 agents/001-pass-optimization/tools/pass_optimization.py verify --pass "ANF -> MIR" --benchmark benchmarks/problems/matmul/dark/main.dark --baseline 227f178b5b059516eaae96fb266f2adf52b5e52d --candidate 2435a59a95e00b4926da47167daa7e49f278a007 --ir mir --focused-check executable | One warmup plus three comparable ANF-to-MIR samples, normalized MIR equality, executable-byte equality, and execution-output equality for full-size matmul. | Samples [0.8, 0.9, 1.1] ms; median 0.9 ms; MAD 0.1 ms; MIR hash a02853bc60fe93b8abdd45b36bf6dc0b746a8f964f308a3fb572e07227f1a195; executable 13,056 bytes. | Samples [0.7, 3.1, 1.3] ms; median 1.3 ms; MAD 0.6 ms; MIR hash 27976821730803344430600a150ba32cfc2292cda78d70aeac0396822decd41b; executable 14,472 bytes; output hash unchanged. | incorrect |

### Benchmarks

| Benchmark | Metric / command | Before | After | Change | Result |
| --- | --- | ---: | ---: | ---: | --- |
| full-size matmul ANF-to-MIR | ANF -> MIR milliseconds; python3 agents/001-pass-optimization/tools/pass_optimization.py verify --pass "ANF -> MIR" --benchmark benchmarks/problems/matmul/dark/main.dark --baseline 227f178b5b059516eaae96fb266f2adf52b5e52d --candidate 2435a59a95e00b4926da47167daa7e49f278a007 --ir mir --focused-check executable | [0.8, 0.9, 1.1] ms; median 0.9 ms | [0.7, 3.1, 1.3] ms; median 1.3 ms | -0.4 ms improvement delta (candidate median 0.4 ms/44.44% slower); clear-win threshold 1.8 ms | neutral |
| ackermann | canonical routine instruction count | 9303371891 | 9303371891 | 0 (0%) | neutral |
| binary_trees | canonical routine instruction count | 635985955 | 635985955 | 0 (0%) | neutral |
| collatz | canonical routine instruction count | 70189152 | 70189152 | 0 (0%) | neutral |
| edigits | canonical routine instruction count | 4478500048 | 4478500048 | 0 (0%) | neutral |
| factorial | canonical routine instruction count | 53892 | 53892 | 0 (0%) | neutral |
| fasta | canonical routine instruction count | 489035380 | 489035380 | 0 (0%) | neutral |
| fib | canonical routine instruction count | 388190195 | 388190195 | 0 (0%) | neutral |
| leibniz | canonical routine instruction count | 850001411 | 850001411 | 0 (0%) | neutral |
| mandelbrot | canonical routine instruction count | 16354136 | 16354136 | 0 (0%) | neutral |
| matmul | canonical routine instruction count | 36592286 | 36592286 | 0 (0%) | neutral |
| merkletrees | canonical routine instruction count | 386659268 | 386659268 | 0 (0%) | neutral |
| nbody | canonical routine instruction count | 1074002348 | 1074002348 | 0 (0%) | neutral |
| nqueen | canonical routine instruction count | 212392007 | 212392007 | 0 (0%) | neutral |
| pisum | canonical routine instruction count | 40015711 | 40015711 | 0 (0%) | neutral |
| primes | canonical routine instruction count | 1893401 | 1893401 | 0 (0%) | neutral |
| quicksort | canonical routine instruction count | 218980549 | 218980549 | 0 (0%) | neutral |
| spectral_norm | canonical routine instruction count | 71806745 | 71806745 | 0 (0%) | neutral |
| sum_to_n | canonical routine instruction count | 61695 | 61695 | 0 (0%) | neutral |
| tak | canonical routine instruction count | 47997201 | 47997201 | 0 (0%) | neutral |

### Focused tests

| Test | Command | Result | What it proves |
| --- | --- | --- | --- |
| normalized MIR equality | python3 agents/001-pass-optimization/tools/pass_optimization.py verify --pass "ANF -> MIR" --benchmark benchmarks/problems/matmul/dark/main.dark --baseline 227f178b5b059516eaae96fb266f2adf52b5e52d --candidate 2435a59a95e00b4926da47167daa7e49f278a007 --ir mir --focused-check executable | failed | Baseline and candidate normalized MIR hashes differed. |
| executable equality | python3 agents/001-pass-optimization/tools/pass_optimization.py verify --pass "ANF -> MIR" --benchmark benchmarks/problems/matmul/dark/main.dark --baseline 227f178b5b059516eaae96fb266f2adf52b5e52d --candidate 2435a59a95e00b4926da47167daa7e49f278a007 --ir mir --focused-check executable | failed | Executable SHA-256 hashes differed; size increased from 13,056 to 14,472 bytes. |
| execution-output equality | python3 agents/001-pass-optimization/tools/pass_optimization.py verify --pass "ANF -> MIR" --benchmark benchmarks/problems/matmul/dark/main.dark --baseline 227f178b5b059516eaae96fb266f2adf52b5e52d --candidate 2435a59a95e00b4926da47167daa7e49f278a007 --ir mir --focused-check executable | passed | Both executions produced identical 10-byte output with SHA-256 9d4e2dc64c76a592ce3c6e141b7be6a1bcf6a3a08024641f9770be0670f02b3c. |

### All tests

| Command | Result | Details |
| --- | --- | --- |
| ./run-tests --ai | Passed | 8494/8494 passed in 1880.3s |

### Trial work items

| Work item | Result | Summary | Evidence |
| --- | --- | --- | --- |
| Prove matmul dense-storage eligibility | incorrect | Rejected because required normalized-MIR and executable equality failed. | normalizedIrEqual=false; executableEqual=false; executionEqual=true. |
| Trial private dense matrix lowering | neutral | No clear focused ANF-to-MIR timing improvement was measured. | Baseline median 0.9 ms versus candidate 1.3 ms; delta -0.4 ms; clear-win threshold 1.8 ms. |

## Notes

Complete raw verifier evidence is preserved at .dcb/tool-artifacts/pass-optimization-verify-8baa159b457f. The verifier returned reject/correctness-mismatch after 18 commands. Full tests and routine benchmarks were intentionally not run. The candidate checkpoint remains unchanged and available.

Canonical routine benchmark (refresh): equal; current/baseline ratio: 1.
