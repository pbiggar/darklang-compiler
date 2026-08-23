# Pisum Benchmark Investigation

## Outcome

The apparent Dark advantage was not real. The historical Dark implementation
performed one `1 / k^2` summation, while the Rust reference performed 500. The
two programs produced the same final value because every round resets the
accumulator and only the last result is printed, but they did not perform the
same work. A 95,305-instruction Dark measurement therefore could not be
compared with Rust's 50,258,602-instruction measurement.

The audited Dark full and quick sources now perform the same number of rounds
as their Rust counterparts, return the last-round sum, and compute the square
as an `Int64` before converting it to `Float`. `benchmarks/PARITY.json` locks
both source pairs and their shared expected outputs.

## Current Comparable Record

The ARM64 routine snapshot records the following Cachegrind instruction counts:

| Runtime | Instructions | Relative to Rust |
| --- | ---: | ---: |
| Rust | 50,258,602 | 1.00x |
| Dark | 55,014,671 | 1.09x |

Dark is therefore about 9% slower than the audited Rust reference, not faster.
The canonical suite ratio in `benchmarks/RESULTS.md` is **3.17x**; it is the
geometric ratio across the full routine suite and is not a Pisum-only result.

## Equivalent Workload Shape

Both full implementations execute 500 rounds of an inclusive sum over
`k = 1..10000` and print the last round scaled by `10^12`.

```dark
let innerSum(k: Int64, n: Int64, acc: Float) : Float =
    if k > n then acc
    else
        let square = k * k in
        innerSum(k + 1, n, acc + 1.0 / Stdlib.Int64.toFloat(square))

let pisum(rounds: Int64, n: Int64, lastResult: Float) : Float =
    if rounds <= 0 then lastResult
    else pisum(rounds - 1, n, innerSum(1, n, 0.0))
```

The Rust reference expresses the same integer-first denominator as
`((k * k) as f64)`. The fixed range makes the integer product safe:
`10000 * 10000` is within `Int64`.

## Compiler Evidence

The current ANF dump preserves the intended denominator order:

```text
let TempId 4 = t0 * t0
let TempId 7 = Int64ToFloat(t4)
let TempId 8 = 1 / t7
```

Tail-call detection lowers `innerSum` to a loop, so ordinary recursive-call
overhead is not paid for every inner iteration. Remaining performance work is
separate from fairness: investigate loop-carried floating-register copies and
the 500 outer calls only with a fresh comparable benchmark measurement.

## Evidence Commands

```bash
python3 benchmarks/infrastructure/benchmark_parity.py check-profile routine
python3 benchmarks/infrastructure/benchmark_baseline.py validate \
  --benchmarks-dir benchmarks --architecture "$(uname -m)" --profile routine
./dark --dump-anf benchmarks/problems/pisum/dark/main.dark
./benchmarks/run_benchmarks.sh --verify routine
```

The first two commands validate the audited workload and canonical snapshot;
the final command recompiles and measures the complete routine profile without
altering tracked benchmark records.
