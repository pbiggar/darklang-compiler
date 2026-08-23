# Pisum Benchmark Investigation

## Outcome

The apparent 500x Dark advantage was real generated-code behavior, but it was
not a fair measurement of the intended workload. Both sources describe 500
identical reciprocal-square sums and expose only the final sum. Dark's
effect-free direct-call LICM correctly observed that every invocation of
`innerSum(1, n, 0.0)` returns the same scalar and moved that call outside the
outer tail-recursive loop. The resulting binary performed one sum and measured
95,305 Cachegrind instructions. The audited Rust binary retained all 500 sums
and measured 50,258,602 instructions.

This is legal optimization of the program's observable semantics, not a
compiler miscompile. It nevertheless defeats a microbenchmark whose stated
purpose is repeated floating-point work. The previously recorded 55,014,671
Dark count was also wrong for the audited source: a fresh routine run reproduced
95,305, so that baseline had no matching generated artifact.

## Workload Repair

The Dark full and quick sources now make each round depend on the preceding
result while preserving Rust's exact reset-to-zero arithmetic for the fixed
positive workload:

```dark
let pisum(rounds: Int64, n: Int64, lastResult: Float) : Float =
    if rounds <= 0 then
        lastResult
    else
        let initialResult = if lastResult >= 0.0 then 0.0 else lastResult in
        pisum(rounds - 1, n, innerSum(1, n, initialResult))
```

The first round starts from `0.0`; every reciprocal-square sum is positive and
finite, so every subsequent round also takes the `0.0` branch. Thus all 500
rounds use the same initial accumulator and arithmetic as Rust. The fallback is
semantically observable for a negative or NaN prior result, which keeps the
call operand loop-carried and prevents LICM from moving `innerSum` to the
preheader. `benchmarks/PARITY.json` locks both repaired Dark sources and their
unchanged expected outputs.

## Current Comparable Record

On ARM64, a focused Cachegrind measurement of the repaired Dark source records
45,016,783 instructions, compared with the audited Rust reference's 50,258,602:

| Runtime | Instructions | Relative to Rust |
| --- | ---: | ---: |
| Rust | 50,258,602 | 1.00x |
| Dark | 45,016,783 | 0.90x |

Dark therefore uses about 10% fewer instructions after both programs execute
500 rounds. That smaller advantage is credible: Dark's tail-call lowering
produces compact loops with little runtime setup, while Rust's checked-in
`rustc -C opt-level=3` binary retains both counted loops and standard formatting
runtime overhead. The canonical full-suite Dark/Rust geometric ratio is 3.14x;
that aggregate is distinct from Pisum's 0.90x result.

## Compiler Evidence

Before the repair, optimized MIR placed the call in the function entry and left
only the countdown in the loop:

```text
pisum_entry:
  v10032 <- Call(innerSum, [1, v12, float[0]])
pisum_L1:
  v10031 <- v10029 - 1
```

After the repair, the initial accumulator is selected from the loop-carried
previous result and `innerSum` remains in the loop body:

```text
pisum_L1:
  v10035 <- v10031 >= float[0]
  branch v10035 ? pisum_L3 : pisum_L4
pisum_L5:
  v10036 <- Phi([(v10045, pisum_L4), (float[0], pisum_L3)])
  v10038 <- Call(innerSum, [1, v10032, v10036])
```

The Rust ARM64 assembly similarly keeps its inner loop beneath a comparison of
the outer counter with 500. The two binaries now execute the intended repeated
work instead of relying on source-level loop counts alone.

## Evidence Commands

```bash
python3 benchmarks/infrastructure/benchmark_parity.py check-profile routine
python3 benchmarks/infrastructure/benchmark_baseline.py validate \
  --benchmarks-dir benchmarks --architecture "$(uname -m)" --profile routine
./dark --dump-mir benchmarks/problems/pisum/dark/main.dark
./run-tests --ai
./benchmarks/run_benchmarks.sh --verify routine
```
