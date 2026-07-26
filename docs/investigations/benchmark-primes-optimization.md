# Benchmark Investigation: Primes

## Executive Summary

**Benchmark:** primes (prime counting via trial division)
**Current Dark output:** `1229`
**Current Dark instructions:** 5,443,919
**Current Dark branches:** 1,030,520
**Cached Rust baseline:** 1,249,930 instructions
**Current Dark ratio:** 4.36x cached Rust baseline

The largest remaining opportunities are not register spills or local CSE in
this benchmark. Current evidence points to integer-square-root strategy,
signed modulo semantics overhead, and function-call boundaries in the hot prime
test path. Cachegrind also shows the gap is instruction and branch count rather
than memory traffic: Dark reports fewer data references than Rust, but about
3.4x as many branches.

## Current Evidence

Evidence was refreshed on commit `3312b4d5`.

- `./dark benchmarks/problems/primes/dark/main.dark -o /tmp/primes_dark` built a local binary.
- `/tmp/primes_dark` printed `1229`, matching `benchmarks/problems/primes/expected_output.txt`.
- `./dark --dump-anf benchmarks/problems/primes/dark/main.dark` showed current ANF before and after optimization.
- `./dark --dump-lir benchmarks/problems/primes/dark/main.dark` showed LIR before and after register allocation.
- `./benchmarks/run_benchmarks.sh primes` measured Dark at 5,443,919 Cachegrind instructions, 40,013 data references, 1,030,520 branches, and 1.2% branch misprediction. Rust could not be refreshed because `rustc` is not installed in this sandbox, so the Rust comparison remains the cached 1,249,930-instruction baseline from `benchmarks/RESULTS.md`.

## Benchmark Shape

The Dark benchmark computes primality by checking small guards, recursively
computing an integer square-root limit, and recursively testing divisibility:

```dark
def isqrt(n: Int64, guess: Int64) : Int64 =
    if guess * guess > n then guess - 1
    else if (guess + 1) * (guess + 1) > n then guess
    else isqrt(n, guess + 1)

def isDivisible(n: Int64, d: Int64, limit: Int64) : Bool =
    if d > limit then false
    else if n % d == 0 then true
    else isDivisible(n, d + 1, limit)
```

The Rust implementation uses floating-point square root to compute the limit:

```rust
let limit = (n as f64).sqrt() as i64;
for d in 3..=limit {
    if n % d == 0 {
        return false;
    }
}
```

## Remaining Optimization Opportunities

### 1. Prove positive divisor ranges to remove general modulo correction

`isDivisible` always starts with divisor `3` and increments by `1` until it
passes `limit`, but the generated LIR still emits the general signed modulo path
with a negative-divisor guard and remainder correction:

```text
Label "isDivisible_L1":
  Cmp(X4, Imm 0)
  CondBranch(LT, Label "__modulo_negative_divisor_error_isDivisible", Label "isDivisible_L1_mod_cont_0")
Label "isDivisible_L1_mod_cont_0":
  X1 <- Sdiv(X3, Reg X4)
  X6 <- Msub(X1, X4, X3)
  Cmp(X6, Imm 0)
  X2 <- Cset(NE)
  X1 <- Eor(X6, X4)
  Cmp(X1, Imm 0)
  X1 <- Cset(LT)
  X1 <- And(X2, X1)
  X6 <- Madd(X1, X4, X6)
```

Range information for `d >= 3` would allow this benchmark to use a simpler
positive-divisor modulo sequence. That would not change Dark semantics
globally; it would require a proof that this specific divisor is positive at
the modulo site.

### 2. Inline the hot primality helpers

`countPrimes` already contains inlined guard logic equivalent to part of
`isPrime`, but it still calls `isqrt` and `isDivisible` for odd candidates:

```text
Label "countPrimes_L16":
  X19 <- Call(isqrt, [Reg X20, Imm 1])
  ...
  X19 <- Call(isDivisible, [Reg X20, Imm 3, Reg X19])
```

Inlining alone would not address the square-root algorithm, but it could expose
the positive-divisor fact and other loop-local simplifications to later passes.
This opportunity is secondary to the square-root and modulo improvements.

### 3. Reduce branch work in the hot trial-division path

The current Cachegrind profile reports 1,030,520 Dark branches versus the cached
Rust baseline's 305,748 branches, even though Dark's data references are lower
than Rust's 100,090. That fits the IR and LIR shape: recursive `isqrt`,
recursive `isDivisible`, and general modulo correction all introduce repeated
conditional control flow in the hot path. This is mostly an effect of the
square-root, modulo, and inlining opportunities above, but it is worth tracking
explicitly so future optimization work does not focus on memory traffic for this
benchmark.

## Current Priority

| Opportunity | Status | Priority |
| ----------- | ------ | -------- |
| Positive-divisor modulo simplification | Not implemented | Medium |
| Hot helper inlining for `isqrt`/`isDivisible` | Not implemented | Medium |
| Branch count reduction in trial division path | Not implemented | Medium |

## Caveats

The Rust baseline could not be refreshed in this sandbox because `rustc` is not
installed. The 4.36x ratio uses the cached Rust number in `benchmarks/RESULTS.md`
and the current local Dark Cachegrind measurement.
