# Benchmark Investigation: Primes

## Executive Summary

**Benchmark:** primes (prime counting via trial division)
**Current Dark output:** `1229`
**Current Dark instructions:** 5,443,919
**Cached Rust baseline:** 1,249,930 instructions
**Current Dark ratio:** 4.36x cached Rust baseline

Dark now has several optimizations that older investigation notes still listed
as missing. The current ANF eliminates the repeated `guess + 1` expression in
`isqrt`, rewrites `n % 2 == 0` to a bit test, and the post-register-allocation
LIR for the hot functions uses zero stack frame size with empty save/restore
sets around calls.

The largest remaining opportunities are not register spills or local CSE in
this benchmark. Current evidence points to integer-square-root strategy,
signed modulo semantics overhead, and function-call boundaries in the hot prime
test path.

## Current Evidence

Evidence was gathered on commit `16719658`.

- `./dark benchmarks/problems/primes/dark/main.dark` built `dark.out`.
- `./dark.out` printed `1229`, matching `benchmarks/problems/primes/expected_output.txt`.
- `./dark --dump-anf benchmarks/problems/primes/dark/main.dark` showed current ANF before and after optimization.
- `./dark --dump-lir benchmarks/problems/primes/dark/main.dark` showed LIR before and after register allocation.
- `./benchmarks/run_benchmarks.sh --refresh-baseline=rust primes` measured Dark at 5,443,919 Cachegrind instructions. Rust could not be refreshed because `rustc` is not installed in this sandbox, so the Rust comparison remains the cached 1,249,930-instruction baseline from `benchmarks/RESULTS.md`.

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

## Status Corrections

### ANF CSE for `isqrt` is implemented

The old investigation showed `guess + 1` computed three times in `isqrt`. That
is no longer current. The optimized ANF now computes it once and reuses the
temporary:

```text
Function isqrt:
let TempId 5 = t1 + 1
let TempId 7 = t5 * t5
let TempId 8 = t7 > t0
if t8 then
return t1
else
let TempId 10 = TailCall(isqrt, [t0, t5])
return t10
```

### Evenness strength reduction is implemented

The optimized ANF rewrites the evenness guard from modulo to a bit operation:

```text
Function isPrime:
let TempId 22 = t19 & 1
let TempId 23 = t22 == 0
```

After register allocation, this becomes a direct bit branch:

```text
Label "isPrime_L4":
  BranchBitZero(X19, #0, Label "isPrime_L6", Label "isPrime_L7")
```

### Hot loop register spilling is not the current issue

The older note showed large `SaveRegs` sets and stack traffic around calls.
Current post-register-allocation LIR does not show that pattern for this
benchmark:

```text
isPrime:
  StackSize: 0
  UsedCalleeSaved: [X19, X20]
  Label "isPrime_L7":
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19, X1 <- Imm 1)
    X20 <- Call(isqrt, [Reg X19, Imm 1])
    RestoreRegs([], [])

countPrimes:
  StackSize: 0
  UsedCalleeSaved: [X19, X20, X21]
  Label "countPrimes_L16":
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X20, X1 <- Imm 1)
    X19 <- Call(isqrt, [Reg X20, Imm 1])
    RestoreRegs([], [])
```

There are still local moves at function entries and after calls, but the old
stack-spill diagnosis is stale for this benchmark.

## Remaining Optimization Opportunities

### 1. Replace recursive integer square root with a faster primitive

Dark still computes the square-root limit with recursive trial increments. The
optimized ANF is cleaner, but the algorithmic shape remains O(sqrt(n)) per
candidate:

```text
Function isqrt:
let TempId 2 = t1 * t1
let TempId 3 = t2 > t0
if t3 then
  let TempId 4 = t1 - 1
  return t4
else
  let TempId 5 = t1 + 1
  let TempId 7 = t5 * t5
  ...
  let TempId 10 = TailCall(isqrt, [t0, t5])
```

A hardware-backed `Int64.sqrt` or equivalent compiler/runtime primitive would
remove the recursive `isqrt` loop from this benchmark. This remains the most
direct benchmark-specific gap against the Rust implementation.

### 2. Prove positive divisor ranges to remove general modulo correction

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

### 3. Inline the hot primality helpers

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

## Current Priority

| Opportunity | Status | Priority |
| ----------- | ------ | -------- |
| Hardware-backed integer square root | Not implemented | High |
| Positive-divisor modulo simplification | Not implemented | Medium |
| Hot helper inlining for `isqrt`/`isDivisible` | Not implemented | Medium |
| ANF CSE for repeated `guess + 1` | Implemented | Done |
| Evenness modulo-to-bit-test rewrite | Implemented | Done |
| Remove call-site stack spills in this benchmark | No longer observed | Done |

## Caveats

The Rust baseline could not be refreshed in this sandbox because `rustc` is not
installed. The 4.36x ratio uses the cached Rust number in `benchmarks/RESULTS.md`
and the current local Dark Cachegrind measurement.
