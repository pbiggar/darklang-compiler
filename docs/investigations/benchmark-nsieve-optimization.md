# NSieve Benchmark Optimization Investigation

## Summary

The `nsieve` benchmark counts primes with the Sieve of Eratosthenes. The
current Dark benchmark now runs the full input `nsieve(100000)`, matching the
reference implementations' input size.

The main optimization blocker is still the benchmark representation: Dark uses
`Dict<Int64, Bool>` as a persistent HAMT-backed composite set, while Rust and
OCaml use mutable contiguous boolean arrays. The current compiler also leaves
the composite-marking recursion as real calls after reference-count insertion,
so further scale increases remain exposed to stack growth.

## Current Local Evidence

Evidence refreshed on commit `f88e41ff`:

- `./dark -r benchmarks/problems/nsieve/dark/main.dark` prints `9592`.
- `benchmarks/RESULTS.md` records Dark at `16,533,914,689` Cachegrind
  instructions for the full-size run.
- The same result table reports Rust at `234,163,043` instructions and OCaml at
  `559,365,264` instructions.
- `./dark --dump-mir benchmarks/problems/nsieve/dark/main.dark` shows
  `markMultiples` still calls `Stdlib.Internal.HAMT.__setHelper_i64_bool` and then
  calls itself before decrementing the new Dict.
- `./dark --dump-lir benchmarks/problems/nsieve/dark/main.dark` shows the same
  post-call reference-count decrement in backend IR, so the self-call is not
  emitted as a tail call.

Relevant MIR shape:

```text
Function markMultiples:
  markMultiples_L1:
    v334 <- v329 + v330 : TInt64
    v461 <- Call(Stdlib.Internal.HAMT.__setHelper_i64_bool, [v332, v329, v460, true, 0])
    v335 <- v461 : TDict (TInt64, TBool)
    v336 <- Call(markMultiples, [v334, v330, v331, v335])
    RefCountDec(v461, size=8, kind=dict)
    ret v336
```

Relevant LIR shape:

```text
markMultiples_L1:
  X23 <- Add(X19, Reg X21)
  X19 <- Call(Stdlib.Internal.HAMT.__setHelper_i64_bool, ...)
  X20 <- Call(markMultiples, [Reg X23, Reg X21, Reg X22, Reg X19])
  RefCountDec(X19, 8, dict)
  X0 <- Mov(Reg X20)
  Ret
```

The current benchmark source documents the full input:

```dark
// Single run
nsieve(100000)
```

## Benchmark Shape

Dark:

```dark
let markMultiples(j: Int64, step: Int64, n: Int64, composites: Dict<Int64, Bool>) : Dict<Int64, Bool> =
    if j > n then composites
    else markMultiples(j + step, step, n, Stdlib.Dict.set<Int64, Bool>(composites, j, true))
```

Rust and OCaml both use dense mutable arrays:

```rust
let mut is_prime = vec![true; n + 1];
is_prime[j] = false;
```

```ocaml
let is_prime = Array.make (n + 1) true in
is_prime.(!j) <- false
```

## Findings

### 1. Mutable Arrays Remain the Optimization with Algorithmic Parity

The sieve algorithm wants O(1) indexed reads and writes over a dense boolean
range. Dark currently models composites as a persistent `Dict<Int64, Bool>`.
Each mark operation goes through HAMT hashing, tagged-node dispatch, allocation,
and path copying, while Rust and OCaml mark a composite with a direct indexed
store.

This is not only a constant-factor gap. For dense integer keys, `Dict` adds tree
walk and allocation work to nearly every composite mark and primality check. A
future mutable array or bitset primitive would let the Dark benchmark express
the same data structure as the baselines.

### 2. Reference Counting Prevents Tail Calls in the Marking Loop

Source-level `markMultiples` is tail-recursive, but current MIR and LIR are not
tail-call shaped after reference-count insertion. The new Dict returned by
`Stdlib.Internal.HAMT.__setHelper_i64_bool` is decremented after the recursive
`markMultiples` call returns.

That post-call cleanup means larger runs still have a stack-depth risk even
before considering HAMT helper recursion. Tail-call preservation across
reference-count insertion would make the benchmark more robust, but it would not
address the larger data-structure mismatch by itself.

### 3. HAMT Path Copying and Allocation Still Dominate the Dark Representation

The current `Stdlib.Internal.HAMT.__setHelper_i64_bool` path allocates leaf/internal
nodes and copies children when updating the persistent Dict. The LIR contains
`RawAlloc` in leaf allocation, collision handling, leaf expansion, and internal
copy helpers such as `Stdlib.Internal.HAMT.__copyInternalWithUpdate_i64_bool`.

Improving Dict path copying could help Dict-heavy programs generally, but it
would still leave `nsieve` using a sparse persistent structure for dense mutable
state.

## Optimization Opportunities

| Opportunity | Expected effect | Caveat |
| --- | --- | --- |
| Add mutable array or bitset primitives | Gives `nsieve` the same dense O(1) mark/check representation as Rust and OCaml | Requires language/runtime/compiler support for allocation, indexing, bounds checks, and lifetime handling |
| Preserve tail calls when RC cleanup follows self-recursive accumulator calls | Reduces stack pressure in `markMultiples` and similar loops | Needs ownership/lifetime proof; does not fix Dict algorithmic overhead |
| Improve HAMT path-copying allocation behavior | Helps current Dict implementation and other Dict-heavy workloads | Secondary for `nsieve`; arrays/bitsets are the parity fix |

## Remaining Uncertainties

- Runtime comparisons are now input-size comparable, but still compare different
  data structures because Dark uses persistent `Dict` while Rust and OCaml use
  mutable arrays.
- Larger stress runs should be rechecked after tail-call or stack changes.
