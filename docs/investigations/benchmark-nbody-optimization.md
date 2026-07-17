# Benchmark Investigation: nbody

## Current Status

`nbody` compiles and runs, but the Dark benchmark is intentionally reduced to
5,000 simulation steps while the Rust and OCaml references run 500,000 steps.
The reduced Dark binary for `benchmarks/problems/nbody/dark/main.dark` prints
the expected scaled energy result `-169020`.

The source models the five-body system with immutable `Body` and `System`
records. Each simulation step applies all ten body pairs, then returns a new
system with five moved bodies.

## Current Evidence

Current post-register-allocation LIR shows that each `applyPair` call computes
the distance with native floating-point operations, including `FSqrt`:

```text
D0 <- FAdd(D0, D1)
D0 <- FSqrt(D0)
D1 <- FMul(D0, D0)
D0 <- FMul(D1, D0)
D10 <- FDiv(D2, D0)
```

The same LIR also shows that `applyPair` allocates two fresh 56-byte `Body`
records and a 16-byte tuple for every pair update:

```text
X3 <- HeapAlloc(56)
...
X1 <- HeapAlloc(56)
...
X2 <- HeapAlloc(16)
HeapStore(X2, 0, Reg X3)
HeapStore(X2, 8, Reg X1)
```

`advanceStep` still calls `applyPair` ten times. The pair results remain
materialized as tuples across call boundaries, with repeated save/restore of
live values around each call:

```text
X1 <- Call(applyPair, [Reg X19, Reg X20, Reg X26])
...
X11 <- Call(applyPair, [Reg X21, Reg X19, Reg X26])
```

The final movement phase is partly scalarized by the compiler: the LIR directly
constructs three moved body records in `advanceStep`, but still calls
`moveBody` for the final two bodies:

```text
X20 <- HeapAlloc(56)
...
X19 <- Call(moveBody, [Reg X19, Reg X26])
X21 <- Call(moveBody, [Reg X21, Reg X26])
```

After the moved bodies are available, `advanceStep` allocates a new 40-byte
`System` record and performs reference-count operations for all five body
fields:

```text
RefCountInc(X25, 56, generic)
RefCountInc(X23, 56, generic)
RefCountInc(X20, 56, generic)
RefCountInc(X19, 56, generic)
RefCountInc(X21, 56, generic)
X26 <- HeapAlloc(40)
```

The recursive `advance` loop is not lowered to a direct loop in the final LIR.
It calls `advanceStep`, then calls `advance` recursively and performs cleanup
after the recursive call returns:

```text
X22 <- Call(advanceStep, [Reg X19, Reg X21])
...
X19 <- Call(advance, [Reg X22, Reg X21, Reg X19])
RefCountDec(X22, 40, generic)
```

Rust and OCaml use mutable body storage for the hot simulation loop. They update
positions and velocities in place, so their inner pair loop does not need to
allocate replacement body records or pair-result tuples for each interaction.

## Durable Optimization Opportunities

### Scalarize Fixed-Shape Records Across Pair Updates

The dominant current gap is that every `applyPair` materializes two updated
`Body` records plus a tuple even though `advanceStep` immediately destructures
the result and passes the records into the next pair update. Escape analysis or
scalar replacement for fixed-shape records would let the compiler keep the
seven float fields for each body in registers or stack slots through the
unrolled five-body step.

For this benchmark, scalar replacement would target at least the twenty updated
body records and ten pair tuples created by the ten `applyPair` calls in each
simulation step.

### Represent Linear State Updates Without Heap Churn

The reference implementations express the benchmark as in-place updates over a
fixed five-body collection. Dark's immutable-record source is semantically
clean, but the current backend pays allocation and reference-count costs for
each intermediate version of the bodies and system.

A compiler optimization for linear record updates, or a future mutable/array
representation that preserves Dark semantics at the language boundary, would
better match this benchmark's hot loop. This is broader than `nbody`: any
numeric benchmark that updates a fixed-size aggregate repeatedly will hit the
same allocation pattern.

### Finish Inlining and Scalarization of `moveBody`

The current LIR already expands three `moveBody`-equivalent record updates
inside `advanceStep`, but two `moveBody` calls remain. Completing that
specialization would remove two calls per simulation step and expose those final
body updates to the same scalar-replacement opportunity as the earlier moved
bodies.

### Preserve Tail Position Through Reference-Count Cleanup

`advance` is source-level tail recursion, but the final LIR still emits a
recursive call followed by cleanup of the previous `System`. That prevents the
loop from becoming a simple branch and keeps stack growth tied to the iteration
count.

Tail-call lowering for owned values needs to account for required
reference-count cleanup before the jump. For `nbody`, performing the cleanup
before the recursive transfer would turn the 5,000-step reduced loop into a
constant-stack loop and is a prerequisite for safely restoring the full
500,000-step input.

## Remaining Uncertainties

This investigation did not prove which allocation class dominates runtime once
tail recursion is fixed. The next useful measurement is an allocation or
instruction-count profile that separates `applyPair` tuple/body churn,
`System` reconstruction, and recursive-loop overhead.
