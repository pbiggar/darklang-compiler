# Benchmark Investigation: nbody

## Current Status

`nbody` compiles and runs the canonical 500,000 simulation steps used by the
Rust, OCaml, Node, and Python references. The Dark binary uses the shared
expected output and prints the scaled energy result `-169096`.

The parity gap was compiler/runtime behavior: reference-count cleanup after the
source-level self-tail call kept `advance` as ordinary recursion. The compiler
now recognizes record parameters that are returned self-recursive accumulators,
retains the initial borrowed parameter once, releases each previous record on
the backedge, and transfers the freshly-owned replacement into the next loop
iteration.

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

The recursive `advance` call is now a direct loop in MIR. One-time ownership
setup stays in `advance_entry`; each backedge releases the previous `System`,
updates the parameters, and jumps to `advance_body`:

```text
advance_entry:
  RefCountInc(v163, size=40, kind=generic)
  jump advance_body
advance_L1:
  v167 <- Call(advanceStep, [v163, v164])
  RefCountDec(v163, size=40, kind=generic)
  v163 <- v167
  jump advance_body
```

A targeted canonical Cachegrind run measured Dark at `1,217,501,708`
instructions, or 5.8x the Rust baseline. This is functional parity, while the
remaining performance gap reflects the immutable allocation work below.

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

### Completed: Preserve Tail Position Through Reference-Count Cleanup

`advance` now preserves required reference-count cleanup while lowering the
self-call to a constant-stack loop. The initial borrowed `System` is retained
once at function entry. Each iteration releases the previous accumulator and
transfers the new record's ownership before jumping to the loop header. This
removed the stack-depth blocker and allowed the Dark benchmark to use the full
500,000-step input.

## Remaining Uncertainties

The remaining gap is performance rather than functionality. The next useful
measurement is an allocation or instruction-count profile that separates
`applyPair` tuple/body churn and `System` reconstruction now that recursive-loop
overhead and stack growth have been removed.
