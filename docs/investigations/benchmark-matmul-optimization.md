# Benchmark Investigation: matmul

## Current Status

`matmul` currently compiles and runs as a 3x3 Dark benchmark. The benchmark
source still documents that it uses hardcoded small matrices because nested list
support is not ready for the full 100x100 reference problem.

Current `./benchmarks/run_benchmarks.sh all` measured the reduced-size Dark
benchmark at 51,485 instructions, then omitted that number from
`benchmarks/RESULTS.md` because `matmul` is classified as a reduced-size
benchmark. The results table records the reference implementations at:

| Implementation | Instruction count |
|----------------|-------------------|
| Rust           | 16,956,533        |
| OCaml          | 34,895,082        |
| Python         | 894,891,413       |
| Node           | 487,674,868       |

The current Dark binary for `benchmarks/problems/matmul/dark/main.dark` prints
the expected checksum `342786`.

## Current Evidence

The Dark implementation computes each matrix element through `dot3`, and each
`dot3` call performs six `matGet` calls:

```dark
matGet(a, i, 0) * matGet(b, 0, j) +
matGet(a, i, 1) * matGet(b, 1, j) +
matGet(a, i, 2) * matGet(b, 2, j)
```

Current ANF shows that `matGet` remains a nested list lookup followed by option
matching:

```text
Function matGet:
let TempId 585 = Stdlib.List.getAt_list_i64(t582, t583)
...
let TempId 593 = Stdlib.List.getAt_i64(t592, t584)
```

Current ANF also shows each `dot3` body keeps the six `matGet` calls rather
than specializing the known 3x3 access pattern:

```text
Function dot3:
let TempId 609 = matGet(t605, t607, 0)
let TempId 610 = matGet(t606, 0, t608)
let TempId 612 = matGet(t605, t607, 1)
let TempId 613 = matGet(t606, 1, t608)
let TempId 616 = matGet(t605, t607, 2)
let TempId 617 = matGet(t606, 2, t608)
```

Current LIR confirms that top-level constant calls start to specialize after the
first `dot3` result is expanded, but later matrix elements still call `dot3`:

```text
v12113 <- Call(matGet, [Reg v12102, Imm 0, Imm 0])
...
v12124 <- Call(dot3, [Reg v12054, Reg v12102, Imm 0, Imm 1])
```

The latest post-register-allocation LIR also shows that the compiler inlines
the first `matGet` shape into `dot3`, but leaves generic option-match failure
arms in the generated control flow even though the source match covers both
`Some` and `None`:

```text
Label "dot3_L1":
  RuntimeError("Non-exhaustive match: No matching case found for value <unknown> in match expression")
...
Label "dot3_L10":
  RuntimeError("Non-exhaustive match: No matching case found for value <unknown> in match expression")
```

Rust and OCaml solve a different, larger benchmark shape: both allocate and
multiply 100x100 matrices with indexed array/vector access. Dark's current
source uses nested immutable lists and calls `Stdlib.List.getAt` for each access.

## Durable Optimization Opportunities

### Add an Array Type for Matrix Benchmarks

The highest-impact gap remains the lack of an array-backed Dark benchmark shape.
Rust uses `Vec<Vec<i64>>`, OCaml uses `Array.make_matrix`, and both perform
indexed loads and stores inside loops. Dark currently has no equivalent
contiguous matrix representation for this benchmark.

An `Array<T>` representation with indexed load/store operations would let the
Dark benchmark express the same 100x100 algorithm as the reference
implementations and would remove the nested `Stdlib.List.getAt` traversal from
the hot path.

### Specialize Known Small List Access

For the current 3x3 fallback benchmark, the matrix literals and all `dot3`
indices are statically known. Current ANF and LIR still preserve generic
`matGet` and `Stdlib.List.getAt` calls for many accesses.

A small-list specialization pass could replace known accesses into literal
lists with direct values or direct field loads. This would specifically improve
the current fallback benchmark while the larger array-backed version is not yet
available.

### Unbox Option Results for List Access

`matGet` immediately matches on the result of `Stdlib.List.getAt`. That keeps
the option value on the critical path for every matrix element access. If
`Option<Int64>` can be represented without heap allocation across the
`getAt`/match boundary, list-heavy benchmarks should benefit even when they do
not become array-backed.

### Remove Dead Match-Failure Blocks After Exhaustiveness

The current `matGet` and partially inlined `dot3` LIR still contain
`RuntimeError("Non-exhaustive match...")` blocks for option matches that are
exhaustive in the source program. Removing these unreachable arms after
type-checking or before code generation would reduce generated code size and
branch structure for small pattern-heavy benchmarks. This is probably secondary
to array support and list-access specialization for total instruction count,
but it is a concrete code-generation cleanup visible in the current matmul IR.

## Remaining Uncertainties

The current results table intentionally omits the reduced-size Dark instruction
count, so this investigation should not use the apparent Dark-vs-reference ratio
as optimization guidance. The next useful benchmark change is making the Dark
program solve the same 100x100 workload as Rust and OCaml.
