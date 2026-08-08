# Spectral Norm Benchmark Investigation

## Current result

Spectral norm now runs the same full n=100, ten-iteration algorithm as the
reference implementations. The direct-payload skew RAL made immutable indexed
vectors practical enough for routine regression coverage:

| Implementation | Instructions | Cachegrind runtime in local evaluation |
|---|---:|---:|
| Dark | 144,271,776 | about 0.4 seconds |
| Rust baseline | 5,093,977 | not remeasured |
| OCaml baseline | 22,589,955 | not remeasured |

The Dark program produces the canonical `1274219991` result. Its previous
three-element tuple implementation produced a Dark-specific result, so the
benchmark processor excluded it from aggregate ratios.

## Immutable implementation

Vectors are `List<Float>` values of fixed length 100. Matrix rows use O(log n)
`getAt`; output vectors start as `List.repeat(0.0, n)` and use persistent O(log
n) `setAt`. Each path-copy update shares unchanged skew subtrees through the
normal reference-counting ownership rules.

This is slower than mutable contiguous arrays, but the full workload is small
enough to run routinely and now measures the compiler behavior that matters:

- float payloads in persistent lists
- logarithmic lookup and path-copy update
- typed RC cleanup of shared list structure
- recursive numeric loops and tuple return values

## Decision

Keep the full immutable implementation in the canonical routine profile. A
future mutable array type could substantially improve its instruction ratio,
but is no longer a correctness or benchmark-coverage prerequisite.
