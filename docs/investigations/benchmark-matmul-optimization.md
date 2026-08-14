# Benchmark Investigation: matmul

## Current workload

`matmul` uses the canonical immutable `Dict<Int64, Int64>` matrix
representation. It generates two 100x100 input matrices, materializes the
complete product in a third Dict, and then computes the weighted checksum in a
separate pass. The Dark binary prints the reference checksum `222793267`.

The hot loop performs roughly two million successful calls to
`Stdlib.__HAMT.__getOrDefault`. Previously that entry point called the
Option-returning HAMT traversal and immediately matched the result. Generated
post-register-allocation LIR showed a 16-byte `HeapAlloc` on every terminal
path of `__getHelper`, followed by an Option-tag branch in `__getOrDefault`.

## Direct-default lookup

`__getOrDefault` now calls a dedicated HAMT traversal that returns either the
stored value or the supplied fallback directly. It uses the same generic
`__key_eq` comparison and collision search as `__getHelper`, and the internal
node recursion remains a tail call. The Option-returning lookup is unchanged
for callers that need to distinguish absence.

Post-register-allocation LIR for the direct-default helper has no Option
`HeapAlloc(16)` operations and no caller-side Option-tag branch. Focused E2E
coverage pins existing and missing generic string keys, while the refcounting
suite covers stored and fallback managed values.

## Measurements

The trial was pinned to compiler revision `2cc404d0`. A reduced 3x3 sample fell
from 40,704 to 38,757 instructions, a 4.78% reduction. Full matmul fell from
2,106,152,079 to 2,043,852,076 instructions, a 2.96% reduction; data references
fell from 262,066,410 to 225,916,410 and branches from 316,732,492 to
308,692,491.

The issue's 2,124,300,694 starting count is historical. Intervening unrelated
changes had already reduced the pinned baseline to 2,106,152,079 before this
trial. The other 18 routine benchmark instruction counts were unchanged, and
the rounded aggregate ratio in `RESULTS.md` remains 2.75x. Matmul's individual
ratio improves from 132x to 128x the audited Rust reference.
