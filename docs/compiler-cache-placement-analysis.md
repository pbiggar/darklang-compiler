# Compiler Cache Placement Analysis

Status: removed. The compiler no longer uses a compile cache; this analysis is
kept for historical context about the removed function cache.

This note evaluates where to compute function cache keys and capture cached
artifacts in the compiler pipeline. It focuses on correctness, invalidation
precision, and opportunities to move caching earlier without missing
dependencies.

## Removed Placement (Post-ANF Optimization + Inlining + RC + TCO)

The removed function cache computed dependency hashes from ANF after:

- ANF optimization
- ANF inlining
- reference count insertion
- tail-call detection
- print insertion (user entry function)

The cache key includes:

- compiler hash and cache version
- options hash (all optimizations and codegen toggles)
- per-function dependency hash

The dependency hash includes:

- function body hash (post-inlining ANF body + signature)
- function type hash (types used in body + registry definitions)
- signature hashes of direct callees (post-inlining call graph)

External functions are hashed via function type + associated record/sum type
definitions.

This placement captured inlining changes (caller body changes due to callee
changes), because the body hash was taken after inlining. It also represented
post-optimization ANF, and the key covered RC-related type dependencies.

## What Drives Correctness

To safely reuse cached LIR for a function, the cache key must account for:

- the function body after inlining (callee changes can change the caller body)
- any type definitions referenced by the function
- calling conventions and codegen options (encoded via options hash)
- any external function dependencies

If any of these change without invalidation, the cached function could be
incorrect or ABI-incompatible.

## Earlier Placement: Options and Risks

### 1) Pre-Inlining (Still Post-ANF Optimization)

If we hash pre-inlining ANF bodies, we must ensure that inlining decisions are
accounted for. Inlining depends on callee bodies (size, recursion, closure
presence), so either:

- we still run inlining to compute a post-inline body hash, which removes any
  savings, or
- we include direct callee signatures in the dependency hash, which still does
  not capture inlining decisions without running the inliner.

### 2) Pre-ANF Optimization (Post-ANF Conversion)

If we hash pre-optimization ANF and rely on deterministic optimization, then
changes in the pre-opt IR plus the options hash uniquely determine the
optimized result. This can be correct, but:

- it increases invalidations because pre-opt bodies include dead/duplicate
  code that the optimizer would remove, and
- it still does not resolve inlining dependencies without either running the
  inliner or conservatively hashing callees.

### 3) Pre-RC Insertion

The removed type hash included types used in the function body and type
definitions from registries. Part of that type usage was inferred from the RC
insertion type map. Moving earlier would have required a new, deterministic
type-use analysis that did not depend on RC insertion.

Without a replacement type map, pre-RC hashing risks missing type
dependencies introduced by temp values or inferred return types, which is
unsafe.

### 4) Pre-ANF (Typed AST Level)

At the typed AST level, we can compute function signatures and type
dependencies, but we do not have stable ANF bodies. Any cache keyed at this
level would need a different cached artifact (e.g., AST/ANF), not the current
LIR. This is effectively a separate cache design.

## Summary of Tradeoffs

Post-inlining hashing maximizes correctness and minimizes invalidation, at the
cost of computing ANF inlining for all functions (which is already required to
produce cached LIR for misses).

Earlier hashing is possible but either:

- requires conservative dependency inclusion (more invalidations), or
- requires a new type map analysis to avoid missing type dependencies, or
- needs a different cached artifact (pre-ANF or pre-opt IR).

Given the removed cache artifact (LIR post-regalloc), the safest and most
precise placement was post-inlining and post-ANF optimization.
