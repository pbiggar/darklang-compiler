# Binary Trees Benchmark Investigation

## Current Status

Dark now performs the canonical allocation workload: each of 100 iterations
constructs a complete recursive tree of depth 15, traverses all 65,535 nodes,
and releases the complete tree before the next iteration. The result remains
`6553500`.

The old executable path called `countTree(depth)` directly. Although a
tuple-returning `makeTree` function existed in the file, it collapsed each
child to integer fields and was never called by `stressTest`. The measured Dark
program therefore ran only the node-count recurrence and did not exercise tree
allocation, traversal, or reclamation like the Rust and Python references.

## Canonical Source Shape

The benchmark uses the direct recursive algebraic data type and separates
construction from traversal:

```dark
type Tree<a> = Leaf of a | Node of (Tree<a>, Tree<a>)

def makeTree(depth: Int64) : Tree<Int64> =
    if depth <= 0 then Leaf(1)
    else Node((makeTree(depth - 1), makeTree(depth - 1)))

def countTree(tree: Tree<Int64>) : Int64 =
    match tree with
    | Leaf(_) -> 1
    | Node((left, right)) -> 1 + countTree(left) + countTree(right)
```

`stressTest` binds `tree = makeTree(depth)` and passes that value to
`countTree` on every iteration. This is the obvious implementation of the
intended algorithm; it does not encode the tree as an integer recurrence,
closure, precomputed count, or compiler-specific alternate structure.

## Compiler Ownership Defect

Reference-count shape construction previously expanded sum payload types as an
unbounded tree. Classifying `Tree<Int64>` expanded `Node`'s tuple payload, then
both child `Tree<Int64>` types, then their `Node` payloads, and so on until the
compiler process overflowed its stack.

The finite ownership representation now records a typed `RecursiveSumRef` when
classification reaches a sum already on the current expansion path. Its
release plan becomes `RecursiveRelease(Tree<Int64>)`. This is an explicit
typed back-edge rather than a sentinel or a guessed shallow payload.

ARM64 and x64 code generation collect those back-edges and emit one recursive
decrement helper per concrete sum type. A normal root release still decrements
and reclaims the sum and its tuple payload. When it reaches a child back-edge,
it calls the shared helper, which repeats the same tag-sensitive plan for that
child. Thus the finite compiler data structure describes an arbitrarily deep
runtime tree without unrolling code by tree depth or leaking descendants.

## Validation Notes

The language regression constructs and counts trees from depth zero through
four. A compiler-pass test separately checks that `Tree<Int64>` produces one
typed recursive release back-edge. The quick depth-10/five-iteration program
prints `10235` with leak checking enabled and no stderr, and the full
depth-15/100-iteration program prints `6553500` with leak checking enabled and
no stderr.

The quick workload now executes 1,078,843 instructions, up from 45,173 for
the arithmetic-only program. That increase is the intended cost of allocating,
traversing, and releasing five depth-10 trees rather than skipping the benchmark
algorithm. DCB records canonical routine results only after rebasing the exact
integration commit.

## Remaining Optimization Opportunities

Recursive sums whose cycle crosses list, dict, closure, or dynamic-buffer
payloads still need equivalent ARM64 recursive-helper lowering before those
more complex shapes can use this path. Binary trees contains only generic sum
and tuple roots with immediate leaves, so its ownership plan is fully covered.
The benchmark now exposes real allocation, recursive traversal, reference-count
traffic, and destructor-call costs for later general compiler optimization.
