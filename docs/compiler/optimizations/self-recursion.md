# Self-Recursion Loop Lowering

Direct self-recursive tail calls lower to control-flow loops instead of native
calls. This removes call overhead and keeps stack usage constant.

```dark
let sumTo(n: Int64, acc: Int64) : Int64 =
    if n <= 0 then acc else sumTo(n - 1, acc + n)
```

## Lowering

During ANF-to-MIR conversion, a tail call whose target is the current function
becomes parameter updates followed by a jump to the loop header. The function
entry and recursive backedge give the header two predecessors, so SSA
construction inserts typed phis for changed parameters.

```text
entry ──────┐
            v
         header <── backedge
            |
            v
          return
```

Register allocation resolves those phis into predecessor copies. Code
generation then emits the copies and a local branch rather than a call.

## Parallel parameter updates

Recursive arguments can depend on parameters whose locations they replace. A
swap such as `loop(b, a)` therefore has parallel-move semantics. Argument
values are captured before destinations are overwritten, and cyclic register
moves use a temporary location. The same rule applies to integer and floating
point parameters.

## Ownership

Reference-count insertion must prove that cleanup can occur before the
backedge. Managed accumulator replacement transfers ownership into the next
iteration and releases the previous value exactly once. When that proof is not
available, the call stays on the general tail-call path.

Only direct self-recursion uses this loop lowering. Mutual recursion uses the
ordinary tail-call mechanism described in [tail calls](tail-calls.md).

Non-tail native-integer recursion can become a loop when the recursive result
is combined only by modular addition, recursive-left subtraction, or
multiplication. This applies to every signed and unsigned 8-, 16-, 32-, and
64-bit width, including the corresponding pure `Stdlib` operator wrappers.

Linear recursion beneath immutable constructors uses destination passing.
Tuple-backed sum constructors and records allocate their outer block before the
backedge and fill the recursive slot through a compiler-private raw view. List
prepending uses a reverse accumulator and the existing `__reverseInto` kernel,
so it remains linear-time without depending on the skew-list representation.
The transformation requires every self call to have an eligible constructor
boundary and does not move effectful field evaluation across recursion.

Focused behavior is covered by `src/Tests/e2e/tailcall.e2e`,
`src/Tests/e2e/tco-refcounting.e2e`,
`src/Tests/e2e/tail-recursion-modulo-constructor.e2e`, and the optimization
fixtures under `src/Tests/optimization/`.
