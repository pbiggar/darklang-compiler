# Tail-Call Optimization

Tail-call optimization replaces a call in return position with a jump, keeping
stack use constant. Completion-sensitive interpreter compatibility and
recursive ownership are recorded in the
[recursion compatibility ledger](../../compatibility/language/recursion.md).

## Detection

Pass `TailCallDetection.fs` recognizes direct, indirect, and closure calls
whose result is returned without further computation:

```fsharp
Let (resultId, Call(target, args), Return (Var resultId))
```

The pass runs after reference-count insertion. It may move cleanup before the
tail call only when doing so cannot release a call argument. Otherwise the call
remains ordinary because cleanup after a jump would be unreachable.

The resulting ANF cases are `TailCall`, `IndirectTailCall`, and
`ClosureTailCall`. Direct calls lower to `B` on ARM64 and `JMP` on x86-64;
indirect calls lower to `BR` and indirect `JMP`, respectively.

Call-graph reachability and dead-code elimination must treat all three
tail-call forms as calls. Otherwise a reachable target could be removed even
though the generated jump still references it.

## Argument moves

Tail calls reuse the current frame while replacing argument locations. Those
assignments are parallel moves: swaps and longer cycles must not overwrite a
source before it is read. `TailArgMoves` first emits safe non-register sources,
then acyclic register moves, and finally breaks cycles with a temporary
register.

Direct self-recursive tail calls can instead become local control-flow loops;
see [self-recursion loop lowering](self-recursion.md).

## Ownership

A tail call returns directly to the current caller, so the current function
must finish all of its ownership work before jumping. Cleanup that overlaps a
tail-call argument blocks the transformation unless a more precise ownership
rule proves a transfer. Record-accumulator loops, for example, retain the
initial borrowed value, release each replaced value, and transfer the new owned
value across the backedge.

## Validation

`test/fixtures/e2e/tailcall.e2e` covers direct recursion, argument cycles,
higher-arity calls, generics, managed arguments, and floating-point arguments.
`test/fixtures/e2e/tco-refcounting.e2e` covers cleanup and constant-stack managed
loops. Focused cleanup-ordering tests live in
`test/compiler-passes/TailCallDetectionTests.ml`.
