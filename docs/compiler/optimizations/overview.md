# Optimization Overview

The compiler performs local and interprocedural simplification across ANF, MIR,
and LIR. The canonical pass order and contracts are in the
[pipeline reference](../pipeline.md); focused before/after fixtures under
`test/fixtures/optimization/` define the retained transformations.

This directory documents optimization mechanisms whose invariants are useful
outside their implementation:

- [Tail-call optimization](tail-calls.md)
- [Self-recursion loop lowering](self-recursion.md)
- [Optimization catalog](catalog.md)

Completed optimization trials and benchmark measurements are not maintained as
a prose catalog. The implementation and focused fixtures describe current
behavior, while generated benchmark results describe current performance.
