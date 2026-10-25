# Remaining SSA boundary work

The production path in `src/driver/ANFPipeline.ml` converts structured ANF
before reference-count insertion. Expression optimization, inlining,
higher-order/direct-call specialization and escape analysis already operate on
high-level SSA; block-local reference counts are inserted afterward. The
[pipeline](../compiler/pipeline.md) and [IR documentation](../compiler/intermediate-representations.md)
own the implemented architecture.

## Remaining migration

- Complete ownership-aware reference counting across managed joins and loops,
  beyond the block-local insertion path. Define each edge argument as an owned
  transfer or a verified borrow. Verify each predecessor independently; a
  borrow's owner must survive through the successor's last use. Preserve
  path-local releases, returned accumulators, record reuse, closure captures,
  raw slots and consuming terminal output.
- Move remaining structured-ANF reachability, generated output and accumulator
  helper work to the SSA boundary as their invariants become expressible.
  Keep the accumulator rewrite separate until direct construction can emit its
  generated helpers. Do not run one fixed-point iteration across two IRs.
- Construct SSA directly from checked lowering, including expressions, lambdas,
  monomorphization, list regions and ownership variants. Remove the converter,
  structured ANF production paths and obsolete dumps/fixtures only after all
  compiler entry points migrate. External templates and cache identities must
  migrate with their consumers.

## Invariants and acceptance

SSA definitions dominate uses; edge arguments match ordered typed block
parameters. Every value has one definition and every block has one terminator.
SSA identity, effects, alias facts and ownership contracts remain distinct.
Maintain independent SSA and ownership verifiers after construction and
transformations. Preserve exact evaluation order, self-recursive parameter
swaps, Float loop values and AOT match diagnostics.

Recover a type at each definition before freshening identities. Use-site facts
for tuple/raw projections and closure returns must survive; unresolved types
must resolve before MIR lowering. Do not introduce an SSA-to-ANF round trip or
a second production pipeline to avoid completing an ownership migration.

Keep HIR analyses that supply independent semantic or list-region proofs. Move
whole-function ownership work to the production graph where that removes
duplicate analysis; discard a general HIR representation if it becomes only a
second copy of the same function. Re-establish proofs after cloning or
monomorphization changes functions.

Each slice must cover user, expression, stdlib, preamble and package entry
points, remove its superseded path, and satisfy the
[verification policy](../contributing/verification.md). Measure graph/analysis
cost and generated-code performance against the task parent.
