# SSA optimization cases

[`ssa.opt`](ssa.opt) uses the same plain-text function syntax as the SSA
inlining fixtures. `---OPTIMIZE-SSA---` runs the production SSA optimizer;
`---NO-INLINE---` checks its result before inlining. Each case states the
operation or CFG effect that matters. For example, the tree-tag case loads a
record field before a branch and again in a dominated successor, then checks
that only one load remains. The two-call case checks that effectful calls stay
separate.

The runtime and ownership consequences are covered by E2E cases. In
particular, `strings.e2e` checks both match-arm orders for checked byte access,
including negative, valid, and end-boundary indices. The full benchmark gate
covers the resulting loop code for binary trees, fasta, myers_diff, nbody,
raytracer, and the other application workloads.
