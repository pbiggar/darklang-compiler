# Compiler compile-time findings

This page records compiler-specific causes found in full host-suite profiles.
The measured revisions, corpus sizes, and phase tables are in the
[historical comparison](../project/test-runtime-history-2026-09-29.md).

## Confirmed sources of excess work

- **SCCP traversal order.** At `313ad16db5`, depth-first propagation through
  large E2E batch callers repeatedly revisited paths: SCCP used 46.81 seconds
  of a 91.25-second suite. The FIFO worklist merge at `797ff13051` reduced
  SCCP to 2.88 seconds on a slightly larger corpus. It remains about three
  seconds in the current pipeline.
- **Call graph staging.** Callee summaries were formerly finalized too late
  for some callers, causing additional pipeline work. The callee-first
  pipeline now publishes each function's final facts before its callers and
  asserts one visit per native pipeline stage for each scheduled function
  node. Summary publication was later changed to merge only new batch facts.
- **Backend clobber analysis.** Before this task, output generation recomputed
  a whole-program register-write fixed point for every binary even though
  callee-first compilation had already produced those facts. A direct probe
  measured 20.84 seconds in 1,378 code-generation calls. Passing the saved
  summaries forward and looking up only direct callees reduced full-suite
  code generation from 22.68 to 4.32 seconds in successive profiles. Four
  reused float-list helpers in the emitted binary had no saved summaries;
  analyzing only those missing bodies preserved their narrow call saves.
- **Checked-unit symbol composition.** A simple user expression carried 2,728
  function-name entries into a base catalog with 4,614 entries, mostly
  reimporting names the base already owned. The old full-suite symbol-import
  phase used 18.11 seconds across 1,712 calls. Composing declarations from
  the checked unit's top levels reduced that phase to 0.04 seconds in the
  same 10,800-test corpus.
- **SSA string rewrites.** The SSA optimizer searched the complete function
  catalog for fixed string helper names on each fixed-point iteration. It
  also built predecessor and use maps for a byte-match rewrite in functions
  with no matching call. Direct name lookup and a call-presence check reduced
  the full-suite SSA optimization phase from 25.26 to 7.48 seconds in
  successive candidate profiles.
- **Call graph initial facts.** Each compilation copied the entire external
  summary catalog before considering which functions it called. Across 1,922
  call graph compilations, setup and initial-fact construction took 3.35 and
  5.14 seconds. Restricting the catalog to direct external callees reduced
  those phases to 0.08 and 0.01 seconds in a subsequent full-suite profile.
- **Sum representation lookup.** Lowering a sum value repeatedly searched the
  entire constructor catalog and sorted candidate cases, even when the sum had
  many cases and could not use the special one- or two-case representation.
  The larger standard library amplified this cost in unchanged functions. In
  complete runs of the same 10,800-test suite, an unchanged large-match
  function fell from 59.62 to 1.68 ms after cases were grouped once by owner;
  cumulative function-expression lowering fell from 3.33 to 1.09 seconds.
  Ownership and call-graph times stayed near their parent values; they remain
  separate costs.
- **Ownership representation queries.** One ownership analysis repeatedly
  classified the same semantic types while constructing, elaborating, and
  verifying its intermediate program. Reusing the classification within that
  analysis reduced its measured full-suite phase from 9.96 to 4.42 seconds.
  Specialized ownership scheduling also need not reanalyze an unchanged
  original caller in each materialization round; body-version tracking now
  asserts that each version is analyzed at most once.
- **Allocation is spread across graph and backend data structures.** In the
  10,800-test parent trace, function identities, function-name maps, ownership
  graph sets, SSA dominator sets, and register clobber sets recur among the
  leading allocation samples. The post-FIFO historical revision used far less
  memory on the same focused HAMT workload. Graph identity conversion,
  compact register-write masks, and a change-driven dominator worklist remove
  some of that structural allocation. Focused allocation fell from 17.53 GB
  on the parent to 12.45 GB after these changes. Ownership group set
  differences and function-name map reconstruction remain visible in the
  focused allocation stacks.
- **The current allocation reduction is real but incomplete.** On the same
  10,800-test corpus, the parent allocated 117.73 GB in a sampled full-suite
  trace and this branch allocated 101.57 GB after the ownership, graph,
  clobber, and SSA changes. The remaining samples still concentrate in
  function identities and function-name and clobber maps. GC suspension did
  not fall reliably between these two traced runs (23.63 versus 22.60
  seconds); host load and collection behavior vary, so allocation reduction
  should not be translated directly into a claimed wall-time saving.
- **Historical allocation causes differ by revision.** The pre-FIFO SCCP
  revision allocates heavily in Boolean path-fact maps and lists. A later
  10,800-test checkpoint allocates heavily in repeated symbol maps, binding
  identities, and backend register sets. These are distinct regressions; a
  high total allocation count alone does not identify the current cause.

An earlier 10,800-test profile took 114.95 seconds, including 21.87 seconds
in call graph compilation, 17.19 seconds in AST-to-ANF conversion, 11.28
seconds in dependency lookup, and 10.09 seconds in SSA optimization. Some
phase totals are nested and cannot be added. The remembered 30-second
full-suite revision has not been verified; the fastest verified historical
checkpoint is `9ddd172a14` at 46.00 seconds for 10,150 tests.
