# In-place mutation optimization checklist

This checklist tracks the work from ownership proofs to compiler-selected
in-place updates across calls and recursive functions. Check an item only when
its implementation and relevant validation are complete. The implemented
representation and safety boundaries are documented in
[compiler-selected list arrays](../compiler/runtime/list-array-reuse.md).

- [x] **Finish ownership-variant selection.** Choose a compatible function
  variant from the ownership facts at a call site, treat mutually recursive
  functions as one group, and retain the established call when no variant
  applies. Implemented in
  [SelectOwnershipVariants](../../src/DarkCompiler/passes/ownership/SelectOwnershipVariants.fs)
  with canonical structural identities and ownership-contract tests. Full host
  tests and the parent-relative benchmark gate pass; production scheduling is
  a later slice.
- [x] **Materialize selected variants.** Generate deterministically named
  specialized functions, deduplicate equivalent selections, and rewrite
  selected external calls and internal recursive calls consistently. Reject
  incomplete groups and conflicting contracts; return an explicit plan for
  later scheduling and caching. Implementation:
  [MaterializeOwnershipVariants](../../src/DarkCompiler/passes/ownership/MaterializeOwnershipVariants.fs).
  The pass verifies both original and materialized programs. Full host tests
  and the parent-relative benchmark gate pass; production scheduling remains a
  later slice.
- [ ] **Expose call-site ownership facts.** Report which arguments are provably
  unique immediately before each call, accounting for aliases, branches,
  duplication, and escapes.
- [ ] **Schedule whole-function ownership analysis.** Produce ownership-aware
  HIR for real functions, establish borrowed and consumed parameters and
  ownership-transferring results, and insert duplication and cleanup. Preserve
  compile-time validation, generated result printing, and observable evaluation
  order.
- [ ] **Schedule specialization across functions.** Connect analysis,
  selection, and materialization; propagate uniqueness information until the
  process stabilizes. Bound generated code and compilation work, and cache
  specializations by their bodies, contracts, and dependencies. These
  foundation stages alone are not expected to improve runtime performance.
- [ ] **Carry ownership through lowering.** Preserve contracts through calls,
  returns, branches, tail calls, ANF, and native code generation. Integrate with
  reference counting so ownership is neither duplicated nor lost.
- [ ] **Allow optimized storage across function boundaries.** Let compatible
  specialized functions accept and return compiler-selected arrays. Define
  representation compatibility explicitly, preserve the public language
  interface, and justify the cost of any conversions.
- [ ] **Enable static in-place updates across calls.** Use proven uniqueness
  to reuse storage for supported operations, starting with existing
  `List<Int64>` map and reverse operations. This is the first milestone where
  specialization should produce measurable runtime gains.
- [ ] **Handle dynamically shared values.** Check runtime uniqueness for
  consumed values without a static certificate, update exclusive storage in
  place, and copy otherwise. Protect every surviving alias and preserve the
  copy path's semantics.
- [ ] **Add general constructor reuse.** Safely dismantle old values and reuse
  suitable storage for new constructors. Specialize destruction and release
  child references correctly, extending reuse to records and other supported
  data.
- [ ] **Support managed elements and observable destruction.** Extend reuse to
  collections containing managed values and broader function bodies. Track
  destruction effects so cleanup preserves Stream finalizers and other
  observable actions.
- [ ] **Improve growing collections and recursive construction.** Add builders,
  capacity growth, additional constructors, and profitable large-buffer pooling.
  Combine reuse with suitable tail-recursion-modulo-constructor transformations
  to avoid intermediate allocations and unnecessary stack growth.
- [ ] **Make optimization decisions profitable.** Account for allocations,
  copying, conversions, runtime checks, code size, and compilation cost. Extend
  eligibility to more operations and callbacks as their contracts become
  expressible and verifiable.
- [ ] **Complete validation and rollout.** At every slice, use focused E2E
  tests before language behavior changes, independent ownership verification,
  alias and recursion tests, leak checks, and destruction-order tests. Validate
  each native architecture on appropriate hosts, and require the full
  parent-relative benchmark gate plus targeted allocation and wall-time
  measurements for performance-changing slices. Follow the
  [verification policy](../contributing/verification.md).

Completion means ordinary programs automatically benefit from safe storage
reuse across calls and recursion, with correct fallbacks for sharing and
unsupported cases, bounded compilation costs, and demonstrated performance
improvements.
