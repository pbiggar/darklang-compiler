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
- [x] **Expose call-site ownership facts.** Report which arguments are provably
  unique immediately before each call, accounting for aliases, branches,
  duplication, and escapes. Implemented by the shared ownership verifier and
  exposed through joint typed-HIR analysis, with recursive-contract, alias,
  failure-atomicity, and specialization-handoff tests. Full host tests and the
  parent-relative benchmark gate pass.
- [x] **Schedule whole-function ownership analysis.** Produce ownership-aware
  HIR for real functions, establish borrowed and consumed parameters and
  ownership-transferring results, and insert duplication and cleanup. Preserve
  compile-time validation, generated result printing, and observable evaluation
  order. Implemented by
  [AnalyzeFunctionOwnership](../../src/DarkCompiler/passes/ownership/AnalyzeFunctionOwnership.fs)
  and
  [ElaborateFunctionOwnership](../../src/DarkCompiler/passes/ownership/ElaborateFunctionOwnership.fs);
  the verified artifact is deliberately discarded before existing ANF lowering,
  so this scheduling slice has no runtime effect.
- [x] **Schedule specialization across functions.** Connect analysis,
  selection, and materialization; propagate uniqueness information until the
  process stabilizes. Bound generated code and compilation work, and cache
  specializations by their bodies, contracts, and dependencies. Implemented by
  [ScheduleOwnershipVariants](../../src/DarkCompiler/passes/ownership/ScheduleOwnershipVariants.fs),
  which is invoked by production whole-function analysis and returns the
  materialization, iteration history, and structural cache descriptors. These
  foundation stages alone are not expected to improve runtime performance.
- [x] **Carry ownership through lowering.** Preserve contracts through calls,
  returns, branches, tail calls, ANF, and native code generation. Integrate with
  reference counting so ownership is neither duplicated nor lost. Scheduled
  groups now lower to real ANF clones with recursive and selected calls routed
  to their specialized symbols. Their positional contracts travel with the
  conversion result, keep those symbols out of incompatible inlining, extend
  the function registry, and are revalidated at the RC boundary before the
  ordinary native ABI erases the proof metadata.
- [x] **Allow optimized storage across function boundaries.** Let compatible
  specialized functions accept and return compiler-selected arrays. Define
  representation compatibility explicitly, preserve the public language
  interface, and justify the cost of any conversions. The first implementation
  uses ownership-authorized source fusion for nonrecursive `List<Int64>`
  map/reverse helpers with safe arguments. It erases the internal specialized
  boundary before region extraction, so arrays cross the former source-level
  boundary without an array/skew conversion or a public ABI change; unsupported
  and borrowed-input calls retain the persistent representation.
- [x] **Enable static in-place updates across calls.** Use proven uniqueness
  to reuse storage for supported operations, starting with existing
  `List<Int64>` map and reverse operations. Ownership-selected helpers now feed
  the existing list-region liveness and consume-or-copy solver. The focused
  cross-function workload uses one 56-byte allocation and exactly matches the
  equivalent closed pipeline's ARM64 instruction count, while shared and
  borrowed controls preserve immutable behavior.
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
