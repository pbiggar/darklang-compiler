# Remaining in-place mutation work

Implemented ownership analysis, variant selection, lowering and list storage
reuse are described in [compiler-selected list arrays](../compiler/runtime/list-array-reuse.md).
The [optimization catalog](../compiler/optimizations/catalog.md) owns retained
record and sum reuse constraints. Complete the following work before claiming
general reuse across calls and recursive functions.

- [ ] **Add general constructor reuse.** Safely dismantle old values and reuse
  suitable storage for new constructors. Specialize destruction and release
  child references correctly, extending reuse to records and other supported
  data. Fixed-layout record reuse now accepts immediate and non-observable
  `String`, `Blob`, or `Int` leaves plus tuples, lists, and dictionaries built
  recursively from them. Nominal records join that set when the registry and
  concrete generic arguments prove every nested field safe, including regular
  recursive records through typed release-plan back-edges. It retains
  replacement children, releases displaced children with their complete
  recursive release plans in field order, and then overwrites the block.
  Straight-line boxed sums now preserve instantiated type and variant payload
  metadata through ANF and reuse the same two-word block with source-layout
  cleanup before target-layout stores. Complete sum metadata admits safe nested
  and regular recursive payloads through typed release-plan back-edges. Streams
  and containers holding them fail closed; cross-branch reuse, type-growing
  record or sum cycles, closures, and interprocedural reuse remain before this
  item is complete.
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
