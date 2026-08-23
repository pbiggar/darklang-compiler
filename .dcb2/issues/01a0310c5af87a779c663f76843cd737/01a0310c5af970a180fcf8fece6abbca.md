---
format: 1
event-id: 01a0310c5af970a180fcf8fece6abbca
entity-id: 01a0310c5af87a779c663f76843cd737
entity-kind: issue
event-type: created
occurred-at: 2026-08-23T23:54:48.4419082+00:00
author: worker:65eb66d2e522:3367166:01a03103e1f27b488855a027865db44f
batch: 01a0310c598b7d78870b974f27d1e6e9
problem: 019ffcba981b74e5993e9bdd03a6f234
workflow: trial-first
---
# Trial scalar partial redundancy elimination

Eliminate a scalar expression redundant on only one predecessor of a join when insertion and reuse reduce generated work.

## Constraints

- Operate only on pure scalar MIR expressions with explicit type and dominance facts.
- Do not move expressions across calls, allocation, memory operations, reference-count operations, or exceptional control-flow boundaries.
- Preserve floating-point evaluation semantics and operand ordering.
- Add focused MIR and generated-code tests for partial redundancy, non-redundancy, and barrier cases.
- Run ./run-tests --ai and ./benchmarks/run_benchmarks.sh --verify routine for any retained candidate.
- Update docs/classic-optimizations.md with retained or rejected evidence.
- Remove temporary benchmark inputs and instrumentation.
- Execution preference: frontier

## Trial work

### Trial item 01a0310c5af87811bc6d09a5adad3147

Title: Establish join-local PRE opportunity

Category: MIR global value numbering

Evidence: Current MIR CSE propagates availability only down dominator subtrees; shared-leading-binding hoisting covers both branches, leaving the standard partial-redundancy case unaddressed.

Outcome:

Produce focused MIR evidence for an expression available on one predecessor but recomputed after the join, which current dominator-only CSE cannot remove.

### Trial item 01a0310c5af87ee698dd4b2dbedf45be

Title: Trial conservative scalar PRE

Category: code motion

Evidence: A narrowly typed, barrier-aware PRE trial is independently useful from existing CSE and branch-hoisting transformations.

Outcome:

Insert and reuse only when all predecessor paths can safely evaluate the same scalar expression and measure instruction-count impact.
