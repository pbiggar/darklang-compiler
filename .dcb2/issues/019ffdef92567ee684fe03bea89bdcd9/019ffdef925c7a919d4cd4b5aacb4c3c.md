---
format: 1
event-id: 019ffdef925c7a919d4cd4b5aacb4c3c
entity-id: 019ffdef92567ee684fe03bea89bdcd9
entity-kind: issue
event-type: created
occurred-at: 2026-08-14T01:42:44.0603218+00:00
author: worker:65eb66d2e522:3725750:019ffd825d8d7ddfac574b9f8c608e82
batch: 019ffdef91dc7111ab9141a1107aa72f
problem: 019ffcba981b74e5993e9bdd03a6f234
workflow: trial-first
---
# Make register choice call-aware

Prefer caller-saved registers for short-lived values and callee-saved registers only for values live across calls.

## Constraints

- Derive preferences from existing instruction-level liveness; preserve ABI precolors, phi coalescing, spill correctness, and SaveRegs/RestoreRegs behavior on both architectures.
- Add failing allocator and generated-code tests for values crossing and not crossing calls first; run full verification and routine benchmarks.
- Retain only repeatable reductions in prologue, call-save, or spill instructions; update the backlog and remove temporary inputs.

## Trial work

### Trial item 019ffdef9256707c9a239df8cfbc8f70

Title: Trial per-value call-crossing preferences

Category: register allocation

Evidence: 5_RegisterAllocation.fs currently switches the entire function to callee-saved-first ordering when any non-tail call exists, even for values dead before every call.

Outcome:

Color non-crossing values into caller-saved registers and crossing values into callee-saved registers when interference permits.
