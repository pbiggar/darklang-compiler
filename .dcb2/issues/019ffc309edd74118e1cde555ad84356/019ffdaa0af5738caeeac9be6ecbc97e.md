---
format: 1
event-id: 019ffdaa0af5738caeeac9be6ecbc97e
entity-id: 019ffc309edd74118e1cde555ad84356
entity-kind: issue
event-type: design-proposed
occurred-at: 2026-08-14T00:26:47.4139600+00:00
author: worker:65eb66d2e522:3725750:019ffd825d8d7ddfac574b9f8c608e82
previous: 019ffc3515e273d4b0f06ef2dfe9307c
base-commit: 70709eb082cae3debd8b266c68a623956ff87ab1
constraints-hash: 18e134297412140d698fd477ed090fb0a5299723a16519600ee69e07fe1bef3a
revision: 019ffc3515e273d4b0f06ef2dfe9307c
---
# Design brief

## Problem

At compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899, tuple semantics only partially match darklang/dark 04fbe9dcc995c6188757d583e273cbd30a3e2d3d: legacy syntax and access extensions remain, invalid arities are representable, and matching and rendering have observable gaps.

## Goals

- Make the pinned interpreter’s public tuple behavior the baseline.
- Preserve exact arity and element order through syntax, typing, lowering, matching, equality, rendering, and runtime values.
- Support grouping, nested heterogeneous tuples, destructuring, public access, and left-to-right evaluation while keeping Unit separate.
- Remove tuple-specific compiler extensions and document unavoidable ahead-of-time divergences.

## Proposed solution

Adopt one ordered, at-least-two-element tuple model across language representations. Use current Dark tuple and tuple-type grammar, interpret singleton parentheses as grouping, expose access through destructuring and the public tuple library, and keep positional projection internal. Preserve left-to-right, once-only evaluation; exact-shape destructuring and matching; element-wise structural equality; and interpreter-compatible rendering. Migrate tuple-bearing sources away from comma-style tuple types and numeric field access. Retain the compiler’s native fixed-block representation and earlier static rejection of invalid tuple uses as explicit, non-observable or timing-only divergences, while separating non-language singleton blocks from tuple values.

## Success criteria

- Valid tuple programs produce the interpreter’s values, match choices, equality results, errors, and rendered output.
- Tuple arity and order remain correct for arbitrary, nested, and heterogeneous tuples.
- Parenthesized single values group, singleton tuples cannot be formed, and Unit remains a primitive value.
- Tuple construction and comparisons evaluate operands once from left to right and stop at the first failure.
- Legacy tuple-type syntax and public numeric projection are absent from supported source, with all affected sources migrated.
- Ahead-of-time diagnostic timing and native representation differences are explicitly classified without changing valid-program behavior.

## Open questions

- none

## Delivery shape

single issue
