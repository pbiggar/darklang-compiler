---
format: 1
event-id: 019ffc3baeff7d93a931add82cf06e5e
entity-id: 019ffc31d41271da92ee04c25578e208
entity-kind: issue
event-type: design-proposed
occurred-at: 2026-08-13T17:46:37.6954686+00:00
author: worker:65eb66d2e522:3461018:019ffc2be36570229c864f9f740c6c48
previous: 019ffc347e917e8bade039afa966abfb
base-commit: ea6ce4b1069d502b3e33c3b4c70ddadad801c97b
constraints-hash: 7cd582402b7aff46f9ec2a0bc6048eb5d6bfe288a80d30675a3bd79da8554cfb
revision: 019ffc347e917e8bade039afa966abfb
---
# Design brief

## Problem

At C@51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3 versus I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d, temporal behavior lacks parity: DateTime is publicly interchangeable with an integer, duration parsing is absent, and related signatures and semantics differ. DCB1@8a402797 is historical evidence only.

## Goals

- Make DateTime a distinct runtime value while matching the interpreter’s observable Int-based temporal API.
- Provide canonical duration parsing with identical units, numeric and whitespace handling, integer results, and errors.
- Share portable wrappers while implementing clock, epoch, calendar, arithmetic, and timezone-independent conversion primitives natively.
- Treat interpreter behavior as the parity contract, classify remaining compiler capabilities as extensions, and introduce no intentional semantic divergences.

## Proposed solution

Introduce an opaque DateTime runtime representation backed by native temporal primitives, then expose it through shared portable wrappers whose signatures and behavior follow I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d. Add canonical duration parsing against the same baseline. Keep compiler-only capabilities explicitly outside the parity surface and contain all temporal parity work within one coherent boundary.

## Success criteria

- DateTime values are observably distinct from integers while all specified conversions, arithmetic, calendar extraction, clock behavior, and millisecond operations match the interpreter baseline.
- Duration inputs produce the same integer values or exact errors as the interpreter across recognized units, whitespace, numeric forms, malformed input, and boundary cases.
- Focused same-source comparisons at C@51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3 and I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d demonstrate both value parity and type separation.
- Every remaining difference is recorded as a compiler-only extension; no unclassified or intentional semantic divergence remains.

## Open questions

- none

## Delivery shape

single issue
