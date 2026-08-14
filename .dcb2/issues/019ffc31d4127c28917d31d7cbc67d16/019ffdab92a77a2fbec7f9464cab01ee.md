---
format: 1
event-id: 019ffdab92a77a2fbec7f9464cab01ee
entity-id: 019ffc31d4127c28917d31d7cbc67d16
entity-kind: issue
event-type: design-proposed
occurred-at: 2026-08-14T00:28:27.6879062+00:00
author: worker:65eb66d2e522:3725750:019ffd825d8d7ddfac574b9f8c608e82
previous: 019ffc347e9072259284da22a5f5b664
base-commit: 942d5621f4cdd6e27bc38fcfa3eb0584610e2b72
constraints-hash: 0379d2fcd58a10babd9a6e79dae4ce7d65601e6a51680c9c7e736841dc7c4d99
revision: 019ffc347e9072259284da22a5f5b664
---
# Design brief

## Problem

At compiler HEAD 942d5621f4cdd6e27bc38fcfa3eb0584610e2b72, structural and typed JSON are absent; same-source probes fail during name resolution. Interpreter HEAD 04fbe9dcc995c6188757d583e273cbd30a3e2d3d defines the current behavioral contract, confirming and refining the historical 8a402797 gap.

## Goals

- Match the interpreter’s complete structural JSON surface and observable behavior.
- Match type-directed serialization, parsing, canonical values, paths, and errors.
- Preserve appropriate ahead-of-time type checking and explicitly classify compiler-only extensions or divergences.
- Reuse the interpreter’s portable definitions and behavior wherever possible.

## Proposed solution

Introduce one ordered, duplicate-preserving structural JSON representation and shared parser/formatter core, then copy the portable public data model, helpers, builders, traversal, and error rendering from the pinned interpreter. Generate type-directed conversions from resolved compile-time type information so primitives, collections, tuples, aliases, records, and enums use the interpreter’s encodings and canonical failures without adding interpreter-style runtime reflection. Treat compile-time rejection of statically unsupported types as an intentional AOT divergence, and leave compiler-only value types unsupported unless they receive a separately documented JSON contract.

## Success criteria

- Structural values parse, format, traverse, and build with interpreter-equivalent ordering, duplicate-field, number, string, and malformed-input behavior.
- Supported typed values serialize and parse to the same results as the pinned interpreter across nested aliases, collections, tuples, records, and enums.
- Malformed or mismatched input produces the same public error variants, type information, JSON paths, raw fragments, and rendered messages.
- Current upstream JSON execution contracts are enabled and pass on supported native targets instead of reporting zero executed cases.
- Parity evidence records both exact revisions, and every compiler-only extension or AOT divergence is documented explicitly.

## Open questions

- none

## Delivery shape

single issue
