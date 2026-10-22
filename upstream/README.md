# Imported interpreter sources

This directory contains every `.dark` file under `packages/` and
`backend/testfiles/execution/` at the interpreter revision in
[`porting/sources.yaml`](../porting/sources.yaml). `sources.list` records the
original paths. The upstream license is preserved in `LICENSE.md`.

Source bodies remain upstream-owned. Compiler decisions are per-item
`// @compiler` comments; their short reason tags are defined in
[`porting/compiler-reasons.json`](../porting/compiler-reasons.json).
Compiler implementations remain in `packages/`, referenced by replacement
annotations on the imported declarations. The compiler embeds both sets of
sources and selects implementations before checking discarded bodies.

Named package tests and execution assertions are discovered directly from
these files. Existing adapted fixtures remain compiler regressions; they are
not counterparts used to count this inventory. Test execution receipts are
separate from the source annotations.
