# Roadmap

This file contains concrete work that is not yet represented by a focused
failing test or external issue. Compatibility gaps are tracked in the
[compatibility documentation](../compatibility/overview.md); reproducible
compiler bugs belong in [known issues](known-issues.md).

## Current work

- Complete compiler-selected in-place mutation using the
  [Perceus-style optimization checklist](perceus-checklist.md).
- Deepen the memory-management matrix described in the
  [x86-64 backend status](../compiler/backend/x64.md), especially recursive
  HAMT payloads, allocator reuse, and the shared raw-memory policy.
- Expand byte-level x86-64 instruction-encoding coverage and replace the
  hand-maintained coverage count with a test-derived report.
- Finish upstream-test enablement. `TestRunner.fs` discovers the complete
  upstream corpus and is the source of truth for the unsupported-file and
  unsupported-line denysets. Do not preserve dated failure counts here.
- Validate the compiler against the existing package repository and turn each
  discovered incompatibility into a focused test or compatibility-ledger item.

## Longer-term direction

- Continue toward full language, standard-library, and upstream-test parity,
  with the compatibility ledgers defining concrete slices.
- Unify managed-value ownership around typed shapes and release plans instead
  of ad hoc RawPtr, heap-primitive, and backend-specific paths.
- Treat self-hosting the compiler and test suite in Darklang as a design goal
  that requires a scoped proposal before implementation.

Rejected optimization trials are recorded in the
[optimization catalog](../compiler/optimizations/catalog.md) so they are not
repeated without new evidence.
