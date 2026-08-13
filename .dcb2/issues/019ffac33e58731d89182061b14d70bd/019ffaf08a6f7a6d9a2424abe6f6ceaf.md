---
format: 1
event-id: 019ffaf08a6f7a6d9a2424abe6f6ceaf
entity-id: 019ffac33e58731d89182061b14d70bd
entity-kind: issue
event-type: created
occurred-at: 2026-08-13T11:44:55.9193955+00:00
author: human
batch: 019ffaf08a6e71ba85e38c74d079750e
parent: 019ff58b47c67a868f397bf7db4dd4a1
workflow: plan-first
---
# Complete prelude parity

Expose the interpreter’s polymorphic equality and explicit printing prelude.

## Constraints

- Missing functions: Stdlib.equals, notEquals, print, printLine, and printLines.
- Evidence: C@51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3 src/DarkCompiler/CompilerLibrary.fs:1017-1053 has no prelude source; I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d packages/darklang/stdlib/noModule.dark:1-9 and print.dark:1-14, with backend/testfiles/execution/stdlib/nomodule.dark.
