---
format: 1
event-id: 019ffc1e804b7eff9fc8b9e2d66c0dfb
entity-id: 019ff58b47c67a868f397bf7db4dd4a1
entity-kind: issue
event-type: breakdown-feedback
occurred-at: 2026-08-13T17:14:45.1958431+00:00
author: human
previous: 019ff7d76bc871e8a75e4dcddd9dab90
plan: 019ffac33e597bd3a057640b646f2ae4
---
Many or most of the modules will already have been implemented by the interpreter, so where possible simply take the interpreter's implementation and copy it over. Only for things that are builtins or that are implemented in F# or using dotnet builtin libraries (and other similar edge cases) where it is impossible to copy it over, should they be reimplemented instead of copied over.
