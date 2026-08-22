---
format: 1
event-id: 01a02bcac46e7eecbd8b08bad7afda93
entity-id: 019ffc31d4127b9195d156978112c47d
entity-kind: issue
event-type: assistant-message
occurred-at: 2026-08-22T23:25:03.9821293+00:00
author: worker:65eb66d2e522:1323690:01a02bc8a31e707cbb9f2a3e6040375a
previous: 019ffc347e9371b4ac892375576bfb54
reply-to: 01a02b627b727ee8b3739c4f866a470c
response-kind: investigation
---
Current stage remains approved integration; I made no edits or stage changes.

At current compiler HEAD `C@8878079eeefc0e5a5fbbb35d5e0a4277fd94f8c9`, the integer-family work is not present on this checkout: `Int128.dark` and `UInt128.dark` are absent, and only `int.dark` and `int64.dark` upstream fixtures are registered in `TestRunner.fs:284-285,329-330`. The agreed interpreter baseline remains `I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`; every implementation comparison should record both that revision and the compiler commit actually being changed, rather than retain historical inventories.

Your direction makes the parity rule unambiguous: remove—not document or retain—non-interpreter integer APIs and aliases, and update integer-module callers to the interpreter spellings. Current examples needing removal include `Int.compare`/`Int.equals` (`stdlib/Int.dark:71-78`), `sub`/`mul`/`div` aliases (for example `stdlib/Int64.dark:18-22`), and `^`-based integer implementations such as `Int64.bitwiseXor` (`Int64.dark:58`). The interpreter’s declared named functions are the public surface.

The package-value probe should be documented as a test-environment boundary, not an integer divergence: it is the upstream fixture that obtains an integer through the interpreter’s package-value environment. The interpreter resolves that from its live package manager; the AOT compiler only has package values when compilation is explicitly supplied an immutable `PackageValueCatalog` snapshot. It does not perform live package lookup or ordinary package top-level initialization. Therefore a compiler parity probe must either provide the identical catalog entry or isolate the numeric assertion with the same typed literal. This should be called out next to the probe and linked to `docs/diff-value-search-parity.md:86-92` and `docs/name-resolution.md:108-110`.

`Builtin.testRuntimeError` is currently used throughout the integer stdlib, including `Int.dark:222,240-245,263,270,282,483,530`. Replacing it with `crash` requires a defined Dark-level crash primitive/lowering first: the checkout has F# `Crash.crash` (`Crash.fs:12`), but no source-level stdlib `crash` function; `Builtin.testRuntimeError` is the currently recognized compiler-lowered form (`passes/2_AST_to_ANF.fs`). This is a necessary implementation boundary to establish before converting the wrappers, so the replacement has the intended interpreter-visible failure behavior rather than becoming an unresolved source reference.
