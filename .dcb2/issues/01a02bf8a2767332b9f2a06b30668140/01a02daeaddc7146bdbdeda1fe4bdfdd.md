---
format: 1
event-id: 01a02daeaddc7146bdbdeda1fe4bdfdd
entity-id: 01a02bf8a2767332b9f2a06b30668140
entity-kind: issue
event-type: assistant-message
occurred-at: 2026-08-23T08:13:37.6288822+00:00
author: worker:65eb66d2e522:1481282:01a02c2a368573bc9b39df6a4ae424df
previous: 01a02bf8a277769a853fbc15ea8b2810
reply-to: 01a02bf8a277769a853fbc15ea8b2810
response-kind: investigation
---
The repository already distinguishes several kinds of compiler extension. The defensible target is: retain extensions required by AOT compilation or native implementation, but remove public source-language APIs and observable behavior that do not exist in the reference runtime.

Most promising removal candidates are explicitly documented:

- `docs/text-parity.md`: public `Char` and `String` extension APIs, including byte access, substring/take/drop, codepoint/grapheme APIs, casing, replace, and `String.equals`.
- `docs/blob-parity.md`: `Bytes.*`, `Base64.urlDecode`, and non-parity `Crypto.*` names.
- `docs/name-resolution.md`: the public native intrinsic catalog (`File`, `Path`, `Platform`, `Random`, raw-memory, etc.) and the special `Stdlib.List.empty` module-value lowering.
- `docs/list-parity.md`: the remaining internal `getAtOrDefault` helper, if it is callable from source rather than exclusively compiler-private.
- `docs/html-http-parity.md`: `Html.s`, identified as the sole public HTML extension.

These are different from compiler implementation mechanisms that should normally remain invisible: ANF/MIR/LIR lowering, monomorphization, lambda lifting, refcount insertion, generated comparison/rendering helpers, backend code generation, and optimizations. They affect timing or machine-level implementation but need not enlarge the language surface.

Two current behaviors require an explicit product decision before removal because they are inherently AOT-specific rather than accidental surface area: static type/name failures and exhaustiveness checks, plus target/path/platform facts currently constant-folded in `passes/2_AST_to_ANF.fs`. The first category matches the stated allowed timing difference. The second changes program results across compile/run environments, so it is not merely timing; retaining it is incompatible with the proposed rule unless those calls become runtime operations or are removed from the public API.

Recommended next scope: inventory every source-reachable `CompilerExtension` registration and every documented extension, classify each as (1) parity public API to retain, (2) public extension to delete, (3) implementation intrinsic to privatize, or (4) unavoidable AOT timing behavior. Then remove category 2 and make category 3 unreachable through normal source resolution. This keeps optimization and lowering intact while shrinking the observable language surface.
