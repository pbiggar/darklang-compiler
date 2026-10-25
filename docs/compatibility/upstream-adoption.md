# Upstream adoption decisions

The [source pin](../../porting/sources.yaml) and per-item import annotations own
source provenance and selection. This document preserves reasons for sharing,
replacement and deliberate deferral. Semantic details belong in the linked
contracts; run counts, merge chronology and transient gate failures belong in
Git history.

## Source sharing

Share original declarations when their public contract and ownership behavior
match compiled execution. Engine-specific adapters remain native. SQLite
sharing covers cell values and pure accessors; it does not establish database
I/O support. Declaration checking alone does not establish compatibility.

Candidates retained as compiler implementations after review:

| Candidate | Reason to retain the compiler implementation |
| --- | --- |
| Dictionary-backed `List.uniqueBy` and `dedup` | Candidate sharing failed key-equality/inference regressions. Revisit only with those cases covered. |
| `List.groupByWithKey` | Candidate sharing leaked for compound values. |
| `String.endsWith` | The shared candidate materialized a large string as graphemes. |

Sharing `Float.isFinite` exposed an integer simplification incorrectly applied
to Float: `x - x = 0` does not hold for NaN or infinity. Preserve Float boundary
coverage when changing optimizer rules or sharing library code.

## Adopted contracts

| Surface | Contract and source of detail |
| --- | --- |
| HTTP/HTML | Decode query keys/values, split at the first equals, escape attributes and repeatedly break comment double dashes. See [HTTP/HTML](stdlib/html-and-http.md). |
| String addition | NFC-normalized `+`, including generic specializations; resolve additions bottom-up before ANF flattening. See [text](stdlib/text.md). |
| Tuple and List helper arguments | Tuple precedes mapping callbacks; lists/values precede the comparator in sort helpers; list precedes size in `chunkBySizeHelper`. Original bodies and focused callers pin the order. |
| Char | `toAsciiCode` accepts only a single ASCII scalar. `isAlphanumeric` follows letters or decimal digits, excluding superscript digits and Roman numerals. See [text](stdlib/text.md). |
| Float | `toBits` returns `UInt64` (negative zero: `9223372036854775808UL`); `clampRefusingNaN` replaces `clamp`; `isFinite` rejects NaN/infinity. See [Float/Math](stdlib/floats-and-math.md). |
| Math | `tanh` computes hyperbolic tangent, superseding the old interpreter `Sinh` bug. |
| Float-to-integer conversion | `Int64.fromFloat`/`UInt64.fromFloat` and rounded variants return `Some` only for finite in-range inputs. |
| String comparison | Ordinal UTF-16 comparison; only the result sign is significant. |
| List | `contains` replaces `member`; `setAt` supports negative indices and returns `IndexOutOfBounds` outside the list. |
| List key semantics | Canonical comparison equality includes nested NaNs; reject Function, Blob and Stream keys. The ordered scan remains quadratic in distinct keys. `dedup` preserves first occurrence; `uniqueBy` keeps the first value per key, then sorts survivors. Ordinary equality is unchanged. |
| Collection grammar | Record fields, updates and Dicts use commas or newlines; semicolons remain statement separators. |
| Removed syntax and host cases | Obsolete `[<DB>]` syntax/user datastores, Windows, PowerShell and unused Armv7l cases follow upstream removal. |

## Scope boundaries

Importing new sources does not implement their runtime services. Effect
metadata/cancellation, at-rest checking, package persistence/SCM and UI require
separate decisions and implementation. Traits have their own
[static contract](language/traits.md).

For async, the interpreter carries pending reads through calls, containers and
closures, forces them at builtin boundaries, and exposes explicit `await` via
`Builtin.runtimeForce`. The native implementation must satisfy that contract
before corresponding source dispositions change; a pin update alone supplies
no execution evidence.
