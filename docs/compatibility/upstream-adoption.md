# Approved upstream adoption

Review date: 2026-10-12. Interpreter source: `darklang/dark@07bafe2d8`.
Compiler parent: `26a2a769276e24c0d8ab6b03e025a0ead2214aaa`.
The approved work excludes traits, async/parallel List operations, effect
metadata/cancellation, the Dark at-rest checker, package persistence/SCM and UI.

## Confirmed changes

| Surface | Change | Reason and implementation |
|---|---|---|
| HTTP/HTML | Query decoding, first-equals splitting, attribute escaping and comment sanitization | Upstream #5727/#5735; PR #60; see [HTTP/HTML evidence](stdlib/html-and-http.md). |
| String `+` | NFC concatenation, including generic specializations | Reuse existing Unicode and concatenation lowering; PR #62; see [text evidence](stdlib/text.md). |
| `Tuple2.mapFirst/mapSecond/mapBoth` | Tuple precedes callbacks | Upstream #5788; update existing declarations and caller tests. |
| `Tuple3.mapFirst/mapSecond/mapThird/mapAllThree` | Tuple precedes callbacks | Same upstream argument-order change. |
| `List.SortByComparatorHelpers.validate/merge/mergeSort` | Values/lists precede comparator | Upstream #5788; preserve private ownership-rooting implementation. |
| `List.chunkBySizeHelper` | List precedes size | Upstream #5788; update `chunkBySize` and recursion. |
| `Char.toAsciiCode` | Only a single ASCII scalar yields `Some` | Upstream #5790; a non-ASCII scalar or compound grapheme yields `None`. `isASCII` uses this result. |
| `Float.isFinite` | Add current upstream declaration | NaN and both infinities yield false; finite values yield true. |
| `Float.clampRefusingNaN` | Add current upstream name and exact error text | Reuse existing Float clamp behavior. Preserve compiler `Float.clamp` and its historical error text; generic clamp/trait dispatch remains discussion-only. |
| `String.compare` | Add ordinal UTF-16 comparison | Reuse existing typed comparison; only its sign is significant. |
| `List.setAt`, `List.IndexError` | Add checked replacement with negative indexing | Resolve index from list length; return `IndexOutOfBounds` outside the list, including empty lists. |

The declaration comparison preceded implementation: Char and Math had no missing
public function names; String lacked `compare`; Float lacked `isFinite` and
`clampRefusingNaN`; List lacked `setAt` and deferred `parallelMap`. `Float.sqrt`
is already a compiler builtin even though it has no Dark declaration. Existing
functions above differed in argument order rather than name or result type.

## Existing behavior retained

- Unicode normalization, grapheme-aware String operations, whitespace trimming,
  slugification, display-width padding, character literals and Char/String
  separation are already implemented.
- Upstream #5655's `splitFirst`, `splitN`, `indexOfEgc` and `lastIndexOfEgc` are
  present. Shortest-roundtrip Float formatting from #5773 and the Char/Float/Math
  additions from #5741 are present.
- Signed integer remainder already returns an Error for zero and zero for
  divisor -1, including the minimum signed value. No replacement is needed.
- `Http.Request.header` and order-preserving `List.dedup` from #5739 are present.
- Upstream still implements `mathTanh` using `System.Math.Sinh`; compiler
  `Math.tanh` deliberately retains that behavior. No upstream correction was found.
- Traits and generic clamp/comparison dispatch are not implemented by this work.
  Interpreter-only instrumentation exclusions remain intentional.

## Validation checkpoints

The new `upstream-stdlib-adoption.e2e` fixture reproduced 26 failures out of 29
assertions before the changes; it then passed 29/29 individually. Existing
tuple and Char fixtures retain their language assertions while adopting the
new argument order and ASCII expectations. Full StdLib readiness gates are
pending at this checkpoint.

The existing x86-64 full performance snapshot has an incompatible workload
contract digest. The parent benchmark gate rejects it before measurement; no
performance ratio is claimed, and no tracked benchmark sources or snapshots
are replaced as part of this adoption.

Parser regression review and selective List performance measurements remain
pending at this checkpoint.
