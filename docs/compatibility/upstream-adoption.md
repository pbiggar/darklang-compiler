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
| `Char.isAlphanumeric` | Letters or decimal digits, rather than all Unicode numbers | Match #5741's `Rune.IsLetterOrDigit`: superscript digits and Roman numerals are false; Arabic decimal digits and supplementary letters are true. Reuse existing Unicode categories. |
| `Float.isFinite` | Add current upstream declaration | NaN and both infinities yield false; finite values yield true. |
| `Float.toBits` | Interpret public bits as signed Int64 before converting to Int | Match #5741's `DoubleToInt64Bits`; -0.0 yields -9,223,372,036,854,775,808. Keep the private unsigned representation unchanged; recover imported Float assertion L255. |
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
assertions before the changes; it then passed 29/29 individually. Four further
classification regressions reproduced two `Char.isAlphanumeric` failures
before its correction. Existing
tuple and Char fixtures retain their language assertions while adopting the
new argument order and ASCII expectations. Full StdLib readiness gates are
pending after the additional classification correction. The preceding StdLib
checkpoint passed 13,228/13,228 host tests, `dune runtest` and 58/58 leak checks.

The existing x86-64 full performance snapshot has an incompatible workload
contract digest. The parent benchmark gate rejects it before measurement; no
performance ratio is claimed, and no tracked benchmark sources or snapshots
are replaced as part of this adoption.

## Parser review

Quoted-grapheme fixes from `0f87a71ac` and `3d0699bc3` are already supported.
`quoted-grapheme-regressions.e2e` imports eight observable assertions covering
parentheses, tuples, lists, local lets, callbacks, NFC normalization and compound
graphemes. All eight passed before any parser change; no replacement parser
implementation was added. PR #59's imported character literals and the existing
interpreter-only side-effect instrumentation exclusion remain unchanged.

## Selective performance review

Dictionary-backed `dedup` and `uniqueBy` candidates were measured against the
unchanged implementations with isolated Cachegrind instruction counts. These
are complete-program microbenchmarks, not the canonical full performance gate.
Each distinct-input program builds 1,000 values at runtime and checks the result
length; repeated-input probes contain 1,000 equal values and check length 1.

| Probe | Current instructions | Candidate instructions | Candidate/current |
|---|---:|---:|---:|
| `dedup`, distinct Int | 376,644,750 | 163,616,765 | 0.4344 |
| `dedup`, distinct Int64 | 325,542,204 | 23,788,582 | 0.0731 |
| `uniqueBy`, distinct Int64 | 326,430,145 | 24,701,056 | 0.0757 |
| `dedup`, repeated Int | 4,122,482 | 4,120,328 | 0.9995 |
| `dedup`, repeated Int64 | 3,886,837 | 3,844,749 | 0.9892 |

Neither general replacement was adopted. The `uniqueBy<Int>` candidate failed
compilation because its generated Int comparison helper was absent. The
`dedup` candidate changed repeated-NaN behavior and failed compilation for
function-valued elements (`Could not recover Dict type` for a function key).
Upstream explicitly changed these operations to dictionary-key semantics;
silently doing so would remove existing compiler-supported behavior. New
regressions retain first-occurrence order, compound values, repeated NaNs,
function identity and Blob identity. A future targeted optimization needs to
address these boundaries and pass the full performance gate before adoption.
