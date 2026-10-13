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
| `Float.clampRefusingNaN` | Complete upstream rename and exact error text | Implement `clampRefusingNaN` directly, remove `Float.clamp`, and migrate fixtures; generic clamp/trait dispatch remains discussion-only. |
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
new argument order and ASCII expectations. The final StdLib checkpoint passed
`env -u LD_PRELOAD ./build --ai`, `./run-tests --ai` (13,255/13,255),
`dune runtest`, and `python3 scripts/check_compiled_leaks.py` (58/58 clean).
Focused results were 47/47 adoption assertions, 149/149 enabled imported Float
assertions, and 8/8 quoted-grapheme regressions. The signed `Float.toBits`
correction reproduced two focused failures before implementation; the full
run also found and recovered one old indirect unsigned-bit expectation.

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

The dictionary optimization remains unadopted. Its historical
`uniqueBy<Int>` trial failed because a generated comparison helper was absent.
Those measurements predate the review fixes and do not measure the current code.

The review follow-up adopts upstream key semantics independently of that
optimization: `dedup` and `uniqueBy` use canonical comparison equality, including
NaNs nested in tuples, lists and records. Function, Blob and Stream keys are
rejected, including through generic wrappers and nested record fields. The
ordered scan remains quadratic in the number of distinct keys. `dedup` keeps
first-occurrence order; `uniqueBy` keeps the first value for each key and then
sorts the surviving values. Ordinary language equality is unchanged.

## Review corrections (2026-10-13)

- Resolve specialized String addition bottom-up during preparation. ANF now
  receives `StringConcat` and flattens it without repeated subtree inference.
  Checked type recovery handles internal numeric intrinsics, and failures
  propagate as compilation errors.
- Complete the `Float.clampRefusingNaN` migration, including imported fixtures.
  `Float.clamp` is no longer public; NaN bounds use the upstream error text.
- Adopt List key equality and restrictions without adopting the unverified
  dictionary optimization. Regressions cover scalar and compound NaNs,
  signed zero, first-occurrence order, Int `uniqueBy`, and rejected key shapes.

Follow-up branch: `adopt/upstream-review-fixes`; [PR #65](https://github.com/pbiggar/darklang-compiler/pull/65).
Native x86-64 verification of the review fixes:

- `env -u LD_PRELOAD ./build --ai`: passed.
- `./run-tests --ai`: 13,302/13,302 passed in 118.5 seconds.
- `./run-tests --ai --filter=upstream-stdlib-adoption`: 65/65.
- `./run-tests --ai --filter=string-plus`: 25/25, including a 128-operand generic chain.
- `./run-tests --ai --filter=closures --e2e-batch-size=1`: 94/94.
- `./run-tests --ai --filter=x64`: 98/98.
- `dune runtest`: passed.
- `python3 scripts/check_compiled_leaks.py`: 58/58 clean workloads.
- `./benchmarks/run_benchmarks.sh --verify-parent full`: blocked before measurement;
  the existing snapshot workload contract digest is incompatible.

Full verification found a closure-signature registration gap, which was fixed.
Two subsequent runs hit interrupted waits in the existing x64 test harness.
The harness now retries `EINTR` from polling and process waits while preserving
its original timeout, exit-code and signal checks; the final full run passed.
No benchmark sources or snapshots were changed. PR #65 merged into
`adopt/upstream-adoption-combined` at the user's request on 2026-10-13. The
full performance gate remains outstanding.

## Combined validation

All three adoption branches were combined without changing their reviewed
implementation files, at `e66e4053aa94ed564b1bca471446d5e5bd653466` on
`adopt/upstream-adoption-combined`. Native x86-64 validation passed:

- `env -u LD_PRELOAD ./build --ai`;
- `./run-tests --ai`: 13,278/13,278, in 133.0 seconds;
- `dune runtest`;
- `python3 scripts/check_compiled_leaks.py`: 58/58 clean workloads.

`./benchmarks/run_benchmarks.sh --verify-parent full` was attempted again and
stopped before measurement on the same incompatible existing x86-64 snapshot.
All approved areas have been reviewed; confirmed gaps are implemented and
correctness-tested. No deferred feature was implemented. PRs #60, #62 and #63
were superseded for main integration by PR #66, which preserves their commits.

## Main integration (2026-10-13)

[PR #66](https://github.com/pbiggar/darklang-compiler/pull/66) combines the
adoption branch with `main@82fb2cef`, including the shared-syscall StdLib work.
The merge required no compiler conflict repairs. The pinned Work VM toolchain
was restored and passed all 98 package and native/Unix checks.

`env -u LD_PRELOAD ./build --ai` passed. `TZ=UTC ./run-tests --ai` passed
13,376/13,376 assertions in 124.3 seconds. Benchmark report consistency and
E2E temporary-path isolation checks passed. Stale generated reports were
regenerated from their existing snapshots, without changing measurements.
`TZ=UTC dune runtest` passed, and `python3 scripts/check_compiled_leaks.py`
passed 58/58 clean workloads. Benchmark source integrity passed as well.

`./benchmarks/run_benchmarks.sh --verify-parent full` still stops before
measurement because the existing snapshot workload contract digest is
incompatible. The user explicitly requested main integration with that
previously reported limitation; no performance ratio is claimed.
