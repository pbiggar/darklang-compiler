# Char, String, and Regex parity contract

This contract was revalidated against compiler baseline
`C@2ac3d8a0385e6002f604412e2296662dda6bb000` and interpreter revision
`I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. The implementation is the tree
containing this document. DCB1 report `8a402797` and historical compiler probe
`C@51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` were used only to seed the
comparison; every retained item was checked in the source revisions above.

The canonical interpreter declarations are
`packages/darklang/stdlib/{char,string,regex}.dark`; their managed behavior is
implemented by `backend/src/Builtins/Builtins.Pure/Libs/{Char,String,Regex}.fs`.
The compiler surface is implemented in
`src/DarkCompiler/stdlib/{Char,String,Regex}.dark`, with shared semantics in
`Unicode.dark` and generated data in `unicode_data.dark`,
`unicode_data_index/`, and `unicode_data/`. The generated source is split only
to bound the recursive Dark lexer's stack use; the generator treats the shards
as one versioned table set.

## Value and indexing contract

`Char` is exactly one Unicode extended grapheme cluster (EGC), and `String` is
valid UTF-8 in Unicode Normalization Form C. Source literals and every public
String-producing composition boundary preserve NFC. Invalid scalar values are
rejected by `Char.fromCodepoint : Int -> Option<Char>`; strict byte/Blob decoders
return their documented `None`/`Error`, while replacement decoders insert
U+FFFD and then normalize.

String traversal (`length`, `slice`, `first`, `last`, `dropFirst`, `dropLast`,
`head`, `charAt`, `map`, `toList`, `reverse`, padding, and EGC search) counts
EGCs. Negative slice indexes are relative to the EGC count and are clamped.
`indexOf` and `lastIndexOf` deliberately return UTF-16 code-unit indexes to
match the interpreter; `indexOfEgc` and `lastIndexOfEgc` return EGC indexes and
accept a match only when both ends lie on EGC boundaries. String comparison,
containment, and replacement are ordinal.

Padding has argument order `(value, padWith, goalLength)` and returns
`Result<String, String>`. A pad value other than one EGC returns exactly
`Expected \`padWith\` to be 1 character long, but it was \`"VALUE"\``.

## Unicode contract

The generated tables are pinned to Unicode 17.0.0. They include canonical
decomposition/composition and exclusions, canonical combining classes,
grapheme-break and Extended_Pictographic properties, Indic conjunct-break
properties, full unconditional casing plus contextual-casing inputs, general
categories, and White_Space. `scripts/generate_unicode_tables.py --check`
downloads no data when its versioned cache is populated, records every source
SHA-256 and row/range count, and verifies deterministic output.

NFC uses recursive canonical decomposition, canonical ordering, composition,
and algorithmic Hangul handling. EGC segmentation implements UAX #29 rules for
controls, Hangul, Extend/ZWJ/SpacingMark, Prepend, emoji ZWJ sequences, regional
indicator pairs, and Indic conjuncts. UTF-8 traversal rejects overlong forms,
surrogates, out-of-range scalars, invalid continuations, and truncation.
Public casing follows the interpreter's simple invariant mapping, so a full
mapping which expands one scalar to several (for example `ﬁ` to `FI`) remains
unchanged. The full mappings are retained in the generated data rather than
silently discarded.

## Regex contract

Regex operations are `isMatch`, `find`, `findAll`, `replace`, `replaceAll`, and
`split`. Matching is leftmost-first and greedy unless a lazy suffix is present.
`find`/`findAll` return whole matches, not captures. No match returns `false`,
`None`, `[]`, the unchanged input for replacement, or `[input]` for split.
Zero-length global matches advance by one EGC and also match the final boundary.
`replace` treats replacement text literally; `replaceAll` expands `$$`, `$&`,
and numbered captures. Capturing separators are included by `split`, including
leading and trailing empty fields. Invalid syntax raises the runtime error
`Invalid regex pattern`.

The portable engine represents atoms, quantifiers, captures, candidates, and
matches with discriminated unions and records and does not depend on a host
regex library. Its input units are EGCs; observable match strings and boundary
behavior match the interpreter for the activated pinned suite.

## Explicit compiler-extension boundary

These are compiler extensions, not parity APIs:

- `Char.isLetter`, `isWhitespace`, `isAlphanumeric`, `toCode`, and `fromCode`.
- `String.getByteAt`, `substring`, `take`, `drop`, `toCodepoints`,
  `codepointLength`, `fromCodepoints`, `toUpperCase`, `toGraphemes`,
  `graphemeLength`, `replace`, and `equals`.

Byte-oriented stdlib consumers use internal `String.__byte*` helpers. Public
extension slices validate UTF-8 scalar boundaries, so they cannot manufacture
malformed String or Char values.

## Focused evidence

- The retained decomposed probe, `String.length("e\u0301")`, returned byte
  length `3` at `C@51093e0`; it returns EGC length `1` in this tree, matching
  `I@04fbe9d`.
- `src/Tests/e2e/stdlib/text_parity.e2e` covers NFC concatenation, a family emoji
  EGC, scalar construction, UTF-16 versus EGC indexes, and regex match/split
  behavior.
- The pinned upstream `char.dark`, `string.dark`, and `regex.dark` files are
  registered in `TestRunner.fs`. The String fixture has only harness adaptations:
  its interpreter side-effect callback assertion is omitted, `newline()` uses
  the compiler's existing spelling, and the unrelated, unavailable `Slugify`
  module cases remain outside this approved text-parity scope. Focused results
  are recorded in the implementing commit message.

## Integration verification

The completed implementation was rebased onto `main@9b6c27d0a52c313774ef5a05b78447cf0015c353`.
Its final source commit is
`C@671ab160ef4fb2de3829ee251285e09733e8c1e8`; the exact integration HEAD used
for the post-rebase comparisons was
`C@5515cc67bfe4c5918940b7b3d47d1027c259bcb0`. At that revision:

- `bin/Tests/Debug/net10.0/Tests --ai` passed 7,760/7,760 tests.
- `python3 scripts/generate_unicode_tables.py --check` passed against the
  pinned Unicode 17.0.0 inputs.
- `./benchmarks/run_benchmarks.sh --verify routine` completed all 19 ARM64
  workloads with no failures and an exact current/baseline geometric ratio of
  1.000000. The displayed Dark/Rust performance ratio remained 2.25x.

The compiler's intentional AOT boundary is also preserved: a type-invalid call
fails during compilation and reports the inferred type, whereas the interpreter
fixture's corresponding runtime error includes the concrete argument value.
