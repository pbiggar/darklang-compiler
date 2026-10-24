# Char, String, and Regex parity contract

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

The canonical interpreter declarations are
`packages/darklang/stdlib/{char,string,regex}.dark`; their managed behavior is
implemented by `backend/src/Builtins/Builtins.Pure/Libs/{Char,String,Regex}.fs`.
The compiler surface is implemented in
`packages/darklang/stdlib/{Char,String,Regex}.dark`, with shared semantics in
`packages/darklang/stdlib/string/__unicode.dark` and generated data under
`packages/darklang/stdlib/string/__unicode/`. The generated source is split only
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
`head`, `charAt`, `map`, `toList`, `reverse`, `padStart`, `padEnd`, and EGC search) counts
EGCs. Negative slice indexes are relative to the EGC count and are clamped.
`indexOf` and `lastIndexOf` deliberately return UTF-16 code-unit indexes to
match the interpreter; `indexOfEgc` and `lastIndexOfEgc` return EGC indexes and
accept a match only when both ends lie on EGC boundaries. String comparison,
containment, and replacement are ordinal.

Padding has argument order `(value, padWith, goalLength)` and returns
`Result<String, String>`. A pad value other than one EGC returns exactly
`Expected \`padWith\` to be 1 character long, but it was \`"VALUE"\``.

`Builtin.stringSlugify` lowercases before retaining ASCII letters, digits,
Unicode whitespace, hyphens, and underscores. Vertical tab is removed rather
than used as a separator. After trimming whitespace, consecutive separators
become one hyphen; existing leading or trailing hyphens are preserved. Filtering
uses codepoints so unsupported combining marks do not discard an ASCII base.

`String.trim`, `trimStart`, and `trimEnd` remove Unicode whitespace codepoints
at the requested boundaries. Their ASCII byte scan remains the fast path;
Unicode trimming handles CRLF clusters and preserves any attached non-whitespace
combining marks after removing a whitespace codepoint.

## Unicode contract

Compiler source handling uses native UTF-8 strings, Uutf decoding/encoding,
Uucp scalar properties and case mappings, Uunf NFC normalization, and Uuseg
extended grapheme segmentation (currently pinned to Unicode 16.0.0). Source
positions count scalars and diagnostics retain complete scalars. Compiler maps
use OCaml `String.compare`; helper names and fingerprints hash UTF-8 bytes.
Compiler symbol prefixes use `String.starts_with`/`String.ends_with`, without
locale-sensitive collation. An Indic conjunct such as `'क्ष'` is accepted as a
single `Char`, including in parentheses and escaped literal spellings.

These host compiler libraries are separate from the generated Dark runtime
Unicode tables described below. The runtime's public UTF-16 search-index
compatibility remains the language contract; compiler scanning does not use it.

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

`Char.isLetter`, `isAlphanumeric`, and `isWhitespace` classify the first
scalar through those generated Unicode tables. `Char.toCodepoint` returns
`Some` only when the EGC contains exactly one Unicode scalar, matching the
interpreter distinction between a character cluster and a codepoint.

## Display width and styled terminal text

`String.displayWidth` measures Unicode extended grapheme clusters in terminal
cells: combining and control scalars contribute zero, East Asian and emoji
clusters contribute two, regional-indicator pairs form one two-cell flag, and
VS16 promotes a narrow text glyph to emoji width. Pretty rendering uses this
cell count when deciding whether a group fits.

`String.padStartToWidth` and `padEndToWidth` take `(value: String, width: Int)`
and return a String padded with leading or trailing spaces to the requested
display width. Text already at least that wide is returned unchanged, including
for negative targets. For example, `padEndToWidth "界" 4` returns `"界  "`.

`Stdlib.Cli.Tui.Text.styledWidth`, `clipToWidth`, and `clipMarked` copy the
terminal-row behavior pinned at darklang/dark revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`. SGR sequences occupy no cells and
are retained by clipping; other escapes and control characters are discarded.
Clipping never splits an extended grapheme cluster, and `clipMarked` reserves
one cell for an ellipsis except when the requested width is below two.

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

## Implementation boundary

Byte-oriented stdlib consumers use private `String.__byte*` helpers. These are
not source-resolvable; public text behavior is provided only by the parity API.

## Focused evidence

- `String.length("e\u0301")` returns EGC length `1`, preserving normalization
  rather than exposing UTF-8 byte length.
- `test/fixtures/e2e/stdlib/text_parity.e2e` covers NFC concatenation, a family emoji
  EGC, scalar construction, UTF-16 versus EGC indexes, and regex match/split
  behavior.
- The pinned upstream `char.dark`, `string.dark`, and `regex.dark` files are
  imported. Char and Regex are enabled. String has individual gates for its
  interpreter test helpers; current dispositions are recorded in the source ledger.

## String addition

Interpreter `07bafe2d8` implements String `+` with NFC normalization in
`FastOps.fs` and `Builtins.Pure/Libs/NoModule.fs`. The compiler now selects
the existing normalized concatenation operation for typed String operands and
for generic addition specialized to String. For example, `"e" + "́"` returns
`"é"`; no second Unicode implementation or runtime type dispatch is added.
Specialized additions resolve bottom-up during preparation, before ANF
conversion. ANF flattens the resolved `StringConcat` tree without repeatedly
inferring subtree types. Numeric addition keeps its existing implementation. Mixed operand types,
Bool addition and Char addition are rejected during compilation. `++` is rejected
by the parser; use `+` for Strings and explicitly convert Char operands.

`test/fixtures/e2e/string-plus.e2e` covers literals, variables, function calls,
pipelines, chained operands, inferred lambdas, explicit generic functions,
empty operands, legacy syntax and compilation-error boundaries. The fixture also covers closure operands, simultaneous String/numeric specializations,
and a 128-operand generic chain.
