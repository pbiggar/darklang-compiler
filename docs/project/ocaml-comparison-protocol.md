# Temporary semantic comparison protocol

This is a historical record of the completed F# to OCaml migration. The
comparison harness was retired during the root Dune layout cleanup; its last
complete version is available at commit `c74e21253c864e656b06f7f09736f5f1b874a178`.
The paths and commands below describe that historical checkout.

Migration observations use UTF-8 JSON Lines. Each request contains `stage` and
`source`. Each response has `schema: 1`, the stage, and one complete semantic
`value`. Requests and both responses are retained on a mismatch. This protocol
does not replace the compiler CLI or introduce a public compiler library API.

Values preserve all fields and source order:

- Strings, booleans, and unit/null use JSON strings, booleans, and null.
  A string containing an isolated surrogate uses `utf16String` with every
  UTF-16 unit as four hexadecimal digits. Ordinary JSON serializers replace
  these units and would lose diagnostic evidence for non-BMP stray characters.
- Integer scalars carry `kind` (their signed/unsigned width or `bigint`) and an
  exact decimal string `value`; machine JSON numbers are not used for literals.
- Floating scalars carry `kind: float64` and the 16-digit lowercase hexadecimal
  IEEE bit pattern. NaNs and signed zero retain their complete bits.
- Tuples carry `tuple` with their ordered fields; lists and arrays use arrays.
- Union values carry their canonical source `type`, `case`, and ordered `fields`.
  Semantic options are unions, so absent values remain distinguishable from unit.
- Records carry their canonical source `record` name and ordered `[name, value]`
  field pairs. This preserves source ranges, trivia, and diagnostic evidence.
- Maps use `map` containing the full ordered key/value sequence. Relevant F#
  ordering is checked rather than assumed equivalent to OCaml polymorphic order.

For tokens, the value includes the entire tokenizer result: every token case
and payload, original text, range, doc comment, leading trivia, and the ordered
recovery diagnostics or fatal error. Written and checked AST observations use
the same recursive representation. Later IR adapters will use the same contract
with explicitly recorded mappings for any non-observable ephemeral identities.
Offsets/ranges are compared in the source representation used by the reference;
an OCaml adapter must account explicitly for UTF-8 versus UTF-16 boundaries.

Executable comparison always reads the original output files as bytes. It does
not use this semantic representation, strip sections, normalize IDs, rewrite
headers, or compare only program output. Timing values remain informational.
