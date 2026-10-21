# Darklang parser

The parser and its written syntax and structural validation are translated from
`darklang/dark@e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`, the revision in
`porting/sources.yaml`. The Apache-2.0 license is included as [LICENSE.md](LICENSE.md).

The OCaml files split `LibParser/Parser.fs` into expression, pattern, type,
declaration and file productions. Those boundaries do not change the grammar.
`Tokenizer`, `Lexer`, `WrittenTypes` and `Validation` retain the corresponding
interpreter contracts. Effect-row names come from the translated
`LibExecution/Effects.fs` vocabulary.

Keep compiler concerns after the parser boundary in `frontend/WrittenChecking`:
name resolution, native typing, symbol allocation and lowering do not belong in
the translated productions. In particular, `?` remains `EUnwrap`; converting it
to `Builtin.unwrap` would replace early return with an exception. Its native
execution support is tracked separately in the language ledger.

Host adaptations are UTF-8 scalar scanning, OCaml numeric representations and
collections, and catching `Stack_overflow` where .NET checks available stack.
The expression/type/pattern depth cap remains 200 and interpolation cap 64.
Unicode-source coordinate differences are outside the compatibility assessment.

When updating the interpreter pin, compare every changed tokenizer, lexer,
parser, written-type and validation production, including removals and helper
refactors. Update this provenance and the parser ledger only after that review.
The parser-port regression covers written meaning and structural rejection;
syntax and E2E fixtures cover the executable compiler boundary. Passing compiler
tests alone does not establish an exhaustive differential comparison.
