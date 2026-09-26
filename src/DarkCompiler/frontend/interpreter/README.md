# Copied Darklang parser

The `Tokenizer.fs`, `Lexer.fs`, `Parser.fs`, `WrittenTypes.fs`, and
`Validation.fs` files are copied unchanged from darklang/dark commit
`1cc4bb7f63acdf29dc66458f3c401ed91d444775`, under
`backend/src/LibParser/`. `Effects.fs` is copied unchanged from that commit's
`backend/src/LibExecution/Effects.fs`. The source is licensed under Apache-2.0;
the upstream license is copied here as [LICENSE.md](LICENSE.md).

`ParserDependencies.fs`, `Prelude.fs`, and `ProgramTypesShim.fs` are local
integration definitions for the small set of interpreter modules referenced by
the copied parser. They keep the parser implementation itself unchanged.

The current production compiler still uses `frontend/Parser.fs`. This copied
parser is present for the direct `WrittenTypes` to `CheckedAST` migration.
