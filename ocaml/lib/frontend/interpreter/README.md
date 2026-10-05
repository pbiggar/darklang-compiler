# Translated Darklang parser

The OCaml `Tokenizer`, `Lexer`, `Parser`, `WrittenTypes`, and `Validation`
modules faithfully translate the F# sources from darklang/dark commit
`1cc4bb7f63acdf29dc66458f3c401ed91d444775`, under
`backend/src/LibParser/`. `LibExecution_Effects.ml` translates that commit's
`backend/src/LibExecution/Effects.fs`. The source is licensed under Apache-2.0;
the upstream license is copied here as [LICENSE.md](LICENSE.md).

`ParserDependencies.ml`, `Prelude.ml`, and `ProgramTypesShim.ml` are local
integration definitions for the small set of interpreter modules referenced by
the parser. The original F# compiler sources remain in Git history at
`df9dae7e1647275f6bc9104618f20ef84a7251be`.

The compiler parses with this translated parser, validates its `WrittenTypes`
output, and checks declarations directly into `CheckedAST` through
`frontend/WrittenChecking.ml`.
