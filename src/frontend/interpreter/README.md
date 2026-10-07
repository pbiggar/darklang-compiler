# Darklang parser

The parser scans Unicode scalars, preserves source ranges and trivia, validates
file-purpose boundaries, and checks declarations directly into `CheckedAST`
through `frontend/WrittenChecking.ml`.

The parser and effect definitions are derived from darklang/dark commit
`1cc4bb7f63acdf29dc66458f3c401ed91d444775`, under `backend/src/LibParser/`
and `backend/src/LibExecution/Effects.fs`. The Apache-2.0 license is included
as [LICENSE.md](LICENSE.md).

`ParserDependencies.ml`, `Prelude.ml`, and `ProgramTypesShim.ml` supply the
interpreter definitions used by the parser.
