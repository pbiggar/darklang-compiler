# Tuple parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

The interpreter baseline is `backend/src/LibParser/Parser.fs` for tuple syntax,
`backend/src/LibExecution/ProgramTypes.fs` and `RuntimeTypes.fs` for tuple
values/types/patterns, `ProgramTypesToRuntimeTypes.fs` and `Interpreter.fs` for
construction and recursive matching, plus
`backend/testfiles/execution/language/collections/dtuple.dark`,
`language/basic/elet.dark`, `language/flow-control/ematch.dark`, and
`backend/testfiles/execution/stdlib/tuple.dark` for same-source construction,
access, destructuring, matching, equality, and evaluation-order probes.

Compiler evidence is `src/frontend/WrittenChecking.ml` for ordered positional types and
exact bounds, `src/passes/anf/AST_to_ANF.ml` for ordered allocation, recursive patterns,
and structural equality, `src/memory/RuntimeDataLayout.ml` and `src/frontend/ValueRendering.ml` for
rendering, and `tuples.e2e` together with `tuple-parity.e2e` for executable
coverage.

Public behavior is an ordered heterogeneous tuple of two or more elements.
`()` is primitive unit, `(value)` is grouping, singleton/trailing-comma input is
rejected, nested tuple patterns destructure recursively, and equality is
structural. Tuple allocation checks elements in source order and ANF preserves
that left-to-right order. Tuple types use `A * B`; projection accepts numeric postfix selectors such as
`value.1` and `value.1.2`, destructuring, or `Stdlib.Tuple2`/`Stdlib.Tuple3`.

Generated compiler AST retains `TupleAccess` as an AOT-only typed lowering
operation used by compiler-owned sources. Multi-argument function types use
`A -> B -> C`.
