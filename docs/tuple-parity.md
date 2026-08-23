# Tuple parity

This parity repair revalidated compiler worktree baseline
`87ebadfc66e6e807c35c99988f35e0733c4b9ced`, approved compiler evidence
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899`, and darklang/dark revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit
`8a402797ccccda0ca47b516b356ae1de4d670038` was used only as a lead and its
findings were rechecked against these sources.

The interpreter baseline is `backend/src/LibParser/Parser.fs` for tuple syntax,
`backend/src/LibExecution/ProgramTypes.fs` and `RuntimeTypes.fs` for tuple
values/types/patterns, `ProgramTypesToRuntimeTypes.fs` and `Interpreter.fs` for
construction and recursive matching, plus
`backend/testfiles/execution/language/collections/dtuple.dark`,
`language/basic/elet.dark`, `language/flow-control/ematch.dark`, and
`backend/testfiles/execution/stdlib/tuple.dark` for same-source construction,
access, destructuring, matching, equality, and evaluation-order probes.

Compiler evidence is `passes/1_Parser.fs` and `1_InterpreterParser.fs` for
grouping, unit, tuple construction, and public syntax; `1.5_TypeChecking.fs`
for ordered positional types and exact bounds; `2_AST_to_ANF.fs` for ordered
allocation, recursive patterns, and structural equality; `Runtime.fs` and
`passes/1.6_ValueRendering.fs` for rendering; and `tuples.e2e` together with
`tuple-parity.e2e` for executable coverage.

Public behavior is an ordered heterogeneous tuple of two or more elements.
`()` is primitive unit, `(value)` is grouping, singleton/trailing-comma input is
rejected, nested tuple patterns destructure recursively, and equality is
structural. Tuple allocation checks elements in source order and ANF preserves
that left-to-right order. Tuple types use `A * B`; parenthesized comma tuple
types are rejected in annotations and call-site type arguments. Bare comma
tuples are accepted only as match scrutinees. Interpreter syntax rejects numeric
projection; callers use destructuring or `Stdlib.Tuple2`/`Stdlib.Tuple3`.

Both public parsers reject numeric projection and comma tuple types.
Tuple-bearing compiler sources use star tuple types and destructuring or
`Stdlib.Tuple2`/`Stdlib.Tuple3`, matching the interpreter.
Internal-mode parser input and generated compiler AST retain `TupleAccess` as an
AOT-only typed lowering operation; it is not public source syntax. Multi-argument
compiler function types `(A, B) -> C` remain a documented compiler-only function
type extension and are not interpreted as tuple types.
