# Binding parity

Recursive binding scope and declaration groups are covered by the focused
[recursion parity matrix](recursion-parity.md).

This comparison was revalidated against compiler revision
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and
`darklang/dark` interpreter revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation work began from
compiler HEAD `5ed3eebe4ddd0ddfaa31fc0e33dc0257950cf410`. Every comparison in this
document uses that exact revision pair; DCB1 report `8a402797` was starting
evidence only.

## Parity table

| Claim | Compiler probe | Interpreter evidence | Result |
| --- | --- | --- | --- |
| `let` accepts optional `in` and a layout continuation | `src/Tests/syntax/bindings.syntax`; `src/Tests/e2e/interpreter/bindings.e2e` | `backend/testfiles/execution/language/basic/elet.dark`; `LibParser/Parser.fs:1357-1443` | Shared |
| Let and lambda binders are variable, wildcard, unit, parenthesized, or nested tuple patterns only | `bindings.syntax`; restricted parsers at `src/DarkCompiler/passes/1_Parser.fs:1322` and `1_InterpreterParser.fs:1394` | dedicated `parseLetPattern`, `LibParser/Parser.fs:1306-1355` | Shared |
| Public lambdas use unannotated `fun patterns -> body` | `bindings.syntax`; compiler parser and interpreter-syntax parser probes | `backend/testfiles/execution/language/basic/elambda.dark`; `LibParser/Parser.fs:2044-2071` | Shared |
| Parameter and return annotations belong to local function declarations | `bindings.syntax`; `SyntaxInteropTests.fs` | `LibParser/Parser.fs` local-function production | Shared |
| The RHS is outside the new scope and runs before the continuation | `bindings.e2e` rebinding and use-before-binding probes | `LibParser/WrittenTypesToProgramTypes.fs:591-611` | Shared |
| Duplicate usable names reject the complete let/lambda binder set; `_` names do not bind | duplicate rejection cases in `bindings.syntax`; validator at `AST.fs:213-249` | `LibParser/Validation.fs:81-150,201-216` | Shared |
| Tuple destructuring commits atomically; mismatch skips the continuation | mismatch and evaluation-order cases in `bindings.e2e`; lowering at `2_AST_to_ANF.fs:4263-4339` | `LibExecution/ProgramTypesToRuntimeTypes.fs:721-730`; `LibExecution/Interpreter.fs:1882-1898` | Shared |
| Nearest binders shadow; closures capture the definition-time binding | rebinding and capture cases in `bindings.e2e`; `closures.e2e` | `elet.dark`, `elambda.dark`, and `earg.dark` | Shared |
| Child-expression bindings do not escape conditions, arms, matches, lambdas, operands, calls, or sequences | scope-isolation cases in `bindings.e2e`; resolver and ANF scope tests | interpreter name binding and evaluator expression frames | Shared |
| Match-only literal, constructor, list, and record patterns are rejected in bindings | rejection cases in `bindings.syntax` | `Parser.fs:1305-1355` | Shared |

The representation is one non-recursive `Let` node containing a restricted
`LetPattern`, RHS, and continuation (`AST.fs:168-181,277`). Lambda parameters
use the same pattern language. Parsed parameter annotations, inferred types,
and the optional local-function return annotation remain distinct. Match keeps
its more expressive `Pattern` type.

Type checking evaluates the RHS in the incoming type environment, validates
the entire binder structure, checks the complete tuple/unit shape, and extends
only the continuation. ANF lowering evaluates the RHS once, prepares every
projection before extending the continuation environment, and sends a known
mismatch directly to the runtime-error continuation. This ordering also gives
reference-count insertion one guarded ownership transition on both success and
failure.

## AOT-only diagnostics and intentional differences

The compiler resolves names and types before producing a binary. Consequently,
use-before-binding and names that would escape a child expression fail during
compilation (`Unresolved value name: ...`) instead of becoming an interpreter
runtime name error. This is an intentional AOT timing difference; visibility
and the rejected program are the same.

Unannotated lambdas receive types from their body and call context. When a
standalone lambda remains genuinely ambiguous for ahead-of-time code
generation, authors must use an annotated local function declaration. This is
the retained compiler extension: early static type failure, not a different
binding or evaluation rule.

The compiler's native-code target, static type checking, and early diagnostics
remain extensions. Fat-arrow lambdas, typed lambda binders, general-pattern
lets, last-wins duplicate binders, and the duplicate-warning override are not
extensions and have been removed.

## Verification

The repository-wide suite passed `5857/5857` tests with `./run-tests --ai`
after rebasing the implementation onto compiler integration HEAD `ef1887ca`.
The canonical routine profile was recorded after the intentional lowering and
syntax migration, then passed `./benchmarks/run_benchmarks.sh --verify routine`
for all 19 full-size benchmarks. The recorded performance ratio is **2.75x**
versus the audited Rust references (`benchmarks/RESULTS.md`).
