# Identifier and qualified-name parity

This lexical comparison is pinned to compiler evidence revision
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and darklang/dark interpreter
revision `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation started from
compiler HEAD `83e21f79edbb28857e9f71b0cc27b75d4b612417`, after rebasing on local
`main`. DCB1 report commit `8a402797` and older parity documents were starting
evidence only; every retained row below was revalidated at the pinned revisions.

Pinned interpreter evidence is `backend/src/LibParser/Lexer.fs:477-501,
689-824,905-965`, `backend/src/LibParser/Parser.fs:529-554,1148-1219,
1305-1355,1917-2038,2511-3054`, and `backend/src/LibParser/GRAMMAR.md:23-33,
108-111,172-174,224-281`. Historical compiler evidence began in the now-removed
`src/DarkCompiler/passes/1_Parser.fs` at the pinned compiler revision. The
implemented contract is centralized in `src/DarkCompiler/NameSyntax.fs` and
consumed by the sole compiler parser,
`src/DarkCompiler/passes/1_InterpreterParser.fs`.

## Revalidated matrix

“Same” means the current compiler parser matches the pinned interpreter.
AST spellings are the explicit normalization boundary consumed by the existing
resolver; `NameSyntax.QualifiedName` remains segment-aware until that boundary.

| Name or boundary | Compiler/interpreter acceptance | Parsed shape | Focused probe | Residual classification |
| --- | --- | --- | --- | --- |
| `alpha`, `_alpha`, `élève`, `name2`, `name'` | Same Unicode start/continuation grammar | `OrdinaryIdentifier`; apostrophe stays in value names | apostrophe and keyword-prefix cases in `names.syntax` | none |
| ``` ``anything`` ``` | Same; contents may contain punctuation or dots | one identifier segment | quoted-dot roundtrip and F# segment assertion | none |
| empty ``` ```` ``` and `___` | Same blank name | `BlankIdentifier`, not a control sentinel | blank syntax cases and typed-shape assertion | legacy AST boundary renders canonical `___` |
| unterminated quoted name | Same line-bounded rejection | lexical error | shared `NameSyntax.scanQuoted` | compiler errors rather than building a recovery tree |
| `let`, `val`, `in`, `if`, `elif`, `then`, `else`, `type`, `of`, `match`, `with`, `fun`, `when`, `true`, `false`, `_` | Same exact reserved set | dedicated keyword token | all-reserved quoted-name case | none |
| `def`, `rec`, `private`, `internal` | Same ordinary identifiers | `OrdinaryIdentifier` | contextual-name function case | none |
| `module` | Same contextual declaration word | identifier in expressions; typed path at item start | contextual and nested-module probes | modules flatten before type checking |
| value references and let/lambda binders | Same lexical acceptance | lowercase/blank value or binder; uppercase expression is constructor-shaped | `bindings.syntax`, `names.syntax` | lookup remains resolver-owned |
| function names and parameters | Same unqualified identifier grammar; reserved names require quotes | declaration and parameter identifiers | public declaration and reserved-name cases | AOT declarations require type annotations |
| type declaration names | Same lexical acceptance | unqualified identifier | module/parser probes | identity validation is semantic |
| type references | Same: uppercase custom type, lowercase variable | custom-type path or `TVar` | generic and ADT probes | runtime type inventory is semantic |
| declaration type parameters | Same adjacent apostrophe-prefixed list | `TTypeVar`, stored without apostrophe | public generic and bare-parameter rejection | none |
| enum cases | Same uppercase enforcement | constructor/case node | existing ADT tests | the only retained parser capitalization requirement |
| record fields | Same identifier grammar | separate field identifier | quoted `val` and keyword-field fixtures | none |
| module/package segments | Same segment grammar | non-empty `QualifiedName<Identifier>` | nested-module assertion | package lookup-name validation is resolver behavior |
| `A.B.value` | Same uppercase-led qualification | module-qualified value/function path | stdlib calls and resolution E2E | lookup is out of scope |
| `A.value.field` | Same stop after lowercase `value` | `RecordAccess(Var "A.value", "field")` | syntax and narrow AST assertion | none |
| `A.``b.c``` | Same single quoted final segment | segments `["A"; "b.c"]` | exact format and F# assertion | legacy spelling retains quotes until resolver reparses it |
| empty qualification segment | Same rejection | malformed name, never fabricated segment | invalid-name resolution tests | diagnostics are compiler-shaped |
| keyword prefix (`lettuce`) | Same single identifier | ordinary identifier | `names.syntax` | none |
| glued number (`123abc`, `1.5abc`, `12l3`) | Same rejection as one unit | no number/name split | compiler/interpreter syntax probes | numeric defaults are out of scope |
| adjacent `Name<T>` | Same generic interpretation | name plus type arguments | generic and apostrophe call tests | none |
| spaced or comment-separated `Name < T` | Same comparison interpretation | `TSpacedLt`, lowered as comparison | spaced and comment-separated declaration-generic rejection in syntax fixtures | comments preserve the non-adjacent boundary |
| `let f`, `val x`, module header/block | All declaration starters recognized | module path typed, functions/types normalized | module and `val` boundary probes | top-level value execution is absent |
| `def f`, `let A.f`, declaration `<a>` | Same rejection as legacy syntax | no alias | explicit rejection cases | none |

## Printing and normalization

`NameSyntax.isBareIdentifier` and `formatIdentifier` are shared by both syntax
printers. Apostrophe suffixes print bare; reserved words and embedded dots print
with double backticks; blank names print as `___`. Qualified formatting handles
each typed segment independently, so quoted dots are never split.

Module headers and indented blocks are parsed as non-empty typed paths, then
flattened by prefixing normalized function and type identities before type
checking. Qualified declaration spellings are not a shortcut around this
boundary.

## Non-lexical divergences

- Source modules flatten before AOT typing/resolution; module-open environments
  are not implemented.
- `val` reaches an explicit unsupported normalization error because native
  top-level value initialization/execution is absent. It is not a `let` alias.
- Content-addressed package loading is absent. Package lookup-name validation
  remains resolver behavior, matching interpreter `NameResolver.fs:12-29`.
- AOT unresolved/ambiguous diagnostics occur before execution.
- Runtime helpers live under `Stdlib.Internal.*` or use `__` names and are
  rejected by the post-parse visibility check.
- Explicit compiler intrinsic candidates remain classified in
  [name-resolution.md](name-resolution.md). Performance is outside this matrix.

Imported `src/Tests/e2e/upstream/` fixtures remain unchanged as same-source
evidence. Repository-authored stdlib, E2E, optimization, benchmark, roundtrip,
and documentation sources use `let`, modules, and apostrophe-prefixed
declaration parameters.
