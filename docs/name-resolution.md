# Name resolution parity

This document is the checked-in semantic matrix for callable and namespace
resolution. The comparison is pinned to compiler
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and darklang/dark
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation began from
compiler HEAD `b3a301203bd0377e37887995f49bf0880315df28` after rebasing on local
`main`. DCB1 report `8a402797` was used only to locate likely gaps; every row
below was revalidated against the pinned sources and focused compiler probes.

## Source evidence

The interpreter rule table is implemented in
`backend/src/LibParser/WrittenTypesToProgramTypes.fs`: arguments and lexical
bindings are checked before global resolution; non-applied names resolve value
then function; applied names resolve function then value; qualified field-like
paths use the same category order. Package and builtin lookup is in
`backend/src/LibParser/NameResolver.fs`, with ordered namespace candidates in
`backend/src/LibDB/NameLookup.fs`. Builtins are registered by
`backend/src/Builtins/Builtins.Pure/Builtin.fs` and combined into exact
function/value dictionaries by `backend/src/LibExecution/Builtin.fs`.

The revalidated compiler gaps were the fallback functions in
`src/DarkCompiler/Stdlib.fs`, `src/DarkCompiler/passes/1.5_TypeChecking.fs`, and
`src/DarkCompiler/passes/2_AST_to_ANF.fs`, plus order-dependent constructor and
declaration maps. Focused probes retained in `src/Tests/e2e/name-resolution.e2e`
cover implicit qualification, lexical shadowing, duplicate declarations,
constructor collisions, exact qualification, and missing callables.

## Semantic matrix

`local` means a parameter, lambda parameter, pattern binding, or `let` binding.
An identity repeated through aliases is deduplicated before ambiguity testing.
Candidates at the same winning precedence are sorted by rendered identity.

| Parsed form | Context | Candidate combination | Result | Classification |
| --- | --- | --- | --- | --- |
| `x` | value | local + value + function | local | parity |
| `x` | value | value + function | value | parity |
| `x` | value | function only | function identity as a value | parity |
| `f` applied | callable | local + function + value | local callable value | parity |
| `f` applied | callable | function + value | function | parity |
| `f` applied | callable | value only | value, validated as callable by typing | parity |
| `A.B.f` | value | exact qualified value + function | value | parity |
| `A.B.f` applied | callable | exact qualified function + value | function | parity |
| `A.B.f` | any | only `Stdlib.A.B.f` exists | unresolved | parity; implicit compiler insertion removed |
| `Builtin.f` | callable | registered builtin function | builtin `(f, version)` identity | parity |
| `Builtin.v` | value | registered builtin value | builtin `(v, version)` identity | parity |
| `f_v0` | value/callable | explicit version-zero inventory alias | registered version-zero identity | parity |
| `Case` | constructor expression | one declaring type | constructor identity | parity |
| `Case` | constructor expression | cases in two declaring types | ambiguous, ordered identities | parity |
| `T.Case` | constructor expression | exact declaring type/case | constructor identity | parity |
| `Case` | constructor pattern | cases in several types | scrutinee type selects identity | parity |
| `T` | type | one user type identity | user type | parity |
| `A.T` | type | exact qualified user type | user type | parity |
| any valid form | any context | two distinct equal-precedence identities | ambiguous, ordered identities | parity classification |
| any valid form | any context | no category-valid candidate | structured unresolved error | parity classification |
| empty segment such as `A..f` | any context | n/a | structured invalid-name error | parity classification |

## Identity and boundary

`NameResolution.fs` represents qualified names, reference context, module,
package and builtin namespaces, local/module/package/builtin values and
functions, constructors, user and builtin types, candidate provenance,
successful resolution, and structured errors as discriminated unions. A pure
resolver gathers only candidates whose complete parsed qualified name matches;
it never retries a suffix, prepends `Stdlib`, or changes reference category.

The type-checking boundary builds the immutable inventory from lexical scope,
top-level declarations, the inherited package-like environment, constructors,
types, intrinsic registrations, and exact builtin registrations. It rewrites
checked references to their canonical identity spelling. AST-to-ANF lookup is
therefore exact; a missing name there is an internal compiler invariant rather
than a second user-name search.

Diagnostics preserve the original qualified name and reference context.
Ambiguities additionally carry sorted, deduplicated symbol identities. The
stable rendered classes are `Invalid <context> name`, `Unresolved <context>
name`, and `Ambiguous <context> reference`.

## Extensions and intentional divergences

- The compiler's native `File`, `Path`, `Platform`, `Random`, raw-memory, and
  related intrinsic catalog remains a compiler-only extension. Each entry is an
  explicit `CompilerExtension` candidate under its full registered spelling.
- Both parser modes accept module headers and blocks, then flatten their typed
  paths at the explicit parser-to-AST normalization boundary. The compiler does
  not load content-addressed packages. Imported compilation environments model
  the same precedence boundary, but package hashes and dependency traversal
  remain an intentional, documented program-model divergence.
- The compiler AST has no top-level value declaration. Value identities remain
  explicit in the resolver for lexical, inherited package, and registered
  builtin candidates. The parser recognizes `val` and reports the unsupported
  AOT initialization boundary explicitly; source constants such as
  `Stdlib.Math.pi` are compiler nullary functions and must be called with `()`.
- Repeated flattened type declarations with the same canonical type identity
  are identity-deduplicated. This preserves existing module-adapter behavior;
  distinct type identities and distinct constructor owners remain ambiguous.
- Compiler-generated runtime helpers bypass source lookup because their
  identities are introduced after type checking. They are not public resolution
  extensions.

The immutable package-value snapshot used by `ValueSearch` is specified in
[diff-value-search-parity.md](diff-value-search-parity.md). It is an explicit
AOT bridge and does not add live package lookup or top-level values to this
resolution model.

Performance-only differences are outside this matrix.

Lexical binding construction, duplicate validation, non-recursive RHS scope,
shadow restoration, and definition-time capture are specified separately in
[binding-parity.md](binding-parity.md). Each accepted binder creates a new
lexical identity; child expression environments are discarded when that child
finishes, and a shadowed outer identity is restored rather than reconstructed
from its text.
