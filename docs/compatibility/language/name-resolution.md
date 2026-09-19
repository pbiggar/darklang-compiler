# Name resolution parity

This document is the checked-in semantic matrix for callable and namespace
resolution. The comparison is pinned to compiler
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and darklang/dark release
`v0.0.35`, revision `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`. Implementation began from
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
`src/DarkCompiler/Stdlib.fs`, `src/DarkCompiler/frontend/TypeChecking.fs`, and
`src/DarkCompiler/passes/anf/AST_to_ANF.fs`, plus order-dependent constructor and
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
| `Parent.f` in `Darklang.App.Parent.Child` | any | progressively shorter current-module prefixes | `Darklang.App.Parent.f` | parity |
| `A.B.f` at the root | any | only `Darklang.Stdlib.A.B.f` exists | unresolved | parity; there is no blanket stdlib opening |
| `Stdlib.A.B.f` | any | canonical `Darklang.Stdlib.A.B.f` exists | canonical stdlib identity | parity; explicit stdlib shortcut |
| `Option` / `Result` | type | no nearer relative declaration exists | canonical `Darklang.Stdlib` type | parity; special global fallback only for these types |
| `Builtin.f` | callable | registered builtin function | builtin `(f, version)` identity | parity |
| `Builtin.v` | value | registered builtin value | builtin `(v, version)` identity | parity |
| `f_v0` | value/callable | explicit version-zero inventory alias | registered version-zero identity | parity |
| `Case` | constructor expression | one declaring type | constructor identity | parity |
| `Case` | constructor expression | cases in two declaring types | ambiguous, ordered identities | parity |
| `T.Case` | constructor expression | exact declaring type/case | constructor identity | parity |
| `Case` | constructor pattern | cases in several types | scrutinee type selects identity | parity |
| `T` | type | one user type identity | user type | parity |
| `A.T` | type | exact qualified user type | user type | parity |
| repeated declaration | same category and qualified location | source-order overlay | last declaration wins | parity |
| any valid form | any context | two distinct equal-precedence identities not related by overlay | ambiguous, ordered identities | parity classification |
| any valid form | any context | no category-valid candidate | structured unresolved error | parity classification |
| empty segment such as `A..f` | any context | n/a | structured invalid-name error | parity classification |

## Identity and boundary

`NameResolution.fs` represents qualified names, reference context, module,
package and builtin namespaces, local/module/package/builtin values and
functions, constructors, user and builtin types, candidate provenance,
successful resolution, and structured errors as discriminated unions. A pure
candidate generator prepends the complete current module, removes its innermost
segment until reaching the root, and selects the first candidate present in the
immutable inventory. `Stdlib.*` additionally maps to the canonical
`Darklang.Stdlib.*` identity, while bare Option and Result type/constructor
spellings have their interpreter-defined global fallbacks. Candidate generation
never changes the reference category.

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

- Native file, process, entropy, raw-memory, and representation primitives are
  private implementation names. Public compiler-only catalog entries were
  removed; public names resolve only to upstream functions, values, types, or
  builtins.
- The canonical parser accepts module headers and blocks and retains their
  module paths through name resolution, then lowers references to deterministic
  qualified backend symbols. The compiler does
  not load content-addressed packages. Imported compilation environments model
  the same precedence boundary, but package hashes and dependency traversal
  remain an intentional, documented program-model divergence.
- `val` declarations are first-class program declarations. Top-level, inherited,
  and builtin values resolve in the value namespace, are type-checked once, and
  are materialized once per execution scope as lexical bindings before ANF.
  Upstream constants such as
  `Stdlib.Math.pi`, `Stdlib.List.empty`, and `Stdlib.Blob.empty` are values and
  are referenced without `()`. Qualified values declared in nested user modules
  remain a compatibility gap recorded in
  [remaining differences](../remaining-differences.md).
- Repeated flattened type declarations with the same canonical type identity
  are identity-deduplicated. This preserves existing module-adapter behavior;
  distinct type identities and distinct constructor owners remain ambiguous.
- Compiler-generated runtime helpers bypass source lookup because their
  identities are introduced after type checking. They are not public resolution
  extensions.

The immutable package-value snapshot used by `ValueSearch` is specified in
[Diff and ValueSearch compatibility](../stdlib/diff-and-value-search.md). It is an explicit
AOT bridge and does not add live package lookup to this resolution model.

Performance-only differences are outside this matrix.

Lexical binding construction, duplicate validation, non-recursive RHS scope,
shadow restoration, and definition-time capture are specified separately in
[binding compatibility](bindings.md). Each accepted binder creates a new
lexical identity; child expression environments are discarded when that child
finishes, and a shadowed outer identity is restored rather than reconstructed
from its text.
