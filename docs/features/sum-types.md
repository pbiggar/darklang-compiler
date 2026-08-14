# Algebraic Data Types and Constructors

This document records the compiler's user-defined enum behavior and the
interpreter evidence used as its parity baseline.

## Evidence revisions

The parity comparison is pinned to:

- compiler evidence revision `b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899`
- darklang/dark interpreter revision `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`
- implementation base `0a919617e2390be93506ef367225fe23bfbbf00a`

The historical DCB1 report at `8a402797` was used only to identify candidates.
Every behavior retained below was checked again in the pinned sources. The
same-source compiler matrix is `src/Tests/e2e/interpreter/adt_parity.e2e`;
declaration failures that occur while an E2E preamble is built are covered by
focused parser/type-checker tests instead.

Interpreter evidence is in `backend/src/LibParser/GRAMMAR.md`, enum declaration
types in `backend/src/LibExecution/ProgramTypes.fs`, runtime `DEnum` in
`backend/src/LibExecution/RuntimeTypes.fs`, construction in
`backend/src/LibExecution/TypeChecker.fs`, and equality in
`backend/src/Builtins/Builtins.Pure/Libs/NoModule.fs` at the pinned revision.
The pinned checkout and its tracked tests were source-audited directly. An
exact-revision interpreter execution was also attempted, but that checkout
could not restore because its generated `.paket/Paket.Restore.targets` was
absent and the repository's devcontainer launcher was unavailable. Compiler
expectations therefore encode the pinned interpreter source behavior; they are
not presented as results from a different installed interpreter revision.

## Public syntax

Interpreter syntax is the public parity surface:

```dark
type Color = | Red | Green | Blue

type Option<'a> = | None | Some of 'a

type Pair<'a, 'b> = | Pair of first: 'a * second: 'b

type OneTuple<'a, 'b> = | OneTuple of ('a * 'b)
```

The leading `|` and apostrophes on generic parameters are required. Fields in
`Pair of A * B` are two constructor arguments. Parentheses in
`OneTuple of (A * B)` preserve one tuple argument. Construction supports bare
and declaring-type-qualified references:

```dark
Red
Color.Red
Option.Some 42L
Pair.Pair(1L, "one")
OneTuple.OneTuple((1L, "one"))
```

Both parser modes reject the removed compiler declaration dialect: omitted
leading bars, qualified declaration names, bare declaration type parameters,
and `def` declarations. Public declarations use the interpreter spellings
shown above.

## Declaration and resolution model

`AST.TEnumFields` preserves the difference between several enum fields and one
tuple field. `AST.ConstructorReference` represents an unqualified reference,
a source-qualified reference, or a resolved declaring module and type without
an empty-string sentinel. Type checking predeclares all type names, validates
type and case duplicates, generic parameters, empty declarations, and field
duplicates, then resolves each constructor to its nominal declaring type.

Case names may repeat in different types. An unqualified reference is rejected
when more than one visible nominal type owns the case; qualification selects a
unique declaration. A qualified lookup never falls back to an unrelated short
name.

Both parser modes accept interpreter module headers and nested module blocks.
At the explicit parser-to-AST boundary, source module paths are flattened into
canonical function and type identities. Module-local opening, package loading,
and same-name collision behavior remain post-parse resolution boundaries.

## Typing and evaluation

Each payload use independently unifies the declaration's generic parameters
with its actual fields. Recursive payload references resolve against the
predeclared nominal type. Nullary cases are values. Payload cases require the
exact declared field count; no application form silently bundles, drops, or
invents fields.

Payload expressions are evaluated exactly once from left to right. ANF lowering
uses the same continuation-based atom binding as calls and tuple expressions,
then emits one constructor allocation. Arity and resolution errors occur before
runtime payload evaluation, matching the interpreter boundary.

## Native identity and layout

Cases whose display name occurs in more than one nominal declaration receive a
deterministic tag derived from the complete canonical `declaring-type.case`
identity. Unique case names retain compact declaration-order tags. Declaration
validation rejects the unlikely event that two colliding-name canonical
identities map to the same bounded native tag. This adaptive encoding preserves
compact matches while ensuring that same-named constructors from different
nominal types cannot collapse to one runtime identity.

`Stdlib.Option.Option` and `Stdlib.Result.Result` intentionally retain their
existing 0/1 native ABI because file, string, and other runtime intrinsics
construct those values directly. This is an internal compiler divergence from
the general encoding, not an observable nominal-equality divergence: their
declaring types remain distinct in type checking, rendering, and equality.

- A type whose cases are all nullary uses its case tag as an immediate.
- If any case has a payload, all values of that type use a two-word fixed block:
  case tag at offset 0 and payload (or zero for a nullary case) at offset 8.
- Multiple enum fields use one payload tuple block, preserving the public field
  count and order while retaining the existing backend and reference-counting
  layout.

Qualified entries, rather than collision-prone short aliases, build MIR/LIR
runtime metadata. Both ARM64 and x64 therefore consume the same resolved
nominal identity. Recursive managed payloads continue through the existing sum
shape and reference-counting plans.

## Rendering and equality

Values render in interpreter form, including concrete generic arguments:

```text
Color.Red
Option<Int64>.Some(42)
Pair<Int64, String>.Pair(1, "one")
```

Multiple fields render comma-separated; a single tuple field retains nested
parentheses. Recursive values render recursively.

Equality first respects nominal declaring type and constructor identity, then
recursively compares payload fields. Equal-looking cases from different types
are unequal. Comparing distinct nominal enum types is permitted, evaluates both
operands once, and produces `false` for `==` (`true` for `!=`).

## Implementation map

| File | Responsibility |
|---|---|
| `src/DarkCompiler/passes/1_InterpreterParser.fs` | Interpreter declaration grammar, field shape, constructor reference syntax |
| `src/DarkCompiler/AST.fs` | Enum field shape, unresolved/resolved references, canonical runtime identity |
| `src/DarkCompiler/passes/1.5_TypeChecking.fs` | Pure declaration validation, nominal resolution, generic/recursive typing, arity, equality |
| `src/DarkCompiler/passes/1.6_ValueRendering.fs` | Public enum rendering |
| `src/DarkCompiler/passes/2_AST_to_ANF.fs` | Resolved construction and once-only ordered payload evaluation |
| `src/DarkCompiler/Runtime.fs` | Shared native runtime support used by generated rendering and equality paths |
| `src/DarkCompiler/passes/arm64/6_CodeGen.fs` | ARM64 consumption of shared sum metadata |
| `src/DarkCompiler/passes/x64/6_CodeGen.fs` | x64 consumption of shared sum metadata |

## Test matrix

The parity E2E matrix covers nullary and payload cases, generic instantiation,
recursive types, multiple fields versus a tuple field, qualification,
same-case collisions across nominal types, exact arity, left-to-right payload
failure, rendering, and nominal equality. Compiler-pass tests cover duplicate
types, parameters, constructors and fields, undeclared parameters, empty
declarations, unknown type references, generic reference arity, removed
compiler-only interpreter syntax, and resolved-reference AST shape.
