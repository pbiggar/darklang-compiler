# Darklang Compatibility

The embedded public standard-library inventory follows darklang/dark release
`v0.0.35`, revision `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`.
The copied interpreter parser and effect syntax derive from the newer revision
`1cc4bb7f63acdf29dc66458f3c401ed91d444775`; these baseline pins are distinct.
Repository Dark source can be validated directly against the configured interpreter; there is no
compiler-to-interpreter syntax conversion layer. Compiler source and E2E tests
are authoritative for implemented behavior, and the revision-pinned ledgers in
this directory record AOT compatibility boundaries and intentional runtime or
standard-library extensions.

The consolidated [remaining non-AOT differences](remaining-differences.md)
ledger records current language and standard-library boundaries without
conflating them with earlier AOT diagnostics or test-harness limitations. The
[current audit](current-audit.md) records the compiler revision, source evidence,
validation limits, and status of every detailed ledger. The
[upstream inventory](upstream-test-inventory.md) enumerates every imported file
and the exact live exclusion entries.

Run the compatibility validator with:

```bash
python3 scripts/validate-darklang.py --help-full
python3 scripts/validate-darklang.py
```

## Source syntax

Public source uses space application (`f a b`, `f ()`), whitespace-separated
curried parameters (`let f (a: A) (b: B) : C = ...`), unsuffixed `Int`
literals, parenthesized tuple expressions, and `A -> B -> C` function types.
Lists accept comma, semicolon, or layout-separated elements, sized integer
suffixes and generic angle syntax are shared, and strings support interpolation
and the full scalar-aware escape alphabet.

`**` is exponentiation; `^` is bitwise XOR. The copied interpreter parser
and direct checker support `<<`, `>>`, `&`, `|`, unary `~`, and unary `!`,
with integer or Boolean operand checking as appropriate. Named standard-library
operations remain available. See `operator_parity.e2e` and `ParserTests.ml`.

Compiler-generated AST may contain `TupleAccess`, `RawPtr`, and internal
bitwise nodes. None has an
`allowInternal` parser spelling: internal mode controls identifier access, not
a second grammar.

The detailed language comparisons cover [bindings](language/bindings.md),
[identifiers](language/identifiers.md),
[name resolution](language/name-resolution.md),
[primitive literals](language/primitive-literals.md),
[tuples](language/tuples.md), [records](language/records.md),
[program structure](language/program-structure.md),
[conditionals and sequences](language/conditionals-and-sequences.md), and
[recursion](language/recursion.md).

The standard-library ledgers include the generic [Dict contract](stdlib/dicts.md),
the [List contract](stdlib/lists.md), and the
[Pretty-printing contract](stdlib/pretty.md).

## Validator skip reasons

Some native-runner observations cannot be compared through the interpreter
expression evaluator:

| Prefix | Meaning |
|---|---|
| `eval:*` | Compile errors, expected runtime errors, stdout, stderr, exit codes, or builtin test infrastructure |
| `syntax:*` | Source unsupported by the pinned interpreter or canonical parser |
| `semantic:*` | A compiler operation absent from or observably different from the interpreter |
| `stdlib:*` | A standard-library operation absent from the pinned interpreter |
| `extension:*` | An explicitly supported compiler extension |
| `internal:*` | Private compiler implementation surface |

High-precision float cases use `eval:float_precision` because the compiler's
shortest-roundtrip rendering intentionally differs from the pinned
interpreter. The [float and math ledger](stdlib/floats-and-math.md) owns that
contract.

## AOT-only boundaries

The compiler supports integer `/` with truncation toward zero; the interpreter
uses `/` for Float and named functions for integer division. Private native
intrinsics and SkewList/HAMT helpers remain implementation details and require
internal compilation mode. Compiler support source files use `__` filenames;
these filenames do not enforce language visibility. The
[complete source inventory](../compiler/library-sources.md) distinguishes
interpreter packages, private fragments, native implementation modules, and
private compiler extensions such as `Cli.__Args`. Compiler-only module
declarations use `__` names and require internal compilation mode. Source placement alone does
not establish interpreter API parity.

## Intentional AOT differences

The compiler rejects some invalid programs during static type checking where
the interpreter reaches an equivalent runtime error. Individual ledgers own
those timing differences. The principal areas are conditional arm and sequence
types, mixed or nominal equality operands, invalid stream ordering, and invalid
standard-library argument types.

CLI and host behavior is documented separately in the [CLI ledger](cli.md),
[presentation ledger](cli-presentation.md), and
[process/host/input ledger](cli-process-host-input.md).

Internal functions and data structures are implementation details. An
extension must remain identified in the relevant ledger and validator rule; it
must not be presented as interpreter parity.
