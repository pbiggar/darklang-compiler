# Program structure parity

Current compiler source review: 2026-10-07 at `7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`.
See the [current audit](../current-audit.md) for post-port status and validation.
Older revision pairs and executed counts below are historical evidence, not
a fresh test result for this revision.

This ledger records the program-level comparison against compiler evidence
revision `b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and darklang/dark release
`v0.0.35`, revision `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`. Implementation and source
revalidation started from compiler HEAD
`a78567efd773de86265e55a54445ddf5a5a8911c`. DCB1 report `8a402797` and the
existing parity documents were used only as finding inventories.

The interpreter evidence was rechecked in `LibParser/Parser.fs`,
`LibParser/SourceFile.fs`, `LibParser/NameResolver.fs`,
`LibParser/WrittenTypesToProgramTypes.fs`, `LibDB/NameLookup.fs`,
`LibExecution/ProgramTypes.fs`, and `Builtins.CliHost/Libs/Cli.fs` at the pinned
revision. Compiler evidence was rechecked in `src/AST.ml`, `src/NameSyntax.ml`, both
parser passes, the whole-program section of `src/frontend/WrittenChecking.ml`,
`src/passes/anf/AST_to_ANF.ml`, `src/CompilerLibrary.ml`, `src/Program.ml`, and the e2e runner.

## Rule matrix

| Area | Compiler rule | Classification |
| --- | --- | --- |
| source composition | a compile request contains a non-empty, ordered collection of named units; each unit is parsed independently | AOT extension |
| unit purpose | executable, library, and package purposes are explicit; dependency units cannot contain entries | AOT extension |
| declarations | `let` declares functions, `val` declares first-class values, and `type` declares types | parity at the source boundary |
| modules | file modules and nested source modules retain typed paths until validated composition; lowering uses deterministic qualified native symbols | parity with an internal AOT symbol boundary |
| ordering | all declarations are inventoried before bodies are checked, so supported sibling function and type references are order-independent | parity |
| duplicates | functions and types repeated in one source batch are rejected by the direct declaration prepass; sequential values overwrite the value inventory | compiler difference; categories remain distinct |
| contextual lookup | lexical bindings win; bare references prefer values, applications prefer functions, and types use a separate namespace | parity |
| constructors | constructor identity includes its declaring type; unqualified equal case names remain contextual/ambiguous | parity |
| validation | declaration shape, name resolution, typing, constructor checks, specialization, and entry checks run before ANF | intentional AOT divergence: unused declarations are checked |
| declaration-only compilation | stdlib, preamble, and catalog-generated units type-check and lower without an injected expression | parity boundary |
| entry selection | exactly one expression is required across executable units; `main()` is never an implicit entry | intentional divergence: the interpreter executes several expressions |
| file completion | file entries accept only `Unit`, `Int`, or `Int64`; other statically known results are rejected | parity |
| eval completion | explicit eval mode renders non-`Unit` values and does not alter file entry selection | compiler interface behavior |
| packages | explicit source units and value catalogs are supported; optional `--package-server` resolves hosted declarations and dependencies before checking | compile-time package support; no runtime service claim |

Top-level value declarations are represented explicitly by `SourceValue`,
`AST.ValueDef`, and checked value definitions. They participate in source-tree,
name-resolution, type-checking, separate-compilation, and entry validation.
Before ANF, required values are materialized once per execution scope as
ordinary lexical bindings, so their bodies use the same ownership and lowering
paths as any other expression. No parser or ANF registry hard-codes particular
module values.

## Focused probes

`test/compiler-passes/ProgramStructureTests.ml` covers ordered multi-unit composition, dependency
entry rejection, zero/multiple entry cardinality, last-wins function overlays,
and file-result validation. Canonical syntax fixtures cover retained module
shape and declaration boundaries. `name-resolution.e2e` covers contextual
lookup, last-wins duplicates, constructor identity, exact qualification, and
missing names.

Performance-only differences and native object/executable layout are outside
this ledger.
