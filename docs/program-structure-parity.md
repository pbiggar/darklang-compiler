# Program structure parity

This ledger records the program-level comparison against compiler evidence
revision `b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and darklang/dark revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation and source
revalidation started from compiler HEAD
`a78567efd773de86265e55a54445ddf5a5a8911c`. DCB1 report `8a402797` and the
existing parity documents were used only as finding inventories.

The interpreter evidence was rechecked in `LibParser/Parser.fs`,
`LibParser/SourceFile.fs`, `LibParser/NameResolver.fs`,
`LibParser/WrittenTypesToProgramTypes.fs`, `LibDB/NameLookup.fs`,
`LibExecution/ProgramTypes.fs`, and `Builtins.CliHost/Libs/Cli.fs` at the pinned
revision. Compiler evidence was rechecked in `AST.fs`, `NameSyntax.fs`, both
parser passes, the whole-program section of `1.5_TypeChecking.fs`,
`2_AST_to_ANF.fs`, `CompilerLibrary.fs`, `Program.fs`, and the e2e runner.

## Rule matrix

| Area | Compiler rule | Classification |
| --- | --- | --- |
| source composition | a compile request contains a non-empty, ordered collection of named units; each unit is parsed independently | AOT extension |
| unit purpose | executable, library, and package purposes are explicit; dependency units cannot contain entries | AOT extension |
| declarations | `let` declares functions, `val` is retained by the recursive source tree, and `type` declares types; public `def` is rejected | parity at the source boundary |
| modules | file modules and nested source modules retain typed paths until validated composition; lowering uses deterministic qualified native symbols | parity with an internal AOT symbol boundary |
| ordering | all declarations are inventoried before bodies are checked, so supported sibling function and type references are order-independent | parity |
| duplicates | the last declaration at the same category and qualified location wins; type, value, and function categories are distinct | parity |
| contextual lookup | lexical bindings win; bare references prefer values, applications prefer functions, and types use a separate namespace | parity |
| constructors | constructor identity includes its declaring type; unqualified equal case names remain contextual/ambiguous | parity |
| validation | declaration shape, name resolution, typing, constructor checks, specialization, and entry checks run before ANF | intentional AOT divergence: unused declarations are checked |
| declaration-only compilation | stdlib, preamble, and catalog-generated units type-check and lower without an injected expression | parity boundary |
| entry selection | exactly one expression is required across executable units; `main()` is never an implicit entry | intentional divergence: the interpreter executes several expressions |
| file completion | file entries accept only `Unit`, `Int`, or `Int64`; other statically known results are rejected | parity |
| eval completion | explicit eval mode renders non-`Unit` values and does not alter file entry selection | compiler interface behavior |
| packages | package inputs are immutable compile-request snapshots rather than live package-manager queries | AOT extension |

Top-level value declarations are represented explicitly by `SourceValue` and
participate in source-tree and entry validation. Native materialization remains
an explicit lowering boundary: programs that reach the legacy expression AST
with a source value receive the deterministic diagnostic “Top-level value
declarations are parsed but native execution is not supported.” This is not
claimed as runtime parity; it remains the contained follow-up boundary rather
than being hidden by flattening or a magic declaration form.

## Focused probes

`ProgramStructureTests.fs` covers ordered multi-unit composition, dependency
entry rejection, zero/multiple entry cardinality, last-wins function overlays,
and file-result validation. `SyntaxInteropTests.fs` covers retained source-unit
identity/purpose, module shape, values at the source boundary, and
declaration-only validation. `name-resolution.e2e` covers contextual lookup,
last-wins duplicates, constructor identity, exact qualification, and missing
names. Legacy `def` remains only in explicit rejection/tooling probes.

Performance-only differences and native object/executable layout are outside
this ledger.
