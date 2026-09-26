# Interpreter parser migration

This plan replaces the compiler's source parser with Darklang's parser and makes
validated `WrittenTypes` the single source syntax tree. The intended source
path is:

```text
source → validated WrittenTypes → compiler validation, resolution, and checking
       → CheckedAST
```

The checker may use short-lived semantic facts while it works, but the finished
source path should not construct a second whole-program parsed or semantic AST.
Compiler-generated semantic programs can keep their separate entry path where
they do not originate as source text.

The compatibility baseline is darklang/dark `v0.0.35`, revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`, as recorded in the
[compatibility overview](../compatibility/overview.md). Pin the copied source
to that revision, record its provenance and Apache-2.0 notices, and review any
later interpreter changes as separate language changes. In particular, later
interpreter HEAD uses `**` for exponentiation and `^` for bitwise xor; the
pinned grammar uses `^` for exponentiation.

## Invariants throughout the migration

- Production source paths use one parser and one source grammar for public
  files, internal compiler sources, package units, and tests. The temporary
  comparison path does not choose a grammar by source file. Internal access
  controls may admit privileged names and signatures, but must not create a
  second grammar.
- A successful frontend result is fully checked. Invalid syntax, patterns,
  guards, bindings, exhaustiveness, declarations, types, and entry ownership
  fail before lowering or execution. Parser recovery nodes and diagnostics
  never reach `CheckedAST`.
- Parse each source unit independently. Preserve source order, module scope,
  duplicate declaration behavior, executable versus dependency purpose, and
  the exactly-one-entry rule across executable units.
- Keep source positions and structured parser diagnostics at least until the
  last stage that needs them. Do not reduce them to generic errors prematurely.
- Preserve compiler-only semantic nodes for generated programs without
  exposing them as public `WrittenTypes` syntax.
- Follow the repository's functional F# conventions when adapting upstream
  code. The interpreter implementation uses mutable parser and lexer state, so
  copying its files verbatim is not the final implementation.

## Stage 0: Freeze the behavior contract

Inventory parser entry points and consumers, including CLI compilation,
stdlib and preamble loading, package composition, formatting, syntax tests,
and the compatibility validator. Record the current accepted and rejected
grammar against the pinned interpreter revision. Add focused failing E2E cases
before fixing any discovered compiler behavior, including diagnostic phase
assertions where relevant. Use the existing syntax and E2E corpus as the
regression baseline.

**Done when:** the proposed adapter has a written mapping for every
`WrittenTypes` expression, declaration, pattern, and type form; unsupported
forms and intentional compiler extensions have explicit diagnostic decisions.

## Stage 1: Bring in the pinned parser core

Adapt the interpreter's tokenizer, lexer, parser, `WrittenTypes`, and structural
validation into compiler-owned modules. Retain the pinned grammar and parser
diagnostic behavior. Remove dependencies on interpreter execution, database,
package-manager, and effect machinery at this syntax boundary; define local
types or explicit unsupported-feature diagnostics where needed. Refactor
mutable state and throwing paths to comply with the compiler's F# rules.

At this stage the existing production parser remains the entry point. Exercise
the new parser on focused syntax cases and the source corpus, checking that
successful parse results contain no recovery holes and that invalid input
returns diagnostics.

**Done when:** the copied parser builds independently of the interpreter
projects and accepts the pinned public grammar without changing production
compilation.

## Stage 2: Lower `WrittenTypes` through the current frontend

Implement one explicit, total adapter from validated `WrittenTypes` into the
compiler's existing `NameSyntax`/`ParsedProgram` path. Map source type
references, operator and pipe forms, nested declarations, qualified names,
patterns, and expression order. Keep file-purpose and whole-program entry
validation in force. For privileged compiler sources, recognize existing
internal signatures such as `RawPtr` only after the same syntax pass and
reject them in public mode.

Run the old and new paths over representative source files, comparing
observable compile and runtime behavior plus expected compilation failures.
Differences must become a focused E2E test and either a corrected adapter or
an explicit compatibility decision; AST-shape comparisons alone are not a
success criterion.

**Done when:** the adapter can compile the supported corpus with no unexplained
behavior or diagnostic-phase differences.

## Stage 3: Switch production parsing

Route all source entry points through the new parser and adapter, including
stdlib, preambles, packages, CLI, tests, and formatting tools. Remove the old
lexer, layout rewrites, recursive-descent parser, and obsolete parser-only
tests. Keep meaningful lexer and syntax invariants covered by focused tests.
Update parser, pipeline, and compatibility documentation to name the new
authority.

**Done when:** there is one production parser, the full applicable test suite
passes, and the parent benchmark gate shows no regression.

## Stage 4: Make `WrittenTypes` the retained source representation

Move source-unit composition and entry validation from
`NameSyntax.ParsedSource` onto validated `WrittenTypes` plus explicit unit
name and purpose. Keep the current declaration overlay and module-scope
semantics. Remove `NameSyntax.ParsedSource` and its superseded conversion after
all source consumers have moved. Keep lexical name utilities only where they
still provide an independent contract.

**Done when:** `WrittenTypes` owns retained source-unit structure and
validation still rejects invalid entry ownership before checking or lowering.
The adapter's temporary `ParsedProgram` remains until Stage 5.

## Stage 5: Remove `ParsedProgram` from the source path

First make the frontend entry point accept validated `WrittenTypes` and return
`CheckedAST.Program` plus the type environment. Internally it may briefly
lower to the current semantic `AST.Program` while the checker is being moved.
Then migrate declaration inventory, name resolution, type checking, and
checked-node construction to consume `WrittenTypes` and explicit resolved or
typed facts. Construct `CheckedAST` only after each required proof succeeds;
keep match validation ahead of time. Delete source-driven
`ParsedProgram`/`ParsedType` conversion and, once no source checker needs it,
the whole-program semantic AST intermediary. Preserve a separate path for
compiler-generated semantic programs if still required.

**Done when:** a successful source compilation flows from validated
`WrittenTypes` to `CheckedAST` without constructing `ParsedProgram` or a
second whole-program source AST. No parser recovery or unresolved identity can
reach lowering.

## Stage 6: Clean up and verify the completed boundary

Remove dead adapters, duplicate validation, obsolete types, and stale
documentation. Review the final source diff for one-way ownership of parsing,
source validation, name resolution, and checked construction. Compare
diagnostics, formatter round trips, and observable language behavior against
the Stage 0 baseline. Record intentional differences in the relevant
compatibility ledger.

For every stage that changes compiler behavior, create a failing focused E2E
test first. Before committing a code stage, run `./build --ai`,
`./run-tests --ai`, and
`./benchmarks/run_benchmarks.sh --verify-parent full` on its task branch.
Each stage is independently reviewable and lands before the next begins; do
not carry temporary dual parsers or duplicate source representations into the
final stage.
