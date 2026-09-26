# Interpreter parser migration

Replace the compiler's source parser with the Darklang interpreter parser. The
target source path is:

```text
source → interpreter parser → WrittenTypes → compiler checking → CheckedAST
```

There is no `WrittenTypes` → `ParsedProgram` adapter in this plan, and source
checking does not construct the current semantic `AST.Program` as an
intermediate. Compiler-generated semantic programs may retain their own entry
path. This plan concerns source compilation, not a rewrite of those generators.

The parser should work the same way as the current interpreter checkout. Copy
its implementation and keep its parser algorithms, mutable state, error
recovery, and diagnostics. Only make integration changes required to compile
and call it in this repository; do not refactor the copied parser to match the
compiler's F# style. Record the copied revision, source files, and license
notices. The user selected the current local darklang/dark checkout, revision
`1cc4bb7f63acdf29dc66458f3c401ed91d444775`. This is newer than the
compiler's documented `v0.0.35` compatibility baseline and changes grammar,
including `^` and `**`. Treat resulting differences through the decision rule
below.

## Starting point on current main

The recent [parsed and checked AST boundary](parsed-checked-ast-plan.md) gave
source expressions and `ParsedProgram` their own types. Parsing and
`NameSyntax.normalizeSource` produce that source-only tree. The checker then
converts it to semantic `AST.Program`, resolves and checks it, and constructs
`CheckedAST.Program`. The migration replaces the source side of this chain;
the checked representation and its required proofs remain the destination.

## Decision rule for every stage

Changing the parser will break some tests and may expose behavior differences.
Do not quietly update test expectations, reinterpret upstream syntax, change
compiler behavior, or add a compatibility workaround. For each distinct
failure or design conflict, report a small reproducible source example, the
current compiler behavior, the selected interpreter behavior, affected tests
or source files, and viable choices. Ask the user which behavior to keep before
making the dependent change. Group equivalent failures so the user can make
one decision per issue, then record the decision and its resulting tests in the
appropriate compatibility documentation. Continue independent work while a
decision is pending.

## Stage 1: Copy and run the interpreter parser

Copy the selected revision's tokenizer, lexer, parser, `WrittenTypes`, and
validation code with provenance. Add the minimum project and dependency glue
needed to call its normal parse entry point. Keep parser behavior unchanged.
Run it against representative compiler source files without switching the
production compiler. Collect parse successes, diagnostics, and missing
dependencies. Bring any required parser behavior change or substantial
dependency decision to the user using the decision rule above.

**Exit:** the copied parser runs in this repository and returns the selected
interpreter's `WrittenTypes` and diagnostics for the same input.

## Stage 2: Check `WrittenTypes` directly

Build a separate source-checking entry point that accepts successful,
validated `WrittenTypes` and produces `CheckedAST.Program` and the type
environment. Move the existing compiler work into this path in small slices:

1. Enumerate declarations and source units, including module and entry
   information needed for a compile request.
2. Resolve written type and value names against the compiler's declaration
   catalog and available dependencies.
3. Infer and check expressions, calls, patterns, matches, and declarations.
4. Construct checked identities, types, records, recursion evidence, and
   checked expressions as each proof succeeds.

Reuse checking rules and data structures where they fit. Do not route source
through `NameSyntax.ParsedSource`, `AST.ParsedProgram`, or semantic
`AST.Program`; local environments and resolved or typed facts are fine. Keep
compiler-generated program checking separate if it still uses semantic AST.
Invalid matches and other invalid source must fail during compilation.

**Exit:** the new entry point can compile representative source directly from
`WrittenTypes` to `CheckedAST`, with no whole-program AST bridge.

## Stage 3: Compare and decide differences

Run the new path alongside the current source path over syntax and E2E tests,
stdlib and preamble sources, package inputs, and CLI examples. Compare parser
diagnostics, compilation outcomes, and observable program results. Build a
decision list for each distinct mismatch and ask the user before altering
behavior or expectations. Add a focused failing E2E test before fixing a
compiler behavior, including `compileerror=` where the diagnostic phase
matters. Re-run affected cases after each decision.

**Exit:** every known mismatch has a user decision and a test or documented
reason for its accepted result. No failing test is hidden by changing only its
expected output.

## Stage 4: Switch production source compilation

Route source inputs through the copied parser and direct checker path. Cover
CLI compilation, source-unit composition, stdlib and preambles, package
sources, and test tooling. Remove the old production lexer/parser and the
source-only `ParsedProgram` path once all callers have moved. Remove
`NameSyntax.ParsedSource` and source-driven semantic `AST.Program` conversion
when unused; retain independent lexical utilities and generated-program paths
only where they still have callers. Update pipeline and compatibility docs to
describe the actual boundaries.

**Exit:** production source uses the copied parser and reaches `CheckedAST`
without `ParsedProgram` or semantic `AST.Program` in the source path.

## Stage 5: Verify the completed migration

Review the final diff and repeat the accepted behavior comparisons. Run
`./build --ai`, `./run-tests --ai`, and
`./benchmarks/run_benchmarks.sh --verify-parent full` for the applicable host
target before landing code changes. If a gate fails, investigate and apply the
decision rule to behavior differences; do not treat a changed expectation as
the fix. Remove temporary comparison code and stale documentation.

**Exit:** relevant tests and benchmarks pass, all decisions are recorded, and
the checked source path has no duplicate parser or whole-program AST bridge.
