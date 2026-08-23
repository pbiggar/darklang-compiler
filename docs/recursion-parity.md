# Recursion parity

This matrix was revalidated with compiler revision
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and `darklang/dark`
interpreter revision `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
Implementation started from compiler HEAD
`a78567efd773de86265e55a54445ddf5a5a8911c`. DCB1 report commit
`8a402797` was starting evidence only; no result below is retained from it
without a current source probe.

The focused executable corpus is
`src/Tests/e2e/interpreter/recursion_parity.e2e`. Local-declaration cases are
copied from `backend/testfiles/execution/language/nested-fns.dark` where the
syntax can remain identical. Parser-only rejection cases live beside the
binding syntax corpus, and native completion cases remain in `tailcall.e2e`.

## Matrix

| Same-source case | Interpreter `04fbe9d` | Compiler `b2e1f3d` | Current classification and evidence |
| --- | --- | --- | --- |
| Top-level self recursion | accepts; factorial is `120L` | accepts | shared; `parityFactorial` |
| Two- and three-member top-level cycles | accepts with declaration-wide visibility | accepts through blanket visibility | shared behavior; now represented as deterministic SCCs rather than one implicit global group |
| Direct named local function recursion | accepts; factorial is `120L` | rejected before this change | parity defect closed; copied `fact` probe |
| Direct `let f = fun ...` self recursion | accepts only when the body occurrence resolves to the new binder | rejected before this change | parity defect closed; `localCountdown` |
| Ordinary non-lambda value | RHS runs before binding | same | shared; ordinary `Let` remains nonrecursive |
| Annotated local function | parameter and return annotations accepted | rejected before this change | parity defect closed; annotations remain separate from inferred types |
| Annotated ordinary value | rejected by the parser | parser behavior varied | shared rejection; value annotations are not function declarations |
| Outer binding or same-named parameter | outer/parameter occurrence wins, suppressing self recursion | not modeled | parity defect closed; copied `f` and `x` probes |
| Same-named visible package function | rejects the ambiguous nested declaration | chose by spelling or failed late | shared rejection; `collidingSumDown` |
| Earlier local function | captured normally | parser boundary was ambiguous | parity defect closed; copied `double`/`quadruple` probe |
| Later local function | rejected as a forward reference | could be lost during parsing | shared rejection; `forwardEven`/`forwardOdd` |
| Cross-group and qualified reference | completed/imported functions instantiate normally | blanket source inventory | shared result; distinct completed/imported availability is retained |
| Eager value cycle | rejected because an ordinary RHS cannot see its binder | same for expressible local values | shared rejection; eager top-level values are not a compiler facility |
| Alias-only type cycle | rejected deterministically | expansion could recurse late | shared rejection; declaration validation uses an explicit visiting state |
| Nominal recursive ADT | accepted | accepted | shared; delayed nominal payload references are not alias edges |
| Type-changing self or mutual call | rejected; active group assumptions are monomorphic | could enter specialization late | shared rejection with an AOT-specific `Polymorphic recursion` diagnostic |
| Completed generic function used at two types | accepts | accepts | shared; generalization occurs outside the active recursive group |
| Deep tail recursion | eventually exhausts the execution stack at sufficient depth | native executable completes | intentional AOT completion divergence; compiler failure where the interpreter completes remains a parity defect |

`rec` and `and` are ordinary identifiers. `let` is the only public function
declaration form. Legacy `def` is retained only in identifier and negative
compatibility fixtures.

## Representation and algorithm

`AST.fs` defines opaque `BindingId`, `ScopeBoundaryId`, `RecursiveGroupId`, and
`RecursiveMemberId` values. Parsed, resolved, typed, and lowered groups are
nonempty records. `RecursiveMemberKind`, `RecursiveAvailability`, and
`RecursiveDependencyKind` prevent ordinary bindings, self members, mutual
members, completed/imported groups, eager edges, and alias edges from being
encoded by names or magic strings.

The canonical parser preserves top-level function, named local-function, and direct
lambda-binding candidates. `NameSyntax.assignParsedRecursiveIdentities`
assigns stable structural identities and lexical boundaries. Name resolution
predeclares sibling package functions, resolves bodies, builds the resolved
call graph, and partitions it by deterministic mutual reachability. A singleton
self-loop is self-recursive, a multi-member SCC is mutual, and an acyclic SCC is
a completed generalized group. Local candidates receive singleton self
availability only after parameter, outer-binder, and package-collision rules
have selected the same new binder.

Type checking holds active recursive references to one monomorphic group
assumption and rejects a type-changing `TypeApp` before specialization.
Completed and imported groups retain ordinary instantiation. Lambda lifting
uses binding identity to construct a fixed closure environment, rewrites self
calls to the lifted code reference, and does not capture an ordinary closure
value back into itself. The lowered recursion registry carries member identity
and group-environment index beside ANF. Tail ownership transfer compares those
binding identities; native symbol strings are presentation keys, not recursion
markers.

## Intentional compiler differences

The AOT compiler reports unresolved references, ambiguous declarations,
invalid cycles, and polymorphic recursion before execution. The interpreter
may phrase or time the same rejection differently. Genuinely underdetermined
lambda and declaration types may also receive an early AOT diagnostic because
native code requires a concrete representation. These are intentional timing
or diagnostic differences, not different scope or recursion semantics.

Compiler-only internal declarations and native intrinsics can participate as
imported completed groups. They are extensions of the ahead-of-time target,
not public recursion syntax.

## Source anchors

- Parsing: `passes/1_InterpreterParser.fs`
- Stable parsed identities: `NameSyntax.fs`
- Resolution, SCCs, monomorphic checking, and diagnostics:
  `passes/1.5_TypeChecking.fs`
- Closure identity and lowered group layout: `passes/2_AST_to_ANF.fs`
- Identity-safe self-tail ownership: `passes/2.7_TailCallDetection.fs`
- Existing behavioral coverage: `functions.e2e`, `tailcall.e2e`,
  `tuple-recursion.e2e`, closure tests, and
  `compiler-passes/TailCallDetectionTests.fs`

## Verification

The focused parser and E2E recursion corpora pass, including the million-step
native tail-recursion completion probe. Representative self, mutual, shadowed,
generic, and recursive-closure sources were inspected with `--dump-anf`,
`--dump-mir`, and `--dump-lir`; mutual and self calls retain tail-call lowering
while shadowed calls remain ordinary closure calls.

The full suite passed `6893/6893` with `./run-tests --ai`. The canonical routine
profile passed all 19 benchmarks with no count changes. Performance ratio:
**2.25x** versus the audited Rust references (`benchmarks/RESULTS.md`).
