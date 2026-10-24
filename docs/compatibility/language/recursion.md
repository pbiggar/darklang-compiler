# Recursion parity

Polymorphic-recursion correction: 2026-10-22, against interpreter
`e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`. The earlier claim that the
interpreter rejects type-changing recursive calls was incorrect. Same-source
probes during that investigation accepted concrete resets, parameter swaps,
mutual calls, and terminating constructor-growing calls. The native compiler
supports finite specialization; unbounded specialization is intentionally excluded
from the compiler's language model.

The rest of the source review dates from 2026-10-07 at
`7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`.
See the [current audit](../current-audit.md) for post-port status and validation.
Older revision pairs and executed counts below are historical evidence, not
a fresh test result for this revision.

This matrix was revalidated with compiler revision
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and `darklang/dark`
interpreter revision `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
Implementation started from compiler HEAD
`a78567efd773de86265e55a54445ddf5a5a8911c`. DCB1 report commit
`8a402797` was starting evidence only; no result below is retained from it
without a current source probe.

The focused executable corpus is
`test/fixtures/e2e/interpreter/recursion_parity.e2e`. Local-declaration cases are
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
| Eager value cycle | rejected because an ordinary RHS cannot see its binder | same for expressible local values | local RHS rejection; top-level values now exist and are checked sequentially before materialization |
| Alias-only type cycle | rejected deterministically | expansion could recurse late | shared rejection; declaration validation uses an explicit visiting state |
| Nominal recursive ADT | accepted | accepted | shared; delayed nominal payload references are not alias edges |
| Recursive generic ADT | accepts one nominal identity through construction and higher-order folds | accepts with expected-type-aware constructor freshening | shared; pinned `RBTree<'k, 'v>` construction and `List.fold` cases |
| Type-changing self or mutual call | the historical rejection claim was incorrect; terminating probes accept at `e76430be` | the old blanket self-call restriction rejected finite cases and did not cover every mutual cycle | corrected below: finite native specialization is supported; unbounded specialization is intentionally excluded |
| Completed generic function used at two types | accepts | accepts | shared; generalization occurs outside the active recursive group |
| Deep tail recursion | eventually exhausts the execution stack at sufficient depth | native executable completes | intentional AOT completion divergence; compiler failure where the interpreter completes remains a parity defect |

`rec` and `and` are ordinary identifiers. `let` is the only public function
declaration form. Legacy `def` is retained only in identifier and negative
compatibility fixtures.

## Representation and algorithm

`src/AST.ml` defines opaque `BindingId`, `ScopeBoundaryId`, `RecursiveGroupId`, and
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

Named generic references instantiate their declared signatures independently,
including inside recursive bodies. Argument and return checking still enforce
those signatures; changing type arguments does not permit ill-typed calls.
Completed and imported groups retain ordinary instantiation. Lambda lifting
uses binding identity to construct a fixed closure environment, rewrites self
calls to the lifted code reference, and does not capture an ordinary closure
value back into itself. The lowered recursion registry carries member identity
and group-environment index beside ANF. Tail ownership transfer compares those
binding identities; native symbol strings are presentation keys, not recursion
markers.

## Polymorphic recursion and finite specialization

This is valid and needs only `reset<Int64>` and `reset<Bool>`:

```dark
let reset<'a> (_value: a) (n: Int64) : Int64 =
  if n <= 0L then 0L else 1L + reset true (n - 1L)
reset 1L 3L
```

It returns `3L` in both implementations. Parameter permutations are also
finite: `(a, b) -> (b, a)` alternates between at most two instances. Mutual
calls, specialized function values, and explicit or inferred partial
applications follow the same rule. Native tests also return tuples and
tail-call between scalar/string instances to exercise concrete callee selection
and ownership transfer.

By contrast, this terminating execution returns `3L` in the interpreter but
cannot be expanded into a finite set of native function bodies:

```dark
let grow<'a> (x: a) (n: Int64) : Int64 =
  if n <= 0L then 0L else 1L + grow [x] (n - 1L)
grow 1L 3L
```

Every native instance references another: `Int64`, `List<Int64>`,
`List<List<Int64>>`, and so on. Before enumerating instances, the compiler
examines reachable generic bodies. Graph vertices are function/type-parameter
slots; edges carry a parameter into a callee's type argument. An edge is marked
growing when the parameter occurs beneath a type constructor. A cycle containing
a growing edge produces `Unbounded generic specialization`. This covers direct,
mutual, swapped, and first-class generic references without an arbitrary depth
limit. Unused generic definitions do not require native bodies.

Not every constructor is a problem. `(a, b) -> (List<b>, Int64)` settles after
two transitions because the growing flow `b -> a` has no path back. A constant
reset contributes no parameter-flow edge. Rejecting every changed argument, or
every constructor occurrence within a recursive function group, would reject
these finite programs unnecessarily.

There are also interpreter function-value issues distinct from this native
limit. At the pinned revision, bare `f<Bool>` is treated as a Unit application
rather than a specialized function value. Explicit partial application is a
portable alternative for the finite reset probe. A constructor-growing recursive
partial probe instead produced an interpreter nested-type mismatch. Neither
issue justifies rejecting finite recursion in the compiler. The compiler
regressions live in `test/fixtures/e2e/polymorphic_recursion.e2e`; historical
interpreter observations are not claims of fresh execution on later revisions.

## Intentional compiler differences

The AOT compiler reports unresolved references, ambiguous declarations,
invalid cycles, and unbounded native specialization before execution. Unbounded
specialization is intentionally excluded from the compiler's language model,
not pending implementation work. This is a behavioral difference, not merely
earlier timing of an interpreter error: the interpreter can complete the
terminating example above. Finite polymorphic recursion remains supported.
Genuinely underdetermined
lambda and declaration types may also receive an early AOT diagnostic because
native code requires a concrete representation. These are intentional timing
or diagnostic differences when the same program is otherwise rejected.

Compiler-only internal declarations and native intrinsics can participate as
imported completed groups. They are extensions of the ahead-of-time target,
not public recursion syntax.

## Source anchors

- Parsing: `src/frontend/interpreter/Parser.ml`
- Stable parsed identities: `src/NameSyntax.ml`
- Resolution, SCCs, signature instantiation, and diagnostics:
  `src/frontend/WrittenChecking.ml`
- Generic call inference: `src/frontend/WrittenCallSupport.ml`
- Constructor-growth analysis: `src/passes/preparation/SpecializationGrowth.ml`
- Reachable native instances: `src/passes/preparation/Monomorphization.ml`
- Closure identity and lowered group layout: `src/passes/anf/AST_to_ANF.ml`
- Identity-safe self-tail ownership: `src/passes/anf/TailCallDetection.ml`
- Existing behavioral coverage: `functions.e2e`, `tailcall.e2e`,
  `tuple-recursion.e2e`, closure tests, and
  `test/compiler-passes/TailCallDetectionTests.ml`

## Historical verification

The focused parser and E2E recursion corpora pass, including the million-step
native tail-recursion completion probe. Representative self, mutual, shadowed,
generic, and recursive-closure sources were inspected with `--dump-anf`,
`--dump-mir`, and `--dump-lir`; mutual and self calls retain tail-call lowering
while shadowed calls remain ordinary closure calls.

The full suite passed `6893/6893` with `./run-tests --ai`. The canonical full
profile passed all 19 benchmarks with no count changes. Performance ratio:
**2.25x** versus the audited Rust references (`benchmarks/RESULTS.md`).
