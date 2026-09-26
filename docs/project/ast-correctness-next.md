# AST correctness follow-up

This is a handoff for the seven AST proposals inspired by Darklang's
`WrittenTypes`/`ProgramTypes` separation. The original assessment describes
local main at `852119d2a2` (2026-09-26); the #6 update includes subsequent
work on checked signatures. Before starting later work, check the current code
and the canonical compiler documentation linked from [the index](../index.md). The
goal is to retain useful phase and layout guarantees, not to make our AST
resemble Darklang's for its own sake.

## Status of the original seven proposals

| # | Proposal | Current status and remaining payoff |
|---|---|---|
| 1 | Use a distinct checked AST | **Done.** Checking produces [`CheckedAST.Program`](../../src/DarkCompiler/CheckedAST.fs), which preparation and ANF consume. Do not start another wholesale checked-AST split. |
| 2 | Store constructor fields directly instead of an optional tuple payload | **Done.** Constructors, patterns, and variant declarations carry field lists; the old `TEnumFields` representation is gone. Checked constructor arity is still an unrestricted list, but checking validates it. A type-level arity proof is optional, not currently a demonstrated fix. |
| 3 | Use semantic identities in place of resolved strings | **Mostly done.** Bindings, functions, nominal references, constructors, and fields have distinct IDs. The latest change uses constructor owner/name/tag to do keyed variant lookup instead of scanning by tag. `AST.SemanticType.TRecord` and `TSum` still contain canonical strings, and some registries remain name-keyed. A complete nominal-type-ID migration may clarify those paths but has no established speed or memory benefit; do not replace the canonical-string-backed `FunctionId` merely to make it an integer. See [compiler identities](../compiler/identities.md). |
| 4 | Make checked record literals layout-complete | **Done.** [`CheckedAST.RecordFields`](../../src/DarkCompiler/CheckedAST.fs) has a private constructor; conversion checks owner, slot bounds, uniqueness, and completeness. Lowering retains source evaluation order, then orders computed atoms for layout. Record **updates** are deliberately different: duplicate updates are allowed and last-wins, so do not impose unique-field semantics on them. |
| 5 | Encode collection cardinality | **Partly done.** Calls, lambda parameters, and pattern alternatives were already nonempty. Checked tuple *expressions* now have first/second/rest, and checked matches have a nonempty case list. Checked `PTuple` patterns and `AST.SemanticType.TTuple` remain list-backed because compiler-internal payload/storage layouts can use zero or one element. See the specific remaining choice below. |
| 6 | Separate source types from compiler/internal types | **Partly done.** `AST.ParsedType` and `AST.SemanticType` are distinct. Checked callable signatures, value definitions, dictionary literal types, explicit call and record-reference type arguments, and recursive member types use a private `CheckedType` that excludes live call-local inference identities. Type definitions, checker metadata, and downstream IRs still carry semantic types; see below. |
| 7 | Keep compiler-generated expressions out of parsed syntax | **Open on main.** `AST.ParsedExpr` is an alias for `ExprNode<ParsedType>`, and that generic union includes `IndirectApply`, `Closure`, `RuntimeError`, and `BoundaryRender`. The parser does not ordinarily emit these, but the type permits them. A distinct parsed expression tree and a preparation-only expression tree could make the boundary structural. This is primarily a correctness/clarity change; do not promise a compiler-speed win without measuring it. |

## What “separate the types” would actually mean

The first split already exists: [`ParsedType`](../../src/DarkCompiler/AST.fs)
represents type spellings, while `SemanticType` represents resolved types. That
prevents a number of semantic-only cases from appearing in ordinary parsed
annotations. But `SemanticType` is also carried through
[`CheckedAST`](../../src/DarkCompiler/CheckedAST.fs),
[`ANF`](../../src/DarkCompiler/ir/anf/ANF.fs), and
[`MIR`](../../src/DarkCompiler/ir/mir/MIR.fs). Thus the same union currently
serves several different questions:

1. What type did the programmer write or infer? For example `TRecord`, `TSum`,
   generic `TVar`, and function/collection types.
2. What temporary fact does checking need? `TInferenceVar` is a call-local
   unification identity. [`CheckedAST.normalizeInferenceType`](../../src/DarkCompiler/CheckedAST.fs)
   currently erases it when crossing into checked syntax, but the checked tree
   still stores `SemanticType`, whose union permits it.
3. Does an expression return? `TNever` is a semantic bottom type used when
   checking expressions such as runtime failure. It is not a machine value
   type, and should not be grouped casually with raw pointers.
4. How is a value represented or passed by the backend? `TInternalRawPtr` is
   used for privileged compiler/runtime signatures; MIR also consults
   `SemanticType` to select floating-point operations, call conventions, and
   typed memory operations.

These are related facts, but not identical *type universes*. A useful split
would make at least one currently implicit guarantee explicit. The strongest
candidate is a checked type that cannot contain a live `TInferenceVar` and
cannot contain a privileged raw pointer except through an explicit internal
capability/signature boundary. A separate backend representation or ABI class
could then describe machine operations without pretending that a nominal
`Result<Int64, String>` is merely “a pointer.” Keep nominal identity and type
arguments available wherever descriptors, generic specialization, ownership,
or diagnostics require them.

Do **not** replace `SemanticType` everywhere with a tiny machine-type union in
one sweep. ANF/MIR still use nominal and instantiated types for more than
instruction selection. Conversely, do not just rename `SemanticType` or add
wrappers around every use: that would add traversals and match boilerplate
without excluding a real invalid state. `TNever` needs an explicit policy at
the checked-to-lowered boundary; a nonreturning expression need not be assigned
a fictitious runtime value. `TTuple []` and `TTuple [_]` also cannot be banned
globally because internal storage layouts use them.

A sensible first implementation slice would be:

1. Inventory every construction and consumption of `TInferenceVar`, `TNever`,
   and `TInternalRawPtr`, plus each public parsed-type entry point. Establish
   which are checking facts, privileged signatures, and real runtime values.
2. Pick one boundary and name its invariant before designing a new union. For
   example, a checked-type constructor could certify “no inference identity”
   while a separate internal-signature type handles raw pointers. Decide how
   nominal metadata and generic variables cross that boundary.
3. Migrate one producer and its consumers completely, removing the superseded
   representation and impossible branches. Keep errors at the ahead-of-time
   checking boundary, and use `Crash.crash` only for a genuinely impossible
   undocumented internal state.
4. Validate observable cases: public source cannot name a privileged raw
   pointer; privileged helpers still compile; generic inference and
   specialization preserve type arguments; nonreturning match arms remain
   checked ahead of time; HAMT/dictionary operations and backend ABI behavior
   remain unchanged. Run `./build --ai`, `./run-tests --ai`, and the required
   `./benchmarks/run_benchmarks.sh --verify-parent full` on the task branch.

Checked callable signatures, value definitions, dictionary literals, recursive
members, and generic type arguments now use the certified type. Their producers,
generated-function paths, alias resolution, specialization, and HIR/ANF
consumers preserve nominal type arguments,
`TNever`, and privileged `TInternalRawPtr` where those remain meaningful. The
next type-design step is to decide whether checked type definitions need a
separate certified declaration form, and to choose an invariant for backend
representation/ABI classes. Constructor lookup types are checker metadata and
should be evaluated at that boundary rather than counted as checked syntax.

Expected payoff: **moderate to strong** clarity and phase safety if the split
removes actual impossible states. Speed and memory payoff are **unproven**;
extra conversions could make either worse. If the inventory finds only a few
privileged cases already fenced by checking, keep the present parsed/semantic
split and use smaller boundary-specific types instead of a new global IR.

## Other remaining correctness choices

- **Prepared-expression boundary (#7).** A genuinely distinct `ParsedExpr`
  should exclude compiler-generated variants; a prepared tree can add the
  closure/indirect-call/rendering forms only after checking. Determine whether
  the checker needs any generated forms internally before choosing the
  conversion point. The goal is an exhaustive phase transition, not a second
  tree that still accepts every old case.
- **Recursive binding shape.** `CheckedAST.RecursiveLet` still accepts any
  `Expr` as its value. A lambda-specific checked value would make the
  recursive-lambda invariant structural. This was identified in the later
  correctness review but was not one of the original seven proposals.
- **Tuple-pattern cardinality (#5).** Do not merely change checked `PTuple` to
  `TwoOrMore`. Pattern lowering can construct an empty tuple pattern for the
  payload slot of a nullary constructor when other cases of the same sum have
  payloads. A clean change would separate source tuple patterns from internal
  payload patterns first; it must also preserve the valid zero-/one-element
  *storage* tuple types.
- **Nominal IDs and constructor arity (#2/#3).** Tackle a specific repeated
  lookup or lost checked fact, with a focused correctness or performance case.
  The remaining strings and field lists alone do not establish a worthwhile
  blanket migration.

## Changes deliberately not kept in the record/cardinality/identity task

- Struct-backed proof wrappers for checked records and tuples were tried and
  reverted after a method-signature failure. The landed wrappers remain
  ordinary immutable F# types. Do not count a memory saving from that attempt.
- Checked tuple-pattern cardinality was tried and reverted because of the
  internal empty-payload case above. Only tuple **expressions** gained the
  two-or-more representation.
- `SemanticType.TTuple` was intentionally left list-backed; global
  two-or-more cardinality would reject valid internal layouts.
- A blanket replacement of nominal strings throughout `SemanticType`, and a
  separate parsed/prepared expression-tree migration, were not attempted in
  that task.

The task's full suite passed 10,150/10,150 tests and the parent-relative
benchmark ratio was 1.000000. Test-run wall times varied around the original
44.90-second baseline (final timed run 44.92 seconds), so none of the remaining
proposals should assume a speedup from the completed invariant work.
