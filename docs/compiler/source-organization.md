# Compiler source organization

Source files are named for responsibilities, not execution positions. F# project
compilation order expresses dependencies; typed driver functions express pass
execution order. Diagnostic stage names remain stable when passes are inserted.

## Boundaries

- `frontend/`: parsing, resolution, checking, and generated source helpers.
- `ir/`: representation definitions and their local operations.
- `memory/`: representation layouts, destruction shapes, and release plans,
  independent of ANF.
- `analysis/`: semantic and cross-function analysis.
- `passes/`: transformations grouped by the representation they operate on.
- `backend/`: target instruction selection, runtime instruction generators,
  resolution, encoding, and binary output.
- `driver/`: typed pipeline orchestration, compilation sessions and caches,
  stdlib preparation, and process execution.

Keep cohesive recursive definitions together. Extract types and leaf algorithms
before splitting recursive dispatchers; use narrow typed callbacks for recursive
expression handlers. Avoid generic helper collections and cross-file recursive
module dependencies. File size is a review signal, not a partitioning rule.

ANF expression lowering has typed recursion callbacks in
`passes/anf/lowering/LoweringCallbacks.fs`. `Expressions.fs` ties the recursive
entry points together; atom, ordinary-expression, and pattern handlers cannot
depend on that driver. Pattern matching retains its cohesive recursive pattern
compiler and source-order failure rendering in one larger module.

Backend `Instructions.fs` files retain exhaustive LIR dispatch. Their
`instructions/` children receive typed operands, not arbitrary instructions to
redispatch. Recursive ARM64 expansion receives an explicit lowering callback.
Target runtime generators live under each backend's `runtime/`; shared memory
plans contain no ISA instructions. The former root `Runtime.fs` was ARM64 code,
not a target-independent runtime layer.

## Finding an owner

| Change | Start here |
|---|---|
| Source type rules or diagnostics | `frontend/checking/` |
| Generic identity or closure preparation | `passes/preparation/` |
| Collection recognition and array selection | `passes/hir/`, `passes/storage/` |
| Structured semantic control flow | `ir/hir/HIR.fs` |
| Unit-ownership contracts and independent verification | `ir/owned/OwnedIR.fs`, `passes/ownership/VerifyOwnership.fs` |
| Value liveness and destruction scope proofs | `analysis/ValueLiveness.fs`, `analysis/Destruction.fs` |
| Region liveness, reuse, or verification | `passes/ownership/` |
| Existing ANF lifetime insertion | `passes/anf/ownership/` |
| Shared destruction shapes and release plans | `memory/` |
| Expression, atom, or pattern lowering | `passes/anf/lowering/` |
| ANF or MIR optimization | `passes/anf/optimization/`, `passes/mir/optimization/` |
| Liveness, spilling, coloring, phi edges | `passes/lir/allocation/` |
| Native operation expansion | `backend/<target>/instructions/` |
| Native allocation and destruction helpers | `backend/<target>/runtime/` |
| Pipeline scheduling or cache identity | `driver/` |
| IR definitions and scoped printers | `ir/<representation>/` |

Ownership and ARM64 test registries retain their existing suite entry points;
their cases are grouped under `src/Tests/compiler-passes/ownership/` and
`src/Tests/compiler-passes/arm64/`. Test selection and observable assertions are
unchanged by that grouping.

## Migration sequence

1. Remove numeric filenames, group passes, and name driver diagnostics without
   changing execution order.
2. Extract memory contracts from ANF and separate list-region and RC concerns.
3. Separate specialization, closure preparation, and expression lowering.
4. Separate backend allocation, destruction, instruction selection, and emission.
5. Separate driver, checking, register allocation, optimization, and test concerns.
6. Extend semantic HIR and ownership interfaces, with explicit verification and
   separate behavior-changing commits.

The intended general pipeline is checked AST → semantic HIR → storage IR → owned
IR → ANF → MIR → LIR → target instructions. The first three stages currently
cover only closed list regions; their generalization is implementation work,
not accomplished by moving files. Do not create empty future stages or duplicate
ownership authorities. Generated result printing already precedes ANF ownership
elaboration; general HIR must preserve that ordering. Changes to aliasing and
lifetime after elaboration require proof preservation and verification.

## Shared HIR and ownership interfaces

`HIR.Operation` owns normalized typed value definitions, scalar bindings, and
branches independently of a leaf operation dialect. `HIR.Block` retains a
single sequence after each branch; the list dialect no longer defines its own
control-flow cases. Operands carry checked AST evaluation payloads together
with explicit maps from lexical inputs to value identities. `VerifyHIR` checks
the identity, type, and alias interface separately from ownership. Leaf
primitive contracts also record allocation, failure, callback, and owned-storage
effects. List liveness derives managed definitions from the same output alias
contracts used by verification, rather than a parallel operation table. This is a used
structured-region interface, not a claim that arbitrary source programs have
been converted to HIR.

`OwnedIR` supplies the shared recursive step/block representation, ordered
`Evaluate`/`Dup`/`Drop` steps, and explicit borrow/consume/produce contracts.
Contracts retain operand multiplicity so duplicate consumes cannot disappear
into sets; duplicate definitions remain invalid.
Its block-argument contract distinguishes unmanaged values from managed
ownership identities. At a branch, each arm transfers its result identity, the
verifier compares the residual path ownership, and the continuation receives
one fresh identity.
Function signatures retain every typed parameter position: unmanaged entries
stay outside accounting, while managed entries are borrowed, consumed, or
uniquely consumed. Managed results are borrowed, produced, or uniquely
produced. A checked conversion derives the positional call ownership contract
from this boundary; borrowed results resolve to their unique borrowed parameter
index rather than carrying a callee-local identity across the call.
`OwnedIR.Function` pairs that signature with an ordered HIR definition without
copying its typed boundary. `VerifyOwnedHIR` projects out ownership-only steps,
verifies HIR types and primitive effect/alias contracts, then derives internal
call ownership from the paired definitions before checking direct and recursive
unit transfer.
Resolved direct-call nodes use a typed HIR signature registry and a separate
ownership signature registry. HIR still requires the call's ordinary primitive
effect and alias contract; an ownership signature cannot supply either fact.
Borrowed call results identify their borrowed source parameter, produced
results introduce a fresh ownership unit, unique modes carry an exclusivity
certificate across the call boundary, and recursive targets require an
explicit registry entry. Unknown calls stay opaque.
`VerifyOwnership.verifyFunction` checks the boundary together with scalar
accesses, leaf uses, explicit unit counts, fresh definitions, exact join
agreement, exclusivity provenance, and final ownership balance. Unique use
requires both provenance and exactly one local unit. Duplication retains
provenance but suspends uniqueness until a balancing drop; a scalar escape
revokes provenance. Duplication of a borrowed identity creates an owned unit
that may be consumed, dropped, or returned, but does not establish uniqueness;
dropping a borrow without such a unit is invalid. `verifyClosed` supplies the
empty managed boundary used by current list regions while explicitly covering
their unmanaged scalar parameters. Dialects must provide scalar-use and
scalar-escape accounting explicitly.
List extraction proves its opaque scalars cannot reference canonical list
identities; its adapter still derives managed uses from explicit operand
inputs, so a future dialect cannot inherit the extraction proof by default.

The old `SemanticIR` container is removed: HIR owns control-flow data,
`ValueLiveness` owns backward edge transfer, and `DestructionAnalysis` owns
inert-destruction proofs. Storage layouts and consume-or-copy selection remain
in the list dialect. Region ownership accounting has one verifier, with list
layout/type verification layered around it.

`passes/hir/ConstructFunctions.fs` extends the normalized value interface across
checked functions: it constructs ordered function entries and normalizes
lexical `let`, sequence, and conditional structure. Native immediate literals
and scalar operators become identity-preserving primitive leaves with explicit
effect and alias contracts; integer division and modulo may fail, while their
immediate results cannot alias managed storage. Managed, structural,
arbitrary-precision, and call-lowered operations remain conservative opaque
scalar bindings. Resolved ordinary calls are
normalized only when the group-derived or external typed signature and a
separate effect/alias contract are available; arguments retain left-to-right
evaluation order. It is not scheduled in code generation yet. The next
normalization work must infer and carry unique boundary modes through loops and
escaping boundaries. The verifier does not insert runtime uniqueness tests or
model constructor reset tokens. General managed arguments still need lowering
through ANF and RC insertion. ANF lifetime insertion remains authoritative
outside list regions. Generated result printing is an explicit consuming effect
before that boundary; ownership insertion finalizes unrelated cleanup before
the effect and instruction lowering emits the printed root's shape-specific
release. No empty
future passes or compatibility IR conversions are added.

Each refactoring chunk preserves algorithms, evaluation order, and emitted-code
behavior. Semantic migrations require focused failing E2E coverage before their
implementation. Verification requirements remain in
[verification.md](../contributing/verification.md).
