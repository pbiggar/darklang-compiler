# Compiler identities

The compiler uses IDs for two related but different jobs:

- **semantic identity** says which declaration or binding a reference denotes;
- **lowering identity** gives a temporary value, block, or register a stable key
  while one compiler stage transforms it.

The important property is not that every ID is an integer. It is that passes
compare the identity chosen by the owning phase rather than repeating name
resolution or treating a display name as proof that two entities are the same.

## Semantic identities

Name resolution is the boundary between source spellings and semantic
identity. Before that boundary, `NameResolution.SymbolIdentity` distinguishes
locals, module/package/builtin values and functions, constructors, and types.
It deliberately retains names and namespace information: candidate selection,
ambiguity reporting, and diagnostics are still source-facing operations.

Successful checking constructs a `CheckedAST.Program` with a private `Symbols`
table. The table owns the mapping between the IDs below and the names retained
for diagnostics, generated symbols, and boundaries that still consume names.

| ID | What it identifies | Why it exists | Lifetime |
| --- | --- | --- | --- |
| `BindingId` | One lexical binder: a parameter, `let`/pattern binder, recursive local, or top-level value | Equal spelling does not imply equal binding. Shadowing, free-variable analysis, closure capture, substitution, and lowering must follow the selected declaration. | Assigned around the parsed/checked boundary and consumed when checked locals become HIR values or ANF temporaries. |
| `FunctionId` | One canonical direct function target | Calls, function references, closures, call graphs, optimization registries, reachability, MIR/LIR calls, and backend symbol lookup must agree on the target without re-resolving its spelling. | Carried from `CheckedAST` through HIR/ANF, MIR, LIR, and code generation. |
| `TypeId` | One nominal record, sum, or alias declaration | A checked constructor or record reference needs a declaration identity distinct from the spelling originally used, especially through aliases and imports. | Stored on checked type definitions, record references, and constructor references; current lowering translates it back through type metadata before ANF. |
| `ConstructorId` | One case of one sum type | Case names are not globally unique. The ID prevents `A.Error` and `B.Error` from being confused and carries the already-validated runtime tag. | Stored in checked constructor expressions and patterns; lowered to the runtime tag and layout. |
| `FieldId` | One field of one record type | Field names such as `name` or `value` are not globally unique. The ID records the declaring field selected by type checking and carries its physical field index. | Stored in checked record construction, update, and access; lowered to the layout index. |
| `ScopeBoundaryId` | A lexical boundary containing recursive candidates | Same-named recursive candidates in nested scopes must not be grouped together merely because their spelling matches. | Assigned after parsing and used while resolving recursive declarations. |
| `RecursiveMemberId` | One declaration eligible for recursive treatment | A member needs stable identity before the compiler knows whether it is ordinary, self-recursive, mutually recursive, or already completed. It also gives a singleton local-recursion group a collision-free seed. | Assigned after parsing and carried in parsed, resolved, typed, and lowered recursion evidence. |
| `RecursiveGroupId` | One resolved strongly connected component, or one singleton local recursive scope | Type checking and ownership/tail-call work need to discuss a recursive group directly rather than reconstructing it from member names. | Assigned during declaration resolution and carried with typed/lowered recursion metadata. |

These types are distinct even when their representation contains integers. An
integer for a field slot must not accidentally be usable as a binding or
constructor identity.

### Name-backed and allocated IDs

The semantic IDs do not all have the same allocation model.

`BindingId`, `ConstructorId`, and `FieldId` are allocated inside a checked
program's symbol namespace. `CheckedAST.importTopLevels` remaps them when
combining independently built checked programs. `ConstructorId` additionally
carries the runtime case tag, and `FieldId` carries the runtime field index;
their separately allocated identity component prevents equal layout numbers
from making declarations equal. The recursion IDs are deterministic structural
IDs assigned to one parsed program and travel with that program's recursion
evidence; imported recursion metadata remaps its associated `BindingId`.

`FunctionId` and `TypeId` have two private forms: an ordinal form used by
explicit construction and a canonical-name form used for source programs.
The canonical-name form is intentional. Functions and types from independently
compiled stdlib, preamble, generated, and user units need to acquire the same
identity without sharing an allocator. Keeping the complete canonical name
also avoids the collisions that occurred when these IDs were derived from a
31-bit name hash. Although the representation is name-backed, downstream code
handles a `FunctionId` or `TypeId`, not an interchangeable raw string.

Names remain metadata beside these IDs. For example, every ANF/MIR/LIR
function has both `Id` and `Name`: `Id` is the key for calls and analyses;
`Name` is used for readable dumps, diagnostics, emitted symbols, and the small
set of named external conventions described below.

## Lowering identities

Lowering identities are local coordinates, not source declarations. They can
be regenerated when a transformation clones or rebuilds an IR, provided all
uses are rewritten consistently.

| ID | Scope and purpose |
| --- | --- |
| `HIR.ValueId` | A typed normalized value within a HIR function. HIR contracts, aliases, ownership steps, branch results, and list-region storage plans use it to describe data flow. `ListRegion.ListId` is an alias for the same identity because a list region tracks those HIR values. |
| `ANF.TempId` | A function-local ANF value or continuation target. Explicit evaluation order, substitutions, liveness, reference counting, inlining, and specialization use it instead of source variables. Cloning passes freshen it. |
| `ANF.ExprId` | A compact coverage-instrumentation site number used to connect emitted `CoverageHit` operations to coverage descriptions. It is currently an `int` alias rather than an opaque type. |
| `MIR.VReg` | A virtual register in the target-independent CFG. SSA and MIR optimization use it as the definition/use identity. |
| `MIR.Label` | A basic-block identity. It is a wrapper around a generated string because labels are also rendered in dumps and carried toward symbolic code generation. |
| `LIR.Reg.Virtual` / `LIR.FReg.FVirtual` | Integer and floating-point virtual registers immediately before register allocation. Allocation maps them to physical registers or stack slots. |
| `LIR.Label` and backend label references | Symbolic control-flow and data addresses retained until layout/encoding resolves them to offsets. These are intentionally symbol-like rather than source declaration identities. |

`LiteralPool` also assigns compact integer indices to distinct string values
and floating-point bit patterns. Those are deduplication/layout indices, not
semantic identities: equal literal values intentionally share an entry.

## Where names are still authoritative

The ID migration is substantial but incomplete. The remaining name use falls
into several categories.

### Source and diagnostic boundaries

The parser, `NameResolution`, and much of type checking use strings because
their job is to interpret source spellings. Original and canonical spellings
are also retained for errors. This is expected; the important rule is that a
successful reference crosses into `CheckedAST` with the selected semantic ID
where that ID model exists.

### Nominal types and type variables

`AST.Type` still represents `TRecord`, `TSum`, and `TVar` with strings. As a
result, type, alias, record-layout, sum-layout, memory-planning, JSON-planning,
and rendering registries are still commonly keyed by type name. `TypeId`
currently protects checked nominal references, but it is not yet the identity
carried by every occurrence inside the compiler's type representation. This is
the largest remaining semantic name-based area.

Type-parameter names are a separate case. They are locally bound variables
used by substitution and unification; replacing them would improve alpha-
renaming robustness, but they do not denote nominal declarations.

### Values and preparation registries

Checked lexical references are `BindingId`s, but `CheckedAST.NamedValue` and
some top-level/import materialization maps remain string-keyed until the value
is placed into a lexical scope. Several frontend and preparation registries
also retain canonical function or type names while composing source units or
generating helpers. These are bridges around independently prepared artifacts,
not a license for later passes to redo lexical lookup.

### Partially migrated downstream analyses

Most direct-call analyses now use `FunctionId`, including call graphs,
inlining, specialization, reachability, effect facts, MIR/LIR calls, and
backend call emission. Some older interfaces still key auxiliary information
by function name: recursive-member preparation, portions of ownership-variant
selection, external optimization maps, and some return-type/parameter
registries. They are migration candidates when the corresponding structures
can carry `FunctionId` without losing their cross-unit composition contract.

### Deliberately named external conventions

Some strings are not substitutes for semantic identity:

- `_start`, the generated program entry, exported symbols, and backend labels
  are ABI or object-format names;
- intrinsic and runtime-helper names select compiler-provided behavior at the
  boundary where those helpers are registered;
- dump filters and diagnostics operate on display names;
- mangled specialization/helper names are generated symbol names and cache or
  emission keys;
- record field names and constructor names retained beside their IDs are used
  for source-compatible rendering and errors;
- string and float literal-pool keys represent literal values, not declarations.

Hard-coded checks that infer semantic properties from a particular helper name
are more fragile than registration-time metadata, even when the name denotes a
real ABI symbol. Those checks should be treated separately from legitimate
symbol emission and diagnostic use.

## Practical rule for compiler changes

When a pass asks whether two things are the same declaration, binding, value,
call target, or recursion group, it should consume the corresponding ID. A
name lookup is appropriate while resolving source, crossing an explicitly
named external boundary, or recovering presentation metadata from `Symbols`.
If a downstream transform needs to inspect a name in order to rediscover
semantic facts, that usually indicates either a missing ID on its input or
missing typed metadata in a registry.
