# Compiler-selected list arrays

The compiler has an initial closed-region implementation of immutable
`List<Int64>` computations using mutable array storage. No source syntax,
ownership annotation, public list type, or external calling convention changes.

## Implemented boundary

`passes/hir/ExtractListRegions.fs` recognizes structured regions beginning with a list literal or
a supported list operation after monomorphization and lambda lifting, before
AST-to-ANF lowering destroys collection semantics. Supported operations are
`List.map<Int64, Int64>`, `List.reverse<Int64>`, and
`List.fold<Int64, Int64>`, plus runtime-sized construction with
`List.repeatUnsafe<Int64>(count: Int, value: Int64)`. Literal lengths are no
longer restricted to the small allocator's 28-element limit. Scalar bindings
and region results have type `Int64` or `Bool`; the repeat count is a
constructor-specific managed `Int` operand, not a general managed region value.
Regions can contain nested `if` expressions with immediate scalar conditions
and `Int64`/`Bool` results, including scalar bindings used after a join. Each
branch may construct and consume local lists, or use lists from its enclosing
scope. Lists cannot themselves be branch results. Result/Option payloads and
pattern matching remain outside this grammar, including checked `List.repeat`.

Callbacks must be known closure constructions or function references. Captures
are restricted to immediate scalar values and static code addresses. External
list parameters, escaping lists, unknown callback values, unsupported
operations and managed elements/captures retain the existing
persistent skew-list implementation. This is a supported representation
choice, not a conversion shim. There are no array/skew conversions.

The initial selection rule is an eligibility rule, not an interprocedural cost
model. Operations on literal arrays of up to 28 elements are unrolled; larger
and runtime-sized arrays use shared tail-recursive kernels in
`stdlib/__ListArray.dark`, compiled into loops.
Literal initialization still emits work proportional to the source literal.
Literal lengths must fit the existing signed 32-bit layout offsets. Runtime
lengths use checked 64-bit byte arithmetic. Runtime lengths through 28 use the
256-byte recyclable class; larger lengths use independent mappings. Growing
builders and pooled large buffers are not implemented.

## Typed stages and ownership

The region IR has three stage-specific program types.

Their compact shared model lives in `ir/hir/ListRegion.fs`. Constructors are
internal to the compiler; extraction, storage selection, ownership solving,
independent verification, budget analysis, and ANF lowering have separate modules.

```text
FunctionalRegion: typed blocks + semantic collection edges + scalar joins
  -> StorageRegion: explicit array layouts
  -> OwnedRegion: consume-or-copy transformations + explicit releases
  -> existing ANF primitives -> MIR -> existing native backends
```

`ir/hir/HIR.fs` defines shared typed value identities, operands, scalar
bindings, resolved direct calls, branches, and blocks independent of array
layouts and ANF.
Opaque scalar expressions retain their checked AST evaluation payload while
their local inputs use normalized identities. `passes/hir/VerifyHIR.fs` checks
definitions, uses, types, and structured branch results independently.
`ir/owned/OwnedIR.fs` defines ownership-bearing blocks with ordered
`Evaluate`, `Dup`, and `Drop` steps plus explicit unit-transfer contracts. Value-edge
liveness and destruction proofs live separately in `analysis/`. These interfaces
are used by the list dialect, not yet a whole-program semantic IR
or a primitive effect registry. Opaque scalar expressions and callbacks retain
their original evaluation order; their types are not evidence of purity.

Each list primitive has one HIR contract. Construction produces fresh managed
storage and may allocate; map/reverse may reuse their list input and read/write
owned storage; fold produces an unmanaged scalar. Callback-bearing operations
declare user-code invocation. The verifier rejects reuse or may-alias sources
that are not typed primitive inputs, and list liveness consumes these same
contracts. Opaque operand evaluation remains ordered and conservative.

The list stages share a leaf `Operation<'transform>` family inside the common
HIR control flow. Only owned transforms carry
an ownership decision. Collection identities are monotonic and separate from
ANF temporary identifiers; lexical aliases resolve to the same collection
identity before liveness solving. Runtime extents name their originating
construction identity, not a lexical count variable; aliases, consuming
transforms and variable shadowing cannot change this origin. Lowering carries
the pointer, validated length atom and selected layout together. Scalar bindings
retain checked types.

A backwards liveness pass solves collection uses through branch joins. A final
use transfers the source allocation into the result. If another use survives,
including in the continuation after a join, the transform borrows the source
and allocates/copies independent storage. Mutually exclusive final uses may
each consume the same incoming allocation. A list needed by only one branch
is released on entry to the other branch, after condition evaluation. Every
reader's last use releases its source, and unused results are released.
This closed grammar proves physical uniqueness statically: a buffer
has one physical ownership unit, while all logical aliases are visible in the
region graph. A borrowed parameter with RC=1 is **not** evidence of uniqueness;
borrowed external lists are ineligible. The shared verifier now records this as
exclusivity provenance independent of the local unit count. Every consuming
map/reverse requires both that provenance and exactly one unit, and its result
inherits a fresh certificate. Construction and borrow-and-copy results also
establish fresh certificates.

The list ownership solver now emits each destruction as an explicit `Drop` in
the same ordered position used by native lowering. The shared verifier tracks
unit multiplicity, so `Dup` can justify repeated consuming uses and exact unit
counts must agree at branch joins and function returns. Closed list regions
remain statically unique and therefore reject `Dup`; enabling it for escaping
arrays first requires a shareable representation and corresponding lowering.

The shared ownership interface also models managed block arguments beyond the
current list extraction grammar. Mutually exclusive arms transfer their
path-local ownership units to a fresh continuation identity after their
residual ownership states agree. The continuation receives exclusivity only
when both incoming results are unique. Current list regions still return only
immediate scalars from branches; managed ANF joins and escaping list boundaries
are not enabled by this architecture change.

The same verifier accepts explicit function ownership signatures. Borrowed
parameters are readable but cannot be released or consumed; consumed
parameters enter with one ownership unit but no assumption about outside
aliases; unique parameters additionally establish exclusivity. Produced results
transfer one unit back to the caller, while unique results also transfer an
exclusivity certificate. Resolved HIR calls require matching typed, primitive,
and ownership registry entries. Unknown calls remain opaque, and recursion is
enabled only by an explicit self-entry. Current list regions intentionally
reject this general call node and use the closed signature because external
source lists still have the persistent skew-list representation. Owned
whole-function inputs and fixed-point specialization scheduling now run in the
production pipeline. Scheduling is bounded by iteration, generated-group, and
rewritten-call limits, and its cache descriptors retain exact source bodies,
canonical contracts, and dependencies. The resulting ownership HIR remains an
analysis artifact until ownership-aware ANF lowering consumes it; escaping
array values remain later work.

`verifyFunctional` checks the closed region's incoming collection interface
using representation-independent value contracts. `verifyBlockOwnership`
supplies list operation contracts to the shared `VerifyOwnership.verifyClosed`, which checks
live inputs, globally unique definitions (including between sibling branches),
balanced dup/drop units, valid uniqueness contracts, identical surviving
ownership and exclusivity states at joins, and absence of leaked region roots.
The stage verifier also checks
operand types, layout agreement, and allocation bounds. Construction is atomic
at the region level: element expressions run first in source order, then the
compiler allocates and initializes the buffer before exposing its identity.
Map callbacks execute in list order; fold callbacks execute in traversal order.
Repeat evaluates count and value once, in source order, even for nonpositive
counts. Negative arbitrary-precision counts normalize to zero before narrowing.
The checked constructor rejects counts above `(Int64.MaxValue - 40) / 8`
before conversion or multiplication. That rejection uses the stdlib fatal-error
mechanism (`Uncaught exception: Out of heap memory`); an OS allocation failure
uses the native allocator's fatal path.

`allocationBudget` reports exact region allocation counts/requested bytes, copies,
reused transformations, and releases, excluding work inside scalar expressions
and callbacks. Its tree retains the prefix, mutually exclusive branch budgets,
and shared continuation separately; it neither sums alternatives nor enumerates
every execution path. Requested bytes include the mapped allocator's private prefix,
but exclude OS page rounding. Byte budgets contain a constant term and physical
runtime-buffer counts keyed by construction identities. Each runtime buffer
costs `256` for `n <= 28`, otherwise `40 + 8*n`; copies add another buffer with
the same extent. Here `n` is the validated, nonnegative array length, not the
original signed count. Distinct runtime constructors retain distinct terms.
These are piecewise budgets, not an affine approximation. Pass tests check them and
the resulting native-memory ANF operations. `--dump-anf` exposes allocations,
stores, calls, and cleanup.

ANF has explicit lexical joins and scalar jumps. Lowering emits each shared
continuation once; the former sixteen-path eligibility limit is removed.
Collection ownership stays in the enclosing scope, with explicit edge cleanup
from ListHIR; scalar jumps do not return or transfer managed values. RC insertion
releases branch-local owners at jumps and preserves enclosing cleanup for the
continuation. The post-RC join-interface verifier checks lexical captures,
target visibility, argument types, and entry control transfers.

Moving a cleanup boundary can be observable even when an expression returns
an integer. Entry-local scalar expressions and callbacks therefore require
inert-destruction evidence. `DestructionAnalysis` describes structural destruction and
function-scope contracts; ListHIR collects local evidence and direct-call
dependencies from resolved source functions. Unproven scopes and unknown
callees reject all transitive callers; safe recursive components need no
unrolling. Registry composition retains dependencies so a replacement definition
can revoke a caller's proof. Known printing primitives may perform effects but
have inert destruction: this contract is not purity. Streams, unknown closures,
and unproven nominal/container payloads retain the persistent representation
when they could cross an entry boundary. Inlining also checks newly introduced
entry lifetimes. Enclosing owners keep their existing cleanup order, including
Streams held in containers. General destruction-effect propagation remains
outside this conservative slice.

## Storage contract

The internal layout is `[length][capacity][initialized count][Int64 elements][RC]`,
with 8-byte words. Capacity equals the length except for small runtime buffers,
whose capacity is 28. Allocation size is `32 + 8 * capacity`, including the
refcount word. Empty literals use 32 bytes; empty runtime buffers use the same
256-byte recyclable class as other small runtime buffers. There is no special
null-array representation.

Storage selection produces `RecycledArray`, `MappedArray`, or `RuntimeArray`. Statically
sized allocations through 256 bytes use the existing allocator's recyclable
size classes on both native backends. Their cleanup uses the fixed-block release
plan with no child destructors. Runtime allocation and release dispatch on
the validated length. The small branch has RC at offset 248 and uses the same
fixed-block release plan, exposed through an internal array-release intrinsic;
it never disguises the buffer as a source-level managed value. A shared release
helper keeps conditional cleanup out of initial region continuations.
Larger arrays own an independent mapping and explicitly unmap it at their verified final release.
The common array header and RC word remain
uniform; mapped-region lifetime is controlled by the ownership plan, not RC.
The compiler never tags array storage as a source-level list, reinterprets it
as a Blob/String, or uses the 8-byte-only `RawFree` primitive to reclaim it.

`MappedAlloc` and `MappedFree` remain distinct effects through ANF, MIR, and
LIR. The allocator uses `mmap`/`munmap`, with an 8-byte private mapping-length
prefix before the returned word-aligned pointer. Size checks reject negatives
and prefix-addition overflow before entering the kernel. Zero requested bytes
still produce a releasable allocation. Syscall failure uses the existing fatal
allocation-error path. Syscall operands and live caller registers are protected
by ordinary LIR caller-save boundaries; an allocation result is moved out of
the result register only after restoration. Leak accounting counts mappings
only after successful allocation and decrements only after successful release.

These mappings favor simple, auditable reclamation over a pooled large-buffer
allocator: every new large physical buffer incurs a mapping syscall, and every
release incurs an unmapping syscall. Consumed transformations reuse the mapping
without either syscall. Copying preserves the source and allocates one new
mapping. Native wall-time evidence is therefore required alongside instruction
counts when evaluating these workloads. Growing, pooled, and escaped buffers
remain separate future work.

The internal allocator test seeds an exact-size block, runs the list pipeline,
reacquires a block, and observes transformed contents. It also checks leak
accounting. Inlining is disabled for that probe to preserve the seed function's
release boundary. Ordinary semantic tests run with default optimizations.

Repeated native execution also validates the instrumentation itself. Both x86
allocation paths (raw and fixed-block) count a recycled block as live again.
ELF leak counters are placed on a separate 64 KiB boundary, shared by relocation
and image construction, so their writes cannot repeatedly invalidate hot code
pages under QEMU. This padding is confined to leak-check builds; ordinary
binaries retain their previous layout.

## Further architecture work

This is the first end-to-end region slice, not a replacement for the entire
ANF pipeline or a complete Perceus implementation.

Owned HIR can refine an established borrow/consume function boundary into a
bounded set of verifier-proven uniqueness variants. It upgrades only consumed
parameters and produced results, rejects variants that fail ownership
verification, and removes a variant only when another requires no stronger
inputs while promising no weaker result. Keeping incomparable boundaries makes
the later specialization policy explicit. Mutually visible and recursive
functions are inferred as one bounded candidate group, with internal call
contracts derived from that same candidate before group verification.
Program-level inference discovers those groups in callee-first order, delegates
acyclic singletons and recursive SCCs to their respective solvers, and retains
each group's dependency metadata and every nondominated candidate. Cross-group
calls continue to use their established ownership registry contract during
inference. Call-site selection indexes every group member, accepts explicit
argument-uniqueness facts, and chooses a candidate only when its ordinary
borrow/consume/produce shape matches that established contract. It prefers a
unique result and then fewer unique-input requirements. Candidate identities
are structural across the complete group and independent of discovery order
and local ownership identities, so recursive SCC selection remains atomic.
When no inferred candidate applies, the established contract remains
the fallback.

Variant materialization now returns a verified plan with rewritten original
functions, deduplicated specialized groups, and explicit call rewrites. Each
clone receives a deterministic symbol derived from the complete candidate
identity. A recursive group is cloned atomically; normalized calls within that
group use the corresponding cloned symbols and ownership contracts. Selected
external calls are rewritten, while established requests and calls outside a
cloned group keep their existing targets. Different complete candidates may
coexist, but selecting individual recursive edges is rejected. Missing members,
changed boundaries or group membership, stale call requests, and symbol
collisions also fail explicitly. The resulting program is checked again for
typed value flow and ownership, including the actual caller's uniqueness.
Effect and alias contracts remain independent and are forwarded from source
targets. Ownership analysis now exposes verified, pre-transfer uniqueness facts
for normalized call sites, accounting for duplication, aliases, escapes, and
branches. A contract-level test feeds those facts through selection and
materialization and rechecks the specialized boundary. Whole-function
ownership analysis is now scheduled before ordinary ANF lowering: real checked
functions receive fixed-point borrow/consume boundaries,
ownership-transferring managed results, explicit duplication and cleanup, and
joint HIR/ownership verification. The artifact is intentionally discarded until
selection and materialization are scheduled across functions, so no runtime or
storage-selection change is enabled by this foundation alone.

The [in-place mutation checklist](../../project/perceus-checklist.md) tracks
the complete implementation sequence. The remaining architecture boundaries
include:

1. Further runtime-sized constructors (including Result-wrapped `List.repeat`),
   builders, a growth policy, and profitable pooling for large runtime buffers.
   Checked repeat construction, independent variable-byte mappings and
   loop-based kernels provide the reclamation/execution foundation.
2. Extend the typed block/value-contract foundation into a general semantic HIR
   with primitive effect/alias/ownership contracts and general block interfaces;
   layout/destruction metadata independent of ANF; stage verifiers throughout
   the pipeline. Generated result printing now precedes ANF ownership
   elaboration; whole-function HIR must retain that established boundary.
3. Carry the registered HIR call boundary through whole-function construction,
   then add representation interfaces, bounded specialization, explicit
   conversion profitability, specialization scheduling, and cache integration.
4. Runtime uniqueness tests for consumed arrays whose sharing is not statically
   known; surviving borrowed aliases must remain protected.
5. Managed elements and destruction-effect propagation. Stream finalizers are
   observable, including through containers, so general last-use release cannot
   move them arbitrarily. This slice excludes managed elements and captures.
6. Drop specialization, constructor reset/reuse tokens, safe child-edge
   dismantling, and later suitable tail-recursion-modulo-constructor lowering.

Roc's ownership solver/emitter/certifier separation and array uniqueness
mechanics inform these later stages. Koka's Perceus implementation supplies the
model for precise RC and constructor reuse. Representation selection between
Dark's skew lists and arrays remains a separate compiler analysis. Opportunistic
reuse does not constitute a general fully-in-place guarantee.

Validation commands and integration requirements remain owned by
[verification.md](../../contributing/verification.md). Collection-specific diagnostics are
documented in [the targeted benchmark guide](../../../benchmarks/targeted/README.md).
