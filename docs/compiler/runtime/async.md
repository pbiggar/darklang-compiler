# Async runtime

The target is transparent pending values with stackful native tasks. Async
operations start eagerly and return without waiting for their result. Only an
operation that needs a concrete value forces it. Pending values may pass through
calls, returns, containers and closures without changing source types. Ordinary
native calls remain ordinary calls; transitive callers are not async state
machines. Interpreter differences are not constraints on this implementation.

## Implemented promise core

`packages/darklang/stdlib/__async.dark` implements a private, typed promise object and an explicit
ready/pending value carrier. Both are embedded in the compiler, using the existing
typed raw-slot operations, sums and lifecycle owners. The private `__Task` runtime connects these
cells to native stack suspension and timer completions; ordinary source-level
operations do not yet use it implicitly.

`__Promise<a>` owns a mutable native cell and a reference-counted lifecycle owner.
Its phantom parameter fixes the cell's payload type and release plan. Each cell
has the following word layout:

| Byte offset | Contents | Ownership |
|---|---|---|
| 0 | State, pending `0` or ready `1` | Immediate |
| 8 | Ready payload | One typed owning edge, initialized only at resolution |
| 16 | Waiter task IDs in reverse registration order | One owned `List<Int64>` edge |

The payload slot's initial zero is not an `a` value and must never be read or
released while pending. In particular, no integer, float, constructor tag or
pointer bit pattern is reserved to encode pendingness inside the payload.

`__resolve` initializes the owning payload slot, detaches the waiter list, then
publishes readiness before returning the waiter IDs. A second completion is an
internal runtime error, not an ordinary I/O failure. Ordinary I/O failures belong
in the payload's normal `Result.Error` value. Completion returns waiter IDs in
registration order; the scheduler will enqueue them rather than execute them
inside resolution.

`__wait` returns false if the cell is already ready. Otherwise it registers the
task ID at most once and returns true. Registration, completion and the eventual
park transition must all occur on the single scheduler thread without an
intervening scheduler yield. This protocol is not thread-safe and is not a
multithreaded completion queue.

`__poll` inspects one promise without waiting. A ready payload is returned as an
ordinary owned `Some` value; `None` means the promise is pending. A ready payload
which is itself `None`, zero, false or an empty list is not pending. Repeated
reads do not consume the cell's payload.

The lifecycle owner releases a ready payload using its concrete type's existing
release plan and frees the cell at last reference. An unresolved cell without
waiters can also be freed. A cell with parked waiters must remain owned by their
stacks; losing its last owner with waiter IDs still registered is an invariant
failure. Started I/O must keep a producer-owned reference until completion, so
dropping a consumer reference does not cancel that I/O. Task cancellation and
run-completion policy are not implemented by dropping this object.

## Compiler integration contract

`__Value<a>` is a boxed sum with `ValueReady a`, `ValuePending (__Promise<a>)`
and `ValueDeferred (Unit -> __Value<a>)` variants. The ordinary sum release plans
own these payloads correctly, including
when carriers are returned, captured or nested in containers. `__ready` and
`__pending` construct it; `__pollValue` probes it without waiting. This separates
the discriminator from full-width scalar/float payloads while retaining ordinary
native calls. Source representation changes currently enter through the private
compiler hooks described below.

A deferred carrier follows an already-started producer when demanded; it does
not postpone initiation. `__joinCurrent` flattens a carrier containing another
carrier without forcing either at the join point. `__delayValueCurrent` and
`__startValueCurrent` use it to transport pending arguments and operation results.
The nonblocking `__pollValue` probe returns `None` for an unevaluated deferred
join, even if following that join would no longer require suspension. Forcing it
outside a task succeeds when its underlying promises are ready; an unresolved
promise still requires a task. Deferred joins retain their producer references
through ordinary closure ownership and support repeated demands.

The following demand rules guide the next compiler slice; they are not
implemented by defining these explicit runtime types alone.

`LowerAsyncDemands` provides lowering through private
`__Task.__transport` and `__Task.__delayTransparent` hooks. Their source results
have the ordinary payload type. Bindings and same-type aliases retain the
carrier; arithmetic, branch conditions and other concrete inspections insert
typed force calls before ownership elaboration. A fixed-point analysis propagates
pending parameters and results through direct calls, recursion and branch joins.
Pending-capable calls use generated function versions with typed carrier
parameters and results; concrete calls to synchronous functions retain their
existing signatures. Original source ANF bodies are retained across preamble
compilation so later callers can request these versions. Explicit native task
entry callbacks demand their result to satisfy the runtime ABI. Closure capture
slots widen independently to carriers, with matching environment types in lifted
function versions. Statically known closure calls propagate pending arguments
and results. Capturing closures with concrete callable interfaces can escape
through ordinary source functions. Local tuples and records retain independent
pending field layouts through aliases and nested storage. Local records use a
structural physical type with updated field descriptors, so ownership sees each
field's actual carrier type. Projecting one field preserves that field's carrier
and does not inspect its siblings; explicit native result adapters materialize
fields recursively and rebuild nominal records using their source descriptors.
Variants likewise retain an independent layout for each payload-bearing arm.
Logical constructor operations survive until after async inference, so tag tests
do not inherit a payload demand from an optimized nullable representation. A
changed payload layout receives a private physical sum descriptor and matching
ownership metadata; ordinary synchronous variants keep their existing layouts.
Conversions branch on the tag and adapt only the selected arm. This supports
nested variants, variants in list storage, managed payloads, and outer delayed
variants. For example, matching `Some pendingValue` with `Some _` does not wait;
adding to the selected integer payload does. A wildcard-only match on a
single-constructor type requires no value inspection at all. Explicit native
result adapters materialize only the selected payload before rebuilding its
source constructor.
Before transport inference, `AsyncCallableSpecialization` reuses known-callback
specialization for ordinary source helpers. Callback captures become ordinary
arguments, exposing their pending layouts and callback results to the existing
inference. Direct callback invocation, forwarding through direct source helper
calls, self recursion and multiple callbacks are supported without naming the
source helper in an async rule. Forwarding requests are discovered to a fixed
point, including duplicated callbacks and recursive helper chains. This preparation
runs independently of optional inlining and its size budgets. It preserves the
structured source control flow, reserves generated function identities before
runtime specialization, and leaves programs without async producers unchanged.
When a preamble already compiled a concrete callback version, later callers
receive fresh identities for the logical versions needed by transport inference.
Precompiled stdlib interfaces retain their existing adapters; unknown callback
targets and escaping changed callable interfaces remain outside this preparation.

`AsyncTransport` merges these layouts across direct signatures, typed joins and
closure captures, promoting concrete fields to ready carriers where call sites
require a common layout. Concrete calls to synchronous functions keep their
original representations. Literal lists retain their skew-tree representation
with typed element slots
widened consistently across each connected allocation component. List aliases,
ordinary calls and captures carry the element layout; length and emptiness use
library specializations for the physical element type without demanding elements.
Head selection preserves the element layout without demanding the selected value;
tail, reversal and dropLast preserve that layout while changing only the spine.
Take and drop use physical element specializations without inspecting retained
or removed elements. Counts use the ordinary arbitrary precision Int helpers.
For an outer pending spine, a typed bridge tests the count before demanding the
spine: nonpositive take returns an empty list, while nonpositive drop returns the
original carrier. A positive count demands only the outer spine. Singleton,
push, pushBack and append merge element layouts and promote concrete inputs
without demanding pending elements. Builders inspect outer spines when their
operation needs them. Append uses a typed bridge when its suffix has outer
pendingness: it inspects the prefix, returns the untouched suffix for an empty
prefix, and demands the suffix spine only for a nonempty prefix. List.map can
coordinate a known local callback with the input element layout and the callback
result layout. It invokes callbacks eagerly; callback bodies demand only the
values they inspect, and pending callback results remain stored in the mapped
list. Known filter, any, all, takeWhile, dropWhile, findFirst and findFirstIndex
predicates use the same input interface; when a predicate returns a pending Boolean, a typed adapter
demands that Boolean before the library chooses whether to retain the element
or continue traversal. Unused input fields remain unresolved. Any and all
preserve left-to-right short circuiting; takeWhile and dropWhile stop at their
first false decision. Element and index searches stop at their first true
decision. `findFirst` preserves the selected element's field layout inside
`Option`, so inspecting the result's tag or another tuple field does not demand
an ignored payload. The native result adapter materializes the selected payload
when needed. These operations demand an outer pending list before traversing it.
Empty lists never invoke the predicate. Already-started
producers retain their normal runtime ownership. Known fold steps share one
physical accumulator interface with the initial value and every step result.
Steps execute eagerly, but unused elements and intermediate pending accumulators
are not demanded. Empty folds return the initial accumulator unchanged. Opaque
callback interfaces still require diagnostics when their ABI would change. Native
result adapters map typed demands over the list. List pattern bindings preserve
element layouts; length and empty checks inspect only the spine, while literal
comparisons and guards demand the values they inspect.

Dictionary values likewise carry a physical field layout through literals,
ordinary calls, captures, updates and merges. Keys remain concrete: insertion,
membership and lookup demand the key before hashing, but do not demand unrelated
values. Size, emptiness, keys, values and entry enumeration inspect only the
dictionary structure; a lookup preserves its selected value's layout inside
`Option`. Native result adapters materialize retained values. Known `Dict.map`
callbacks execute eagerly and can leave their results pending. Known `Dict.filter`
callbacks demand only their Boolean decision; unused value arguments remain
unresolved, and empty dictionaries never invoke the callback. Both operations
retain the public library's traversal order. A typed callback adapter reconciles
concrete dictionary keys with a shared callback's wider key interface. Private sum
metadata is available before runtime-helper specialization so raw managed slots
retain and release nested variant values using their actual sum representation.
Entry-list constructors load their logical source specializations on demand when
the precompiled native body cannot accept the entry layout. Ordinary signature
inference then adapts the source loop, including independently pending tuple
fields. The rejecting constructor stops at its first duplicate: it does not
demand keys in later entries, and neither constructor demands unused values.
Loading a logical body preserves the existing function identity; only its
changed async version is emitted, leaving the synchronous ABI intact.

Storage layouts have an opaque interface in `AsyncTransport`: lowering inspects
one level at a time, while mode equality and physical aggregate descriptors
belong to the transport module. This separates call-interface inference from the layout
representation needed for recursive aggregates. Storage interfaces are finite
node graphs: equality is bisimulation, joins solve shared recursive constraints,
and regular cycles close at fully instantiated sum descriptors. Private sum
identities are reserved before resolving cyclic payload types. This supports
recursive variants, including paths through tuple and list storage, without
forcing ignored leaves. Allocation adapters reconcile widened recursive payload
interfaces; native result adapters rebuild only the selected variant at each
step. Generic specializations compiled as separate native dependencies retain
their logical bodies for the same inference as ordinary functions.
A repeated generic owner with
different instantiated arguments, such as `Option<Option<Int64>>`, is finite
and must not be confused with a recursive nominal layout.

Unmigrated list and dictionary operations,
recursive aggregate layouts without a sum anchor, untyped value selections, and
escaping callables whose argument or result interface changes still produce
compilation diagnostics. Outer pendingness also carries the inferred field
layout. A generated mapper
promotes only concrete fields, and `__mapValueCurrent` applies it to an outer
payload when demanded. This reconciles wholly pending containers and concrete
containers with pending fields without waiting at calls, captures or returns.
Native result adapters first demand the outer container, then materialize fields. The hooks are
compiler test interfaces, not public async APIs or automatic I/O lowering.

Runtime helper dependencies are requested explicitly by lowering. When a typed
specialization is missing, the ANF pipeline materializes it from the retained
stdlib generic definitions, converts and canonicalizes its body, and retries
lowering with the extended function and symbol registries. Inserted helper bodies
reserve identities allocated during native dependency compilation, then receive
fresh ANF temporaries and the same inlining exclusions as other late
stdlib specializations. The source hooks do
not contain fallback force calls to cause incidental specialization.

| Operation | Demand |
|---|---|
| Binding or returning a value | None |
| Passing an ordinary argument | None solely because of the call |
| Constructing a tuple, list, record, variant or closure | No demand solely to store a field or capture |
| Invoking a callable | Demand the callable's concrete function representation |
| Arithmetic or a branch condition | Demand the operands actually inspected |
| Matching a constructor or literal | Demand the value inspected by that pattern |
| Projecting a field | Demand the outer container; not an unrelated pending field |
| Hashing, equality or rendering | Demand the parts inspected by the operation |

Full-width scalars and floats cannot safely share a spare pending sentinel.
Compiler transport/storage classification must therefore distinguish concrete
payload words from pending-capable carriers, consistently across signatures,
joins, aggregate fields, raw slots, ownership and native lowering. This migration
must not assume the current optimized sum and container layouts already support
pending payloads.

## Native tasks and explicit forcing

`packages/darklang/stdlib/__task.dark` implements a private, single-threaded cooperative scheduler.
`__run` accepts root closures and gives each one a borrowed task-context pointer.
Ordinary calls below each root use its existing native stack. `__force` returns
a ready payload immediately; otherwise it registers the task on the promise and
switches to the scheduler. Completion enqueues registered tasks in FIFO order.
The scheduler never repeatedly probes parked tasks. `__yield` returns a runnable
task to the back of the queue.

`__forceCurrent` obtains the current task from a private native runtime slot, so
ordinary functions can demand a value without receiving a context parameter.
The native switch updates that slot before resuming a task and clears it while
the scheduler executes completion callbacks. Nested explicit `__run` scopes
restore the enclosing task before disposing their scheduler. Ready values can
be forced outside a task; forcing a pending value there reports a runtime error.
The private source hooks use this helper for inserted demand calls. Public I/O
calls and CLI entry points have not yet been migrated.

`__runMain` is a private typed entry adapter from an ordinary `Unit -> a` closure
to a root task. Its promise retains the result after the root stack is released;
the adapter returns an owned payload after the explicit scope drains. Together
with `__delayCurrent`, it permits context-free runtime tests through normal Dark
functions. It retains the explicit scope's drain policy and is not automatically
applied to CLI programs.

`__startCurrent` enters a separate operation task immediately, returning its
pending carrier when the caller next runs. The operation can park, yield or
finish without waiting for its consumer. Started operations stay owned by the
explicit scheduler scope even if their consumer is discarded. This is an
operation boundary; ordinary calls below the operation use its native stack.
Dynamic active-task accounting includes these operations. A switch back to the
scheduler records the actual returning task, which can differ from the task the
scheduler last entered when an operation starts another operation directly.

On Linux x64, each task has an 8 MiB fixed-address virtual-memory stack with a
64 KiB inaccessible guard at each end. The native switch saves stack and frame
pointers, allocatable callee-saved integer registers and floating-point
registers. Normal native-effect call barriers preserve caller-owned registers.
Heap and free-list cursors remain shared between tasks: restoring an old heap
cursor would overwrite allocations made since suspension. The initial fiber
frame links to the process root so argv/environment helpers retain access to
startup arguments. ARM64 fiber intrinsics currently reject compilation.

The scheduler owns every task until its explicit scope completes. A task may be
destroyed before it starts or after it returns; dropping a runnable or parked
stack is an invariant failure because its locals still own managed values.
Task entry closures are normal managed closures, and stack mappings participate
in native leak accounting.

`__delay` starts a private monotonic timer eagerly and returns a pending carrier.
The completion callback retains both the promise and its typed payload until
the deadline. `__watchCurrent` starts a one-shot native socket readiness wait
and returns a pending ordinary `Result<Ready, Int64>` carrier. Its registration
owns the descriptor lifecycle and producer promise until completion, including
when the consumer is discarded. Registrations wake tasks in registration order;
native readiness errors remain ordinary result payloads.

`__readTcpCurrent` starts a nonblocking native receive immediately and returns a
typed pending result. An operation that would block registers a readiness
callback retaining its connection and promise; the callback retries receive
before resolving the payload. No task is created for this operation. EOF is an
ordinary empty Blob and socket errors are ordinary `Result.Error` payloads.
This private chunk-read primitive does not yet replace public HTTP calls.

`__writeTcpCurrent` attempts nonblocking sends immediately and returns a typed
pending `Result<Unit, Int64>`. It advances through bounded chunks and partial
sends, registering a writable callback only when the socket would block. The
callback retains the original payload, offset, connection and producer promise
until every byte is accepted or an ordinary socket error resolves the result.
Discarding the consumer does not discard the remaining write. Empty writes
still validate their connection. Like the read primitive, it requires an
explicit scheduler scope and does not yet replace public HTTP calls.

Timers and socket events land on the scheduler thread. Runnable tasks permit
nonblocking readiness probes; with no runnable task, the scheduler waits in the
OS for registered descriptors or the earliest timer deadline. Interrupted waits
preserve registrations. This explicit test/runtime scope drains all registered
tasks, timers and readiness registrations. It does not choose the production
program's policy for unused outstanding I/O, cancellation or effect ordering.

The next integration boundary is compiler carrier and demand lowering across
closures, indirect calls and aggregate storage, followed by automatic entry
and real I/O initiation. The explicit runtime does not establish those source
semantics by itself.

## Tests and validation boundary

`test/fixtures/e2e/stdlib-internal/async_promise.e2e` exercises native promise
transitions, idempotent waiter registration, late registration, multiple reads,
typed payloads, consumer/producer ownership, managed result survival and payload
disposal. `async_value.e2e` exercises explicit carriers, including scalar bits,
pending storage, closure capture, return and consumer abandonment. These tests
do not establish implicit propagation or platform-specific I/O behavior.
`async_task.e2e` adds explicit native scheduling cases: round-robin progress,
deep live stack locals, multiple promise waiters, timers, floating-point values,
shared-heap allocation and startup arguments inside a fiber. These remain
private runtime tests rather than evidence of transparent source propagation.
`async_context.e2e` exercises implicit context lookup across deep calls, task
switches, nested scopes and scheduler callbacks, including pending demands
outside a task.
`async_entry.e2e` checks typed entry results, managed aggregates and closures,
ordinary `Result.Error` payloads, nested entries and leak accounting.
`async_io.e2e` checks native descriptor readiness, socket closure from a timer,
FIFO wakeups, invalid descriptors and abandoned readiness consumers.
The native `async_socket_regression.py` peer sends response B and requires its
acknowledgement before sending response A. A single task starts both reads and
consumes B first, proving eager initiation without an early wait on A. The
regression also checks native leak accounting. It runs both explicit runtime
carriers and ordinary source `Result<Blob, Int64>` values passed through helper
functions; the latter relies on compiler-inserted demands.
The write cases prevent a peer from draining an 8 MiB patterned payload until a
second connection receives its command. They check complete byte ordering under
backpressure, explicit and source-typed results, ordinary errors after peer
closure, empty writes, and producer ownership after consumer abandonment.
`async_write.e2e` checks that unavailable connections resolve ordinary errors,
including for empty payloads.
`async_start.e2e` checks eager entry, nested operations, runnable ordering,
producer ownership after consumer abandonment, managed results and startup
arguments from child stacks.
`async_chain.e2e` checks composition without early demands, repeated reads,
managed storage and capture, abandonment, and deep floating-point chains.
`async_demand.e2e` checks ordinary source types through bindings, aliases,
arithmetic, conditions, unused values and explicit native entry results.

`async_calls.e2e` and `async_returns.e2e` check direct argument/result transport,
recursive calls, mixed concrete and pending call sites, branch joins, and
unselected pending arguments. They resolve a promise after passing it through
ordinary functions, so an early demand prevents the expected result.

`async_capture.e2e` checks delayed inspection of captured integers, floats and
managed payloads, abandoned and nested captures, known closure calls, escaping
inspecting closures, callable aliases, and native child task result adaptation.

`async_tuple.e2e` checks construction, aliases, independent integer/float fields,
unrelated projections, abandoned managed fields, nested tuples, and concrete
native entry results.

`async_record.e2e` checks independent fields, managed native results, nested
records and tuples, record updates and abandoned fields. `async_callable.e2e`
checks that passing or abandoning a pending function value does not demand it,
while invocation does. `async_result.e2e` checks constructor demands across
ordinary calls and preserves managed `Error` values as ordinary payloads.

`async_storage_calls.e2e` checks tuple and record arguments and results, fields
constructed in callees, unchanged concrete call sites, captured aggregates and
typed branch joins. The caller resolves a promise after the ordinary call has
returned, exposing early demands at those boundaries.

`async_runtime_helper.e2e` checks a custom record payload whose typed demand helper
is independent of the root result type, including specialization while compiling
a preamble and later use of its source functions.

`async_mixed_storage.e2e` checks mixed outer and field pendingness across calls,
nested fields and captures, abandonment, managed payloads, and native results.

`async_list.e2e` checks pending elements in leaves, trees and connected digits,
spine-only length and emptiness, native float results, abandonment, ordinary
calls, captures, nested lists and mixed record elements. It also reconciles a
wholly pending list with a list containing independently pending elements. Head
selection defers demands until inspection, and tail and reversal leave unresolved
elements alone, including managed payloads and records with pending fields.

`async_list_patterns.e2e` covers exact and cons patterns, nested lists and tuples,
managed and floating point bindings, outer pending lists, literal comparisons
and guards. Wildcards and unused bindings leave element producers unresolved.

`async_list_construction.e2e` checks singleton, push, pushBack and append with
pending scalars, managed values, tuples with pending fields, and pending outer
list spines. Constructing a singleton around an unresolved list does not demand
that inner spine.

`async_list_append.e2e` checks conditional suffix demands with ready and pending
prefixes, abandonment, and native managed results.

`async_list_spines.e2e` checks take and dropLast with pending elements, managed
suffixes, floats, nested fields, mixed outer readiness, pending counts and
arbitrary precision counts. `async_list_drop.e2e` checks the corresponding drop
boundaries, including returning an unresolved outer carrier for nonpositive
counts and demanding its spine only for positive counts.

`async_list_map.e2e` checks known callbacks that ignore or inspect pending inputs,
return pending values or records with pending fields, share ready and pending
call sites, initiate producers eagerly, and retain mixed outer and field layouts.

`async_dict_storage.e2e` and `async_dict_storage_edges.e2e` cover independent
value demands, pending keys, updates, merges, lookup tags, managed values, nested
storage, outer pending dictionaries and abandonment. `async_dict_map.e2e` and
`async_dict_callbacks_edges.e2e` cover eager map invocation, pending callback
results, filtering decisions, empty dictionaries and shared callback interfaces.
`async_dict_from_list.e2e` covers rejecting and overwriting constructors, pending
keys independent of values, early duplicate rejection, outer pending entries and
managed result ownership.
`async_captured_variant_ownership.e2e` checks captured pending values returned
inside variants directly and through list callbacks, including leak checking.
`async_sum_nested_generic.e2e` checks repeated generic owners at distinct
instantiations, source-call joins and managed native results.

`async_list_filter.e2e` covers ignored and inspected inputs, pending and delayed
Boolean results, shared predicate layouts, tuple field projection, and managed
elements retained or rejected without premature demands.

`async_list_predicates.e2e` checks any and all with ignored or inspected inputs,
pending and delayed Boolean decisions, empty lists, shared predicate layouts,
mixed outer readiness and short-circuiting. Callback side effects verify that
later predicates are not invoked after a decisive result.

`async_list_search.e2e` checks element and index searches with ignored or demanded
inputs, pending predicate decisions, selected tuple fields, shared callbacks,
native managed results and outer pending lists. Empty searches and effects in
later predicates check that traversal stops at the correct point.

`async_raw_release.e2e` covers sized raw releases with pending byte counts for
small and large allocations. Release demands the original byte count before
choosing the allocator path; the concrete-size case also exercises release
within an active task context.

`async_higher_order.e2e` checks ordinary application helpers, recursion, a
user-defined map and fold, multiple callbacks and callback arguments, pending
producers and results, managed and nested captures, eager callback invocation,
shared concrete/pending calls, and
operation with optional inlining disabled.

`async_callback_forwarding.e2e` checks direct forwarding chains, recursive and
duplicated callbacks, independent captures, managed results, named function
callbacks, multiple arguments, and list storage without premature demands.

`async_list_while.e2e` checks takeWhile and dropWhile with independently pending
elements, nested fields, pending Boolean decisions, shared callback interfaces
and outer pending lists. It also verifies empty inputs, suffix preservation and
stopping before later predicate effects or unresolved element inspections.

`async_list_fold.e2e` covers ignored pending elements and initial accumulators,
pending step results, independent float demands, record fields, empty folds,
eager step invocation, managed ownership and shared callback interfaces. Source
ANF is retained separately from synchronous list-region optimization, including
preamble and standard-library definitions: a later async call must still see
the list values and callback interface, not a scalar loop that erased them.
Synchronous native compilation and inlining retain their optimized definitions.

`async_recursive_sum.e2e` checks ignored and demanded recursive leaves, shared
ready/pending calls, managed cleanup, native result conversion, distinct generic
instantiations, tuple/list recursion and outer pending trees. The cases also run
as one batch: independently optimized copies of a runtime specialization must
not lose any dependency when tree shaking selects the emitted body.
