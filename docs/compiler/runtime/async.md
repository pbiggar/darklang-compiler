# Async runtime

The target is transparent pending values with stackful native tasks. Async
operations start eagerly and return without waiting for their result. Only an
operation that needs a concrete value forces it. Pending values may pass through
calls, returns, containers and closures without changing source types. Ordinary
native calls remain ordinary calls; transitive callers are not async state
machines. Interpreter differences are not constraints on this implementation.

## Implemented promise core

`StdLib/__Async.dark` implements a private, typed promise object and an explicit
ready/pending value carrier. Both are embedded in the compiler, using the existing
typed raw-slot operations, sums and lifecycle owners. Implicit source-value
lowering is not yet implemented. No language operation currently initiates
asynchronous work, parks a task, or forces one of these cells.

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

`__Value<a>` is a boxed sum with `ValueReady a` and `ValuePending (__Promise<a>)`
variants. The ordinary sum release plans own either payload correctly, including
when carriers are returned, captured or nested in containers. `__ready` and
`__pending` construct it; `__pollValue` probes it without waiting. This separates
the discriminator from full-width scalar/float payloads while retaining ordinary
native calls. It does not yet change the representation of source-level values.

The following demand rules guide the next compiler slice; they are not
implemented by defining these explicit runtime types alone.

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

After carrier integration, add fixed-address VM-backed task stacks and a
single-threaded runnable scheduler. Connect force to waiter registration and
stack suspension, then validate a private timer before migrating HTTP. Do not
discard suspended stacks: their locals still carry reference-count obligations.

## Tests and validation boundary

`test/fixtures/e2e/stdlib-internal/async_promise.e2e` exercises native promise
transitions, idempotent waiter registration, late registration, multiple reads,
typed payloads, consumer/producer ownership, managed result survival and payload
disposal. `async_value.e2e` exercises explicit carriers, including scalar bits,
pending storage, closure capture, return and consumer abandonment. These tests
do not establish implicit propagation, overlap, scheduler progress, native stack
suspension, transparent propagation or platform-specific I/O behavior. Those
require the later end-to-end task and I/O slices.
