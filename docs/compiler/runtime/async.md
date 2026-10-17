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
lowering is not yet implemented. The private `__Task` runtime connects these
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

## Native tasks and explicit forcing

`StdLib/__Task.dark` implements a private, single-threaded cooperative scheduler.
`__run` accepts root closures and gives each one a borrowed task-context pointer.
Ordinary calls below each root use its existing native stack. `__force` returns
a ready payload immediately; otherwise it registers the task on the promise and
switches to the scheduler. Completion enqueues registered tasks in FIFO order.
The scheduler never repeatedly probes parked tasks. `__yield` returns a runnable
task to the back of the queue.

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
the deadline. Timers land on the scheduler thread; when no task is runnable the
scheduler sleeps until the earliest deadline. This explicit test/runtime scope
drains all registered tasks and timers. It does not choose the production
program's policy for unused outstanding I/O, cancellation or effect ordering.

The next integration boundary is compiler carrier and demand lowering across
signatures, joins, closures and aggregate storage, followed by automatic entry
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
