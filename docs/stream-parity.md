# Stream parity contract

This ledger was revalidated against compiler commit
`60ed997edae2f59ffeed2f54d539fd1de285bd72` and darklang/dark commit
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit
`8a402797ccccda0ca47b516b356ae1de4d670038` was used only to locate the
original gap; every entry below was checked again against current source.

Interpreter anchors are `packages/darklang/stdlib/stream.dark`,
`backend/src/Builtins/Builtins.Pure/Libs/Stream.fs`,
`backend/src/LibExecution/RuntimeTypes.fs`,
`backend/src/LibExecution/Stream.fs`, and
`backend/tests/Tests/Stream.Tests.fs` at the pinned revision. Compiler anchors
are `src/DarkCompiler/stdlib/Stream.dark`, `src/DarkCompiler/AST.fs`,
`src/DarkCompiler/passes/1.5_TypeChecking.fs`,
`src/DarkCompiler/passes/2.5_RefCountInsertion.fs`, and both
`passes/{arm64,x64}/6_CodeGen.fs`.

## Public operations

| Operation | Contract | Compiler source/test anchor |
|---|---|---|
| `fromList : List<'a> -> Stream<'a>` | Lazy list producer; source ownership moves into producer state. | `Stream.dark:fromList`; `e2e/upstream/stdlib/stream.dark` |
| `unfold : 's * ('s -> Option<('a * 's)>) -> Stream<'a>` | Does not call `step` until pulled; each `Some` replaces the hidden state. | `Stream.dark:unfold`; `e2e/stream.e2e` |
| `next : Stream<'a> -> Option<'a>` | Single-consumer pull. `None` exhausts and closes; later pulls return `None`. | `Stream.dark:next`; both Stream E2E files |
| `toList : Stream<'a> -> List<'a>` | Tail-recursively drains the remaining values. A second drain is empty. | `Stream.dark:toList`; both public Stream corpora |
| `toBlob : Stream<UInt8> -> Blob` | Drains through `Blob.fromList`. | `Stream.dark:toBlob`; upstream Stream corpus |
| `close : Stream<'a> -> Unit` | Idempotent. It disposes hidden state once and makes future pulls empty. | `Stream.dark:close`; internal lifecycle corpus |
| `map` | Lazy, order-preserving transformation; closing it closes the source. | `Stream.dark:map`; both Stream corpora |
| `filter` | Pulls only as needed to find the next accepted value; closing it closes the source. | `Stream.dark:filter`; both Stream corpora |
| `take` | Lazy bounded view; zero/negative counts pull nothing; termination closes the source. | `Stream.dark:take`; both Stream corpora |
| `concat` | Drains sources in order, closing each at handoff and all remaining sources on early close. | `Stream.dark:concat`; both Stream corpora |

The portable combinators are Dark source, matching the interpreter's public
algorithm where its representation permits. Typed raw-slot construction,
state advancement, close, and backend finalization are compiler internals.

## Opaque type and value behavior

`Stream<'a>` is a built-in opaque type with no constructors or patterns. It is
propagated through the canonical parser, aliases, substitution, unification,
specialization/mangling, inference, IR type traversals, and value rendering.
Two values compare equal only when they are the same handle. Ordering is
rejected by AOT type checking. Rendering produces `<stream>` without pulling.
Streams have no persistence encoding and therefore cannot be serialized as
program data. These checks live in `e2e/stream.e2e`.

The interpreter performs unsupported-operation and some callback checks while
executing. The compiler intentionally rejects statically knowable invalid
operations, such as Stream ordering, during AOT type checking; the earlier
error phase is a compiler-model divergence, not an alternate runtime result.

## Native handle ABI

The opaque pointer addresses a 32-byte owned root:

| Offset | Field |
|---:|---|
| 0 | lifecycle tag |
| 8 | owned `Unit -> Option<'a>` step closure |
| 16 | owned `Unit -> Unit` disposer closure |
| 24 | reference count |

Producer state is erased behind a typed owned raw cell captured by both
closures. Replacing or disposing that cell uses typed `RawTake`, so managed
state is released exactly once. `RcShape.StreamRoot`/`RcKind.StreamHeap`
preserve the handle through aliases and containers. On the last release both
backends run the same idempotent close boundary, release callback captures,
return fixed-size storage to free lists, and balance leak accounting.

## Lifecycle transitions

| Current state | Event | Next state and effect |
|---|---|---|
| ready | `next` | advancing, invoke one step |
| ready | `close`/last release | closed, invoke disposer once |
| advancing | callback yields `Some` | ready, publish one value |
| advancing | callback yields `None` | exhausted then closed, dispose once |
| advancing | `close` | close-requested; callback completion closes and publishes no value |
| advancing/close-requested | overlapping `next` | stable runtime error |
| close-requested | callback completes | closed, dispose once |
| exhausted/failed | close/last release | closed, dispose once |
| closed | `next`/`close`/last release | no-op (`next` returns `None`) |

The typed cases are encoded by private lifecycle tags because the public ABI
is an opaque native block. No public program can observe or construct a tag.
`e2e/stdlib-internal/stream.e2e` observes disposer counts rather than only
materialized values, covering natural and empty exhaustion, repeated close,
abandonment, early take, concat handoff, and nested transformations.

Callback runtime errors retain their existing message and exit behavior. Since
the native executable's language-visible runtime-error path terminates the
process, process teardown is the outermost resource boundary; normal return,
early termination, explicit close, exhaustion, and abandonment use synchronous
in-process disposal.

## Classified differences

- **Compiler extension:** last-owner release deterministically closes an
  abandoned Stream. The pinned interpreter relies on GC finalization and notes
  that concurrent single-consumer enforcement is latent.
- **Intentional AOT divergence:** statically invalid Stream operations fail at
  compilation rather than after evaluation begins.
- **Behavior-neutral omission:** the interpreter has a chunked byte-reading
  fast path. The compiler implements `toBlob` by draining `UInt8` values through
  the existing Blob constructor; performance is out of scope and observable
  values, pull boundaries, and close behavior are unchanged.
- **No compiler-only public Stream operations:** the public API is the ten
  operations in the table. Names beginning `__` are private lowering support.
