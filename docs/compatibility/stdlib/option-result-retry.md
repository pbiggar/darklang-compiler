# Option, Result, and Retry parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

## Revalidated contract

| Work item | Interpreter behavior at the pinned revision | Compiler implementation and focused evidence | Classification |
| --- | --- | --- | --- |
| `Option.and<'a, 'b>` | If the first input is `Some`, return the second input; if it is `None`, return `None`. The payload types may differ. | `packages/darklang/stdlib/option.dark`; all four truth-table cases and differing payloads are in `e2e/control_combinators_retry.e2e:4-10`. | Public parity |
| `Result.and` | Return the second input for `Ok`; otherwise return the first `Error`, including its payload. | `packages/darklang/stdlib/result.dark`; the four pinned truth-table cases are enabled and distinct payloads are checked at `control_combinators_retry.e2e:12-16`. | Public parity |
| `Result.or` | Return the first input for `Ok`; otherwise return the second input, including its payload. | `packages/darklang/stdlib/result.dark`; the four pinned truth-table cases are enabled and distinct payloads are checked at `control_combinators_retry.e2e:17-20`. | Public parity |
| Argument evaluation | Function and argument expressions are evaluated eagerly and arguments are evaluated left to right before the combinator selects a value. | Same-source print probes at `control_combinators_retry.e2e:22-26` require the exact byte sequence `12` before the enclosing result. | Public parity |
| `Retry.withBackoffLoop` / `withBackoff` | Run the callback before testing the limit. Stop on the first `Ok` or when `attempt >= maxAttempts`; return that callback result unchanged. Sleep only after an eligible `Error`, then increment the attempt and double the delay. The wrapper starts at attempt 1 and 100.0 ms. | Portable source at `packages/darklang/stdlib/retry.dark`; deterministic callbacks cover immediate success, nonpositive maximum, terminal failure, explicit starting attempts, eventual success, counts, and final payloads at `control_combinators_retry.e2e:28-43`. | Public parity |
| `Retry.withFixedDelayLoop` / `withFixedDelay` | The same termination and propagation rules apply, but the delay is unchanged. The wrapper starts at attempt 1. | Portable source at `packages/darklang/stdlib/retry.dark`; deterministic focused cases cover both entry points and a real 1 ms inter-attempt delay at `control_combinators_retry.e2e:32-42`. | Public parity |
| Retry delay | Milliseconds are passed to a blocking delay between callback attempts only. | `Cli.Posix.sleep` delegates at `packages/darklang/stdlib/cli/posix.dark`; the typed effect is introduced in `src/passes/anf/lowering/LoweringPrimitives.ml` and retained as a Float through ANF/MIR/LIR. Code generation normalizes total nanoseconds into native seconds/nanoseconds and retries the remaining timeout on `EINTR` for Linux ARM64, Linux x86_64, and macOS ARM64. Backend assertions pin conversion, syscall numbers, target conventions, and interruption loops. | Behavior parity through an internal AOT boundary |

The imported Option/Result truth tables and `control_combinators_retry.e2e`
cover argument order, distinct payloads, callback counts, termination and
result propagation. Current selection is recorded in source annotations.

Option, Result and Retry use original interpreter source selected at the pin in
`porting/sources.yaml`; native arithmetic and sleep remain compiler dependencies.
The empty arm of `Option.and` constructs `None`, allowing differing payload types.

Qualified generic constructors retain the contextual result type in their
checked metadata, including parameters absent from their payload. This keeps
the nullable representation of `Option<String>.None` consistent between its
constructor and caller. `qualified-generic-constructor-result.e2e` covers a
user-defined enum, Option, and Result across generic function boundaries.

Enabling the exact upstream `Option.and(None, None)` case previously exposed a
parser association error: the second nullary constructor was attached as a
payload of the first. The current interpreter parser handles constructor
payload application through its normal call parsing path. This is required
for the unchanged pinned source.

## Pinned source anchors

At the interpreter revision, the public implementations are
`packages/darklang/stdlib/option.dark:187-195`,
`packages/darklang/stdlib/result.dark:197-214`, and
`packages/darklang/stdlib/retry.dark:4-52`. Interpreter application lowering
that establishes eager left-to-right argument evaluation is
`backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs:937-963`. Its delay
boundary converts milliseconds to a `TimeSpan` and awaits `Task.Delay` in
`backend/src/Builtins/Builtins.Time/Libs/Time.fs:14-31`.

The compiler loads `packages/darklang/stdlib/retry.dark` in `src/driver/StdlibCompilation.ml`; represents delay in the
ANF, MIR, and LIR definitions; assigns target syscall numbers in `src/Platform.ml`;
and lowers the operation in `src/backend/arm64/instructions/ARM64EmitNativeEffects.ml` and
`src/backend/x64/instructions/X64EmitNativeEffects.ml`.

## Extensions and intentional divergences

`Stdlib.Cli.__sleep` is an internal compiler-only execution boundary, not a
public stdlib extension. Public Dark code uses `Stdlib.Cli.Posix.sleep`, while
the exact-name intrinsic lets the AOT pipeline carry its Float argument through
a typed sleep operation instead of an untyped CLI operation or shell process.

The compiler continues to report statically knowable type errors ahead of
execution. The interpreter may encounter analogous errors only when evaluating
the affected expression. This timing is the retained intentional divergence;
the accepted Option, Result, and Retry results above do not intentionally
diverge.
