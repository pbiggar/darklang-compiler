# Option, Result, and Retry parity

Every source comparison in this document uses compiler baseline HEAD
`7e08aa752a123ba6094ce6f6aafac6dfd2c8a4a9` and darklang/dark revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. The approved compiler evidence
revision `51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` and DCB1 report commit
`8a402797ccccda0ca47b516b356ae1de4d670038` were checked as starting evidence;
neither is treated as retained behavioral proof. Performance is outside this
comparison unless it changes an observable result.

## Revalidated contract

| Work item | Interpreter behavior at the pinned revision | Compiler implementation and focused evidence | Classification |
| --- | --- | --- | --- |
| `Option.and<'a, 'b>` | If the first input is `Some`, return the second input; if it is `None`, return `None`. The payload types may differ. | `stdlib/Option.dark:40-44`; all four truth-table cases and differing payloads are in `e2e/control_combinators_retry.e2e:4-10`. | Public parity |
| `Result.and` | Return the second input for `Ok`; otherwise return the first `Error`, including its payload. | `stdlib/Result.dark:58-62`; the four pinned truth-table cases are enabled and distinct payloads are checked at `control_combinators_retry.e2e:12-16`. | Public parity |
| `Result.or` | Return the first input for `Ok`; otherwise return the second input, including its payload. | `stdlib/Result.dark:64-68`; the four pinned truth-table cases are enabled and distinct payloads are checked at `control_combinators_retry.e2e:17-20`. | Public parity |
| Argument evaluation | Function and argument expressions are evaluated eagerly and arguments are evaluated left to right before the combinator selects a value. | Same-source print probes at `control_combinators_retry.e2e:22-26` require the exact byte sequence `12` before the enclosing result. | Public parity |
| `Retry.withBackoffLoop` / `withBackoff` | Run the callback before testing the limit. Stop on the first `Ok` or when `attempt >= maxAttempts`; return that callback result unchanged. Sleep only after an eligible `Error`, then increment the attempt and double the delay. The wrapper starts at attempt 1 and 100.0 ms. | Portable source at `stdlib/Retry.dark:5-16`; deterministic callbacks cover immediate success, nonpositive maximum, terminal failure, explicit starting attempts, eventual success, counts, and final payloads at `control_combinators_retry.e2e:28-43`. | Public parity |
| `Retry.withFixedDelayLoop` / `withFixedDelay` | The same termination and propagation rules apply, but the delay is unchanged. The wrapper starts at attempt 1. | Portable source at `stdlib/Retry.dark:18-29`; deterministic focused cases cover both entry points and a real 1 ms inter-attempt delay at `control_combinators_retry.e2e:32-42`. | Public parity |
| Retry delay | Milliseconds are passed to a blocking delay between callback attempts only. | `Cli.Posix.sleep` delegates at `stdlib/CliPosix.dark:29-30`; the typed effect is introduced at `passes/2_AST_to_ANF.fs:155-157` and retained as a Float through ANF/MIR/LIR. Code generation normalizes total nanoseconds into native seconds/nanoseconds and retries the remaining timeout on `EINTR` for Linux ARM64, Linux x86_64, and macOS ARM64. Backend assertions pin conversion, syscall numbers, target conventions, and interruption loops. | Behavior parity through an internal AOT boundary |

The enabled interpreter truth tables are
`src/Tests/e2e/upstream/stdlib/option.dark:288,291,294,297` and
`result.dark:223,227,231,235,240,244,248,252`. They are gated in
`src/Tests/test-suite-tooling/TestRunner.fs:513-514`. The additional focused
file is deliberately source-compatible with both implementations so argument
order, distinct payloads, callback counts, termination, and result propagation
can be compared without separate fixtures.

The portable Dark source follows the interpreter control flow directly. The
compiler spelling uses its canonical `Stdlib.Int.add` and
`Stdlib.Float.multiply` helpers. In `Option.and`, the `None` arm constructs the
same payload-free `None` value instead of returning `option1`, because the
compiler statically distinguishes `Option<a>` from the declared `Option<b>`
result. Neither adaptation changes an accepted program's observable value.

Enabling the exact upstream `Option.and(None, None)` case also exposed a
compiler interpreter-syntax association error: the parser attached the second
nullary constructor as a payload of the first. Constructor payload application
now follows the same left-associative path as other calls at
`passes/1_InterpreterParser.fs:2202-2208,2267-2270`. This is required for the
unchanged pinned source, not a compiler-only syntax extension.

## Pinned source anchors

At the interpreter revision, the public implementations are
`packages/darklang/stdlib/option.dark:187-195`,
`packages/darklang/stdlib/result.dark:197-214`, and
`packages/darklang/stdlib/retry.dark:4-52`. Interpreter application lowering
that establishes eager left-to-right argument evaluation is
`backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs:937-963`. Its delay
boundary converts milliseconds to a `TimeSpan` and awaits `Task.Delay` in
`backend/src/Builtins/Builtins.Time/Libs/Time.fs:14-31`.

At compiler baseline HEAD, `stdlib/Option.dark:34-50` and
`stdlib/Result.dark:52-70` had no requested combinators, the stdlib source tree
had no `Retry.dark`, and `stdlib/CliPosix.dark:29-30` implemented sleep by
spawning the shell command `sleep`. The implementation now loads
`stdlib/Retry.dark` at `CompilerLibrary.fs:1129-1138`; represents delay at
`ANF.fs:292`, `MIR.fs:154`, and `LIR.fs:190`; assigns target syscall numbers at
`Platform.fs:90-138`; and lowers the operation at
`passes/arm64/6_CodeGen.fs:6142-6195` and
`passes/x64/6_CodeGen.fs:4275-4319`.

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

## Verification record

On Linux ARM64, the focused parity file first failed 24/24 before the functions
and Retry module existed. After implementation it passed 26/26, including the
native 1 ms delay. Enabling the unchanged pinned truth tables then found the
constructor-association error above; after that correction, the Option-focused
set passed 109/109 and the backend sleep assertions passed 3/3.

After integration rebase onto compiler main
`f1e769453ef41309d056ae386351939ce1eb83b9`, `./run-tests --ai` passed
6,819/6,819 tests in 167.8 seconds. The additional main-side ARM64
entry-transfer test and this work's sleep test both remain registered. The
routine benchmark profile verified all 19 workload outputs and parity
contracts against exact Dark baseline
`153d9588152ac5ba7b98ce8d699eacea1abc3b0e`, with every current instruction
count equal and a current/baseline geometric ratio of `1.000000`. The table
header in `benchmarks/RESULTS.md` reports a Performance ratio of `2.25x`.
