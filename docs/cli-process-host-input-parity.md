# CLI, process, host, and input parity

This contract was revalidated against compiler HEAD
`cce6860f02b13d416295ff342341eeeae997f49f` and darklang/dark
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit
`8a402797` was only a discovery index. The approved evidence revision
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` has no public CLI family.
All same-source comparisons in this work use those exact revisions.

## Public contract

| Area | Public surface | Observable contract |
| --- | --- | --- |
| Shell execution | `ExecutionOutcome`, `execute`, the two Result helpers | `$HOME` expansion, `$SHELL` with `/bin/bash` fallback, inherited stdin, independent complete stdout/stderr, untrimmed outcome text |
| Host | `OS`, `Architecture`, `Shell`, `Host` and discovery functions | Interpreter variants, normalized `uname`, shell recognition, and error precedence |
| System | `Env.get`, the Posix support subset, `Sys.*` | LOGNAME/USER/account precedence, uname values, PID/UID, online CPUs, EPERM-as-running |
| Processes | `ProcessHandle`, `Output`, `spawn`, `communicate`, `terminate`, and portable helpers | PATH-aware argv execution, inherited environment/stdin, normalized signal exits, timeouts, pipelines, and PID lifecycle |
| Input | `Key`, `Modifiers`, `KeyRead`, helpers, and `readKey` | Portable key names, ALT+CTRL+SHIFT display order, UTF-8/ANSI decoding, repeat coalescing, resize events, and terminal restoration |

`Process.Output` intentionally contains `String`, not `Blob`; that is inherited
interpreter behavior. Native execution is unrestricted, so comparison assumes
the interpreter is run with the capabilities required by the command under
test. Performance is outside the contract unless it changes a result.

`Windows`, `Arm`, `Arm64`, `Armv7l`, and `PowerShell` are represented for
source compatibility but are not produced on the supported compiler targets.
The Linux `uname -m` nonzero branch is an intentional failure-path repair: the
interpreter source omits that match arm, while the AOT compiler requires the
branch to make the result total. Ahead-of-time type errors remain the normal
compiler timing divergence.

Only `Posix.Error`, `kill`, `sigterm`, `sigkill`, `sleep`,
`isProcessRunning`, and `Env.get` are claimed from the much larger interpreter
Posix/Env API. The remainder is outside this parity scope. F#
`Platform.Target` and `CompilerLibrary.execute` are AOT driver internals, not
Dark extensions. The former public `Stdlib.Platform.isMacOS/isLinux` extension
was removed in favor of `Stdlib.Cli.OS.getOS`.

## Native boundary

CLI operations are explicit effectful nodes in ANF, MIR, and LIR. Ownership
analysis treats managed arguments as borrowed through a native call and native
results as owned. Register allocation recognizes the operations as calls. At
startup the original environment is retained before the `_start` prologue;
process-handle and pending-terminal roots are allocator-independent runtime
state. Backends construct normal managed Int/String/Result/record/tuple values,
so ordinary reference counting owns their lifetimes.

Process implementations must close every descriptor on success and failure,
retry interruptible operations where the interpreter does, preserve errno,
drain stdout and stderr concurrently, normalize wait status, and force cleanup
of tracked children at shutdown. Terminal input saves/restores termios, treats
Ctrl+C as a key while reading, reports SIGWINCH as `NoName`, and applies the
interpreter's 4ms quiet/40ms total burst rules.

## Source anchors

The pinned public definitions are
`packages/darklang/stdlib/cli/execution.dark:4-41`,
`host.dark:4-142`, `process.dark:4-198`, `stdin.dark:4-506`,
`sys.dark:4-45`, `env.dark:4-6`, and `posix.dark:161-217`.
Native interpreter behavior is in
`backend/src/Builtins/Builtins.Cli/Libs/Execution.fs:25-427`,
`Posix.fs:343-545,945-1052,1305-1398`, and `Stdin.fs:15-386`.

The compiler public wrappers are in the `src/DarkCompiler/stdlib/Cli*.dark`
module files.
The registry is `src/DarkCompiler/Stdlib.fs`; typed lowering starts in
`passes/2_AST_to_ANF.fs`, passes through `ANF.fs`, `MIR.fs`, and `LIR.fs`, and
ends in both architecture code generators. Focused native evidence is
`src/Tests/e2e/cli_process_host_input.e2e` plus the enabled pinned
`src/Tests/e2e/upstream/stdlib/cli-process.dark` corpus.

## Verification record

The implementation was rebased onto compiler commit
`1f5282a113b2abbc15622d5ad7187fb29199964b`. The integrated tree passed the
complete test suite (`6108/6108`) with a zero-warning, zero-error build. Before
that final rebase, the process implementation verified equal to the routine
baseline. Current main's exact-byte presentation repair adds one observable
final LF write (nine instructions) to every benchmark, so the old snapshot
reported `1.000019`. After the required compatibility reset, all 19 programs
verified equal at a current/baseline geometric ratio of `1.000000`; the
`RESULTS.md` performance ratio remains `2.75x`.
