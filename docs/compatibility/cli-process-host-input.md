# CLI, process, host, and input parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../porting/README.md) owns current support and test dispositions.

## Public contract

| Area | Public surface | Observable contract |
| --- | --- | --- |
| Shell execution | `ExecutionOutcome`, `execute`, the two Result helpers | `$HOME` expansion, `$SHELL` with `/bin/bash` fallback, inherited stdin, independent complete stdout/stderr, untrimmed outcome text |
| Host | `OS`, `Architecture`, `Shell`, `Host` and discovery functions | Interpreter variants, normalized `uname`, shell recognition, and error precedence |
| System | `Env.get`, `Env.getAll`, `Cli.Env` lookup/default/home/mutation, the Posix API, `Sys.*` | Exact environment values, child inheritance, passwd fallback, LOGNAME/USER/account precedence, uname values, PID/UID, online CPUs, EPERM-as-running |
| Filesystem | `Cli.FileSystem.currentDirectory`, `listDirectory`, `pathExists`, `isDirectory`, `readFile`, `readFileAsString`, `overwriteFile`, `appendToFile`, and `deleteFile` | Blob I/O, full paths in directory listings, structured file errors, and observable file mutation |
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

The complete interpreter Posix boundary is now implemented: all 29 Builtin
dependencies of the pinned package, with `Cli.File`, `Cli.Dir`, and
`Cli.FileSystem` reimported over it. This extension is pinned to darklang/dark
`v0.0.35` revision `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`.
Daemon is excluded. Coverage, imported behavior, native target details and
validation limits are recorded in [POSIX host API coverage](posix-host-api.md).
`Platform.target` and `CompilerExecution.execute` belong to the compiler driver.

## Native boundary

CLI operations are explicit effectful nodes in ANF, MIR, and LIR. Ownership
analysis treats managed arguments as borrowed through a native call and native
results as owned. Register allocation recognizes the operations as calls. At
startup the original environment is retained before the `_start` prologue;
process-handle and pending-terminal roots are allocator-independent runtime
state. Backends construct normal managed Int/String/Result/record/tuple values,
so ordinary reference counting owns their lifetimes.

Host, account, filesystem, and environment discovery do not launch Unix
command-line utilities. `Env.get` and `Env.getAll` walk the native `envp` and
copy exact values, including trailing spaces and newlines. `Cli.Env.set` and
`unset` update the same environment vector used by lookup and child `execve`.
Filesystem operations lower to the target kernel ABI, while portable Dark code
decodes packed directory/environment data into ordinary List and Dict values.
Architecture and operating-system values are fixed by the compile
target. Hostname, PID, UID, signals, and online CPU count use the target kernel
ABI directly: Linux counts the `sched_getaffinity` mask, while macOS reads
`HW_AVAILCPU` through `sysctl`. `Sys.currentUser` preserves LOGNAME/USER
precedence and otherwise calls `Builtin.posixGetCurrentUserName`, which matches
the native UID in `/etc/passwd`;
`Cli.Env.home` similarly prefers HOME and then the passwd home field. These
paths therefore do not depend on
`PATH`, a configured shell, utility output wording, or text trimming.

This native-host boundary is separate from APIs whose purpose is to execute a
command. `Cli.execute` and `Process.shellPipeline` intentionally accept shell
language. The current portable `Process.runIn`, environment, timeout, and pipe
conveniences are also implemented through the process-launch surface; they are
not used for host, environment, account, architecture, or signal discovery.

The sleep portion of this boundary was separately revalidated from compiler
baseline HEAD `7e08aa752a123ba6094ce6f6aafac6dfd2c8a4a9` against the same exact
darklang/dark revision above. `Stdlib.Cli.Posix.sleep` now calls the internal
compiler-only `Stdlib.Cli.__sleep` intrinsic. Its Float millisecond argument is
carried by a typed ANF/MIR/LIR sleep operation and lowered to blocking
`nanosleep` on Linux ARM64, Linux x86_64, and macOS ARM64. The native timeout is
normalized into seconds and nanoseconds, and interruption resumes with the
remaining timeout. This replaces the compiler baseline's observable shell
execution at `packages/darklang/stdlib/cli/posix.dark`; it does not add a
public compiler extension. The full revision-pinned comparison is in
[Option, Result, and Retry compatibility](stdlib/option-result-retry.md).

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

The compiler public wrappers are in `packages/darklang/stdlib/Cli.dark` and `packages/darklang/stdlib/cli/`
module files.
The registry is `src/DarkStdlib.ml`; typed lowering starts in
`src/passes/anf/AST_to_ANF.ml`, passes through `src/ir/anf/ANF.ml`, `src/ir/mir/MIR.ml`, and `src/ir/lir/LIR.ml`, and
ends in both architecture code generators. Focused native evidence is
`test/fixtures/e2e/cli_process_host_input.e2e`,
`test/fixtures/e2e/filesystem_env_parity.e2e`, plus the enabled pinned
`test/fixtures/e2e/upstream/stdlib/cli-process.dark` corpus. Target-ABI coverage is
in `test/compiler-passes/ARM64CodeGenTests.ml`, and executable Linux x86_64 coverage runs the native
operations under QEMU in `test/compiler-passes/X86_64CodeGenTests.ml`.

## Host adapter contracts

`Builtin.timeNowMs` converts the syscall-backed monotonic clock to Int and
raises with errno on clock failure. Only elapsed differences are meaningful.
`Builtin.getBuildHash` uses the same build-time abbreviated Git stamp as
`--version`, or `dev` without metadata; deployed programs do not invoke Git.

`currentExecutablePath` discovers the running image. Linux uses `/proc/self/exe`
with a captured `AT_EXECFN`/startup cwd/PATH fallback; macOS uses Darwin
`proc_info`. Never infer the path solely from `argv[0]`. Discovery retains a
bounded cache (8175-byte payload); unavailable or oversized paths fail through
the public String-returning error boundary.

`currentUser` preserves LOGNAME, USER, then builtin precedence. The builtin
matches the real UID in `/etc/passwd`, ignores environment overrides, and
returns None for missing/unreadable data or an unmatched UID. It does not
resolve NSS-only or macOS Directory Services accounts. Linux CPU count uses
the process affinity mask; macOS queries HW_AVAILCPU, with a minimum-one
fallback. This can differ from the interpreter's online-CPU count.
PID and UID adapters expose native real process/user IDs as Int.

## Terminal and input boundaries

Terminal facts use direct terminal-attribute queries and the native TERM value.
Viewport size tries stdout, stdin and stderr, rejects zero kernel dimensions,
then accepts positive Int32 COLUMNS/LINES independently, defaulting to 80/24.
Color requires terminal stdout and unset/empty NO_COLOR; any nonempty value,
including `0` or whitespace, disables it. TERM and stdin do not affect color.
Interactivity is true when either stdin or stdout is a terminal.

Row inspection measures plain EGC display width, including escape payloads;
C0/C1/CRLF control detection tells the caller when to sanitize. Shared stdin
buffering preserves bytes across line, counted and whole-stream reads. Lines
accept CR, LF and CRLF and return empty at EOF. All/count reads replace malformed
UTF-8; counts measure UTF-16 units, reject invalid counts and preserve a remaining
surrogate when a read splits an astral character. Native strings render an
isolated surrogate as replacement rather than preserving raw UTF-16 identity.

Key reads temporarily disable canonical mode, echo and terminal signals, then
restore attributes and masks. Text/CSI/SS3 events and modifiers are decoded;
repeat/paste coalescing has 4 ms quiet and 40 ms total bounds. A differing control
key is retained, resize while waiting yields NoName, and redirected stdin yields
Escape. Secret input retains owned text chunks, Enter, grapheme Backspace and
paste behavior. Its redirected fallback tests stdin itself: the general
stdin-or-stdout interactivity predicate would otherwise loop on Escape when
only stdout is a terminal.
