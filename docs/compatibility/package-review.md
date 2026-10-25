# Deferred package-review decisions

These decisions constrain the item-by-item interpreter package review. They do
not select tests or declarations; the [source ledger](../porting/README.md)
owns current dispositions. Review each new failure with the user before
implementing a fix. Completed fixes and their execution receipts belong in Git
history rather than this registry.

## Existing compiler implementation, interpreter builtin call

Per user direction, defer original wrappers that call `Builtin.*` when the
compiler already implements the corresponding public API. Do not add adapters
or move existing implementations solely to make an original wrapper compile.
A matching public name alone does not establish behavioral equivalence.

The Base64 package was skipped in this category: `Base64.decode` calls
`Builtin.base64Decode`, while compiler implementations also exist for `encode`
and `urlEncode`. The latter two were not individually reviewed in that pass.
This category is not a blanket deferral of unrelated compiler failures.

Explicitly approved exceptions exposed terminal facts/size/color/row inspection,
stdin interactivity and reads, monotonic milliseconds, build identity, runtime
executable path, current-user lookup, CPU count, PID and UID. Their public
contracts belong in [host/input compatibility](cli-process-host-input.md) and
[presentation compatibility](cli-presentation.md). Do not infer approval for
other adapters from these exceptions.

## Upstream refactoring candidates

| Package / item | Decision and rationale |
| --- | --- |
| `cli/clear.dark`: `Darklang.Cli.Clear.execute` | Defer `Builtin.stdoutClear`. On Linux/macOS it only writes `ESC[2J` followed by `ESC[H`; prefer ordinary Dark printing upstream. Preserve or separately resolve the interpreter's Windows `Console.Clear()` behavior. |
| `cli/telemetry.dark`: `Darklang.Cli.Telemetry.logFilePath` | Defer `Builtin.cliGetLogDir`. Prefer ordinary environment/path code upstream. The interpreter uses `DARK_CONFIG_RUNDIR` plus `logs/`, defaulting to `./logs/`; a supplied run directory must be absolute. Lookup does not create the directory. |

Other Clear and Telemetry declarations remain individually unreviewed by this
review; the approved Telemetry clock adapter does not resolve `logFilePath`.

## Other deferred items

Paths below are relative to interpreter `packages/darklang/`.

| Package / item | Decision and rationale |
| --- | --- |
| `stdlib/localStore.dark`: `Darklang.Stdlib.LocalStore.path` | Defer as SQLite support: `Builtin.localDbPath` obtains the interpreter's current SQLite database path. Other LocalStore items were not individually reviewed. |
| `languageTools/interpreterStats.dark`: `Darklang.LanguageTools.InterpreterStats.reset` | Defer interpreter instrumentation: `Builtin.interpreterStatsReset` resets and enables interpreter VM counters, which compiled programs do not have. Other items were not individually reviewed. |
| `languageTools/packageManager.dark`: `Darklang.LanguageTools.PackageManager.Type.find` | Defer runtime package-manager lookups, including names, locations and other metadata. Interpreter main-store lookup uses SQLite; compile-time hosted imports are a separate feature. Pure helpers remain in scope and other lookups require individual classification. |
| `stdlib/cli/daemon.dark`: `Darklang.Stdlib.Cli.Daemon.launcher`, `launchDetached` | Defer interpreter-based launching. `launchDetached` invokes `nohup <executable> eval '<expression>'`; a compiled application does not provide the interpreter's `eval` command. The executable-path API now exists, but does not resolve this mismatch. Other daemon helpers remain individually unreviewed. |
| `stdlib/cli/tui/terminalSession.dark`: `Darklang.Stdlib.Cli.Tui.TerminalSession.armRestoreGuard`, `disarmRestoreGuard` | Defer emergency restoration. The interpreter records an escape sequence for process exit, unhandled exceptions and Ctrl-C; the compiled runtime lacks the corresponding shutdown/signal guard. Other TerminalSession helpers remain in scope. |

## Unresolved investigation leads

The stdin review reported two independent ownership/allocation problems. Check
current code and reproduce them before treating them as current defects:

- `String.toBlob` crashed on input of 100 KB or more, while reading and printing
  the same input directly succeeded.
- The original String-accumulator secret-input loop underflowed its reference
  count on repeated separate key reads. Standalone `String.dropLast` on runtime
  input passed. Owned text chunks avoid the problem in the compiler's secret
  reader; that workaround did not establish a general ownership fix.

## Review completion

Enumerate every skipped package and unresolved function, test or value with its
reason and the user's decision. A source match, declaration check, or successful
whole-package compile does not establish runtime coverage of every item.
