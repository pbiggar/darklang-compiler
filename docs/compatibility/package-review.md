# Interpreter package review registry

This registry records decisions and unresolved items from the item-by-item
review of interpreter packages. It does not change compiler behavior or test
selection. Review each new failure with the user before implementing a fix.

Sources: interpreter v0.0.35, commit
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`; compiler main commit
`aeef8bc93a4ff7f21c157625186283301ef7adef`.

## Deferred category: existing compiler implementation, interpreter builtin call

The interpreter package calls a `Builtin.*` entry point, but the compiler has
its own implementation of the corresponding public API. Per user direction,
record the mismatch and skip the affected package for now. Do not add adapters
or move existing implementations merely to make the original wrapper compile.

A source-level inventory identified 497 matching declarations across numeric,
collection, text, encoding, date/time, host, networking, and other modules.
These are review candidates, not 497 confirmed compilation failures or claims
of behavioral equivalence. Classify actual failures as each package is visited.
Entries using the same builtin in both implementations are not in this category.

## Decisions

| Package file | Item | Status | Reason and evidence |
|---|---|---|---|
| `packages/darklang/stdlib/base64.dark` | `Darklang.Stdlib.Base64.decode` | Package skipped; unresolved | Original body calls missing `Builtin.base64Decode`. Direct builtin probe fails with `Unknown function or value 'Builtin.base64Decode'`; compiler `Stdlib.Base64.decode "SGVsbG8="` compiles and runs. User deferred this category. |
| `packages/darklang/stdlib/cli/bash.dark` | `Darklang.Stdlib.Cli.Bash.overwriteBashrc` | Fixed and merged | Canonicalized Env and Cli.FileSystem module names, including FileError, and added qualified Option.Option/Result.Result type fallbacks. Original Bash package compiles. PR #14 merged to main at `aeef8bc93a4ff7f21c157625186283301ef7adef`. |
| `packages/darklang/stdlib/localStore.dark` | `Darklang.Stdlib.LocalStore.path` | Package skipped; missing SQLite support | Calls unavailable `Builtin.localDbPath`, which the interpreter implements using `LibDB.Sqlite.currentDbPath`. User classified this as missing SQLite support and directed deferral. Other LocalStore items remain unreviewed individually. |
| `packages/darklang/languageTools/interpreterStats.dark` | `Darklang.LanguageTools.InterpreterStats.reset` | Package skipped; missing interpreter instrumentation support | Original package compilation fails with `Unknown function or value 'Builtin.interpreterStatsReset'`. Interpreter implementation resets `vm.stats` and enables counting; compiler has no corresponding builtin or interpreter VM counters. User directed recording and skipping this package. Other items remain unreviewed individually. |

The Base64 package also contains `encode` and `urlEncode`, which call
`Builtin.base64Encode` and `Builtin.base64UrlEncode`; compiler implementations
already exist. These remain unreviewed individually because the package is
skipped as a whole. Nothing in this package is marked fixed.

## Most recent fix

`packages/darklang/languageTools/permissions.dark`:
`Darklang.LanguageTools.Permissions.tokens` previously failed with
`Unknown function or value 'walk'`. Local self-recursion detection now traverses
all written expression containers and recognizes function values, respecting
match, lambda and destructured-let shadowing. Recursive inference and closure
lowering also support those forms, including callbacks stored in lists.
Standard-library wrappers now release owned function results.

Branch `chatgpt/local-recursion-match`, commit
`5db9fdf8cd52e3e453ebb3f10817ef2c0dd65244`; PR #16 merged to main at
`df5fbb9079df2415c31ec9704faa792b66b8cbe5`.
Five match regressions and 23 expression/value/ownership regressions pass.
The unchanged tokens function and TokenMode type compile and run in isolation.
Full host suite: 11163/11163 passed. Native `dune runtest` passed after removing
generated read-only action stamps. All 58 quick/full benchmark workloads compile
and run with leak checks clean. Cachegrind equivalence was waived by the user.
Changed OCaml files pass formatting checks. Recursive function values can
allocate a closure; direct recursive calls retain their existing environment.

## Generic function-value fix

`Darklang.LanguageTools.Permissions.parseRule` failed when its HTTP branches
passed generic `Stdlib.List.singleton` directly to `Stdlib.Option.map`.
Fixed on branch `chatgpt/generic-function-values`, commit
`f7db8bb3bee2c455e3c60debd7f6bc8dc50cfc44`, PR #20 merged to main at
`21c0ec108d32cf019e2f4eb1e86cf98ec4d9d4a6`.

Checked function values now retain their type arguments and function type.
Per-reference fresh variables collect contextual and later-use constraints;
type substitution and monomorphization materialize the concrete callback.
Pure generic module function aliases retain the original identity and
specialize at each use. Explicit type arguments without value arguments form
specialized function values.

All 24 focused regressions pass, covering the 17 failing audit probes below,
explicit function-value specialization, ordinary-call controls, alias use at
two types, specialization identity equality, and wrong-argument rejection.
Full host suite: 11386/11386 passed. Native `dune runtest` and changed-file
formatting checks passed. All 58 canonical quick/full benchmark workloads
compile and run with leak checks clean; Cachegrind equivalence remains waived.

The whole unchanged original Permissions package compiles. An entry calling
`Permissions.parseRule ["http", "GET"]` runs with exit 0 and no leaks.
The missing closure-target blocker is resolved; other package declarations
have not been reviewed individually at runtime.

Local value annotations remain rejected, as in the pinned interpreter
v0.0.35: its parser explicitly rejects them and WrittenTypes.ELet has no
annotation field. This is a separate language syntax restriction, not a
remaining generic function-value compilation failure.

## Pre-fix generic function-value audit

Checked 22 focused probes against compiler main `df5fbb9079df2415c31ec9704faa792b66b8cbe5`.
Seventeen generic function-value probes failed compilation:

| Context | Observed result |
|---|---|
| User, qualified user, and library generic callbacks | Missing closure target |
| Callback passed to a user higher-order function | Missing closure target |
| Local alias; alias captured by a local function | Undefined function identity or lambda return inference failure |
| Module value; returned function value | Missing closure target |
| Lists, tuples, and record function fields | Undefined function identity |
| Option and dictionary payloads | Missing closure target |
| Conditional function value | Undefined function identity |
| Same generic callback at Int64 and String | Missing closure target |
| Generic callback inside another generic function body | Missing closure target |
| Callback after an earlier direct call already instantiated the generic | Missing closure target |

Direct generic calls, direct pipe calls, and lambda wrappers compiled and ran
with exit 0 and no leak diagnostics. Two additional probes were rejected for
separate reasons: local value annotations are unsupported; a bare
`identity<Int64>` is treated as a call with missing arguments rather than a
specialized function value. These two are not counted as confirmed instances
of the missing-specialization failure.

The checked function-reference node carries only a function identity.
Specialization discovery, specialization replacement and type substitution all
leave it untouched. The fix must preserve type arguments on function values,
substitute them inside generic bodies, and materialize the concrete targets.
Aliases also need inference from subsequent uses; expected callback types alone
do not cover the tested scope. Expression-container traversal for ordinary
generic calls is already present. No compiler or package changes were made
during that initial coverage investigation; the subsequent fix above resolves
the 17 generic function-value probes and bare explicit specialization.

## Deferred category: package-manager lookups

Per user direction, skip functions that look up items, names, locations or
other information through the package manager. This is not a blanket deferral
of pure helpers in the same package, nor permission to skip unrelated compiler
failures. Retain skipped lookup items in the final unresolved list.

Confirmed item: `packages/darklang/languageTools/packageManager.dark`,
`Darklang.LanguageTools.PackageManager.Type.find`. With its type dependencies
included, compilation fails with `Unknown function or value 'Builtin.pmFindType'`.
The interpreter delegates to its branch-aware package manager; main-store
lookup uses `LibDB.ProgramTypes.Type.find` backed by SQLite. Item skipped,
not fixed. Other lookup functions remain individually unreviewed.

## Deferred: interpreter-based daemon launching

Per user direction, do not implement `Darklang.Stdlib.Cli.Daemon.launcher`
or `launchDetached` yet (`packages/darklang/stdlib/cli/daemon.dark`).
Original package compilation fails at `launcher` with
`Unknown function or value 'Stdlib.Cli.Sys.currentExecutablePath'`.
The compiler's `Cli.Sys` lacks this API; the interpreter implements it through
`Builtin.getCurrentExecutablePath`.

`launchDetached` assumes that executable is the interpreter CLI and invokes
`nohup <executable> eval '<expression>' >> <log> 2>&1 &`. A compiled application's
executable does not provide Dark source evaluation through `eval`, so adding
the path API alone would not make this launcher work. Defer both the missing
API for this work and interpreter-based daemon launching; no fix made.
Other daemon helpers remain individually unreviewed, not blanket-deferred.
Retain these items in the final unresolved list.

## Terminal-support facts

`packages/darklang/stdlib/cli/tui/terminalSupport.dark`:
`Darklang.Stdlib.Cli.Tui.TerminalSupport.currentFacts` failed because
`Builtin.cliTerminalSessionInfo` was missing. Implemented in PR #31 with direct
terminal-attributes `ioctl` syscalls and the existing native environment-vector
lookup for `TERM`, without libc terminal calls or shell commands. The public
package source is embedded unchanged.

Validation: the baseline reproduces the missing builtin; 11 focused E2E cases
pass, including environment mutation and public package behavior. Native checks
cover all four stdin/stdout terminal combinations with unset, empty, normal,
and Unicode `TERM`, plus files, pipes, and closed descriptors; terminal settings
remain unchanged and leak checks are clean. Full host suite: 11441/11441 passed.
`dune runtest`, changed-file formatting checks, and all 58 benchmark compile/run
workloads passed. Runtime validation is Linux x86-64; ARM64 Linux/macOS syscall
lowering is implemented but was not executed in this review.

## Deferred: terminal emergency restoration

Per user direction, defer the emergency restore guard in
`packages/darklang/stdlib/cli/tui/terminalSession.dark`.
`Darklang.Stdlib.Cli.Tui.TerminalSession.armRestoreGuard` fails compilation with
`Unknown function or value 'Builtin.cliTerminalRestoreArm'`. The interpreter
stores an escape sequence and writes it on process exit, unhandled exceptions,
or Ctrl-C. The compiled runtime has no corresponding shutdown/signal guard.
No implementation was added. The companion `disarmRestoreGuard`, which calls
`Builtin.cliTerminalRestoreDisarm`, belongs to the same deferred feature;
it has not been validated separately. Other TerminalSession helpers remain
in scope. Retain these guard functions in the final unresolved list.

## Terminal viewport size

`Darklang.Stdlib.Cli.Tui.TerminalSession.currentSize` failed because
`Darklang.Cli.Terminal.getSize` was absent. Its original wrapper in
`packages/darklang/cli/utils/terminal.dark` called missing
`Builtin.cliTerminalSize`. Both failures were reproduced before implementing
the builtin and embedding the unchanged public `getSize` fragment.

The native implementation queries window size with direct ioctl syscalls,
trying stdout, stdin and stderr in that order, rejecting zero dimensions and
decoding the unsigned 16-bit rows/columns. Without a usable terminal it accepts
positive Int32 `COLUMNS`/`LINES` values, defaulting independently to 80/24.
The interpreter's console-dimension fallback is covered by the native kernel
queries; no libc, shell command or executable is used. On macOS the interpreter
disables its unsafe variadic libc ioctl bridge, whereas native syscall querying
is supported here. Runtime validation is Linux x86-64 only.

Seven E2E cases pass, including public getSize and environment mutation. Native
regressions cover every terminal descriptor combination, descriptor precedence,
kernel dimensions over environment values, zero and unsigned high dimensions,
unset/invalid/overflow/partial environment values, and unchanged terminal
attributes. The unchanged original currentSize function compiles and prints
`132|43` with corresponding environment values, without leaks. Full host suite:
11448/11448 passed; native `dune runtest --cache=disabled` passes after clearing
stale read-only action stamps. All 58 benchmark compile/run workloads pass with
clean leak checks. Cachegrind comparison remains waived by the user. Fix: PR #33.

## Terminal color policy

`Darklang.Cli.Terminal.colorEnabled` in
`packages/darklang/cli/utils/terminal.dark` failed compilation because
`Builtin.cliTerminalColorEnabled` was missing. PR #34 implements the builtin
using the existing native stdout terminal probe and environment-vector lookup,
and embeds the unchanged public wrapper. Color is enabled only when stdout is
a terminal and `NO_COLOR` is unset or empty; every non-empty value disables it,
including `0`, whitespace and Unicode. `TERM` and stdin do not affect this API.

Five focused E2E cases reproduce the baseline failures and pass after the fix.
Native regressions exercise all stdin/stdout terminal combinations, unset,
empty and non-empty NO_COLOR values, environment mutation after startup, file
and closed stdout, unchanged terminal attributes, and clean leak diagnostics.
Full host suite: 11453/11453 passed. Native `dune runtest --cache=disabled` passed
after correcting stale read-only generated action stamps. All 58 benchmark
compile/run workloads passed with clean leak checks. Runtime validation is
Linux x86-64; no new syscall lowering, libc or shell calls were added.

## Terminal row inspection

`Darklang.Stdlib.Cli.Tui.Text.inspect` in
`packages/darklang/stdlib/cli/tui/text.dark` failed compilation because
`Builtin.cliTerminalInspectRow` was missing. PR #35 adds the unchanged public
wrapper and implements the builtin with existing String.displayWidth and
grapheme-aware control detection, including CRLF. The returned width is plain
text width: escape payloads are counted, and the control flag tells callers
to use the sanitizing path. This does not add a new width algorithm.

All 13 focused E2E cases reproduce the missing API on the baseline and pass
after the fix, covering ASCII, CJK, combining marks, emoji/flags/VS16, zero-width
text, C0/C1 controls, CRLF, escapes and non-control format characters. A repeated
Unicode/control row prints `30|true` with clean leak checks. Full host suite:
11466/11466 passed. Native `dune runtest --cache=disabled` passes using a fresh
build directory after stale read-only action stamps blocked the existing one.
All 58 benchmark compile/run workloads passed with clean leak checks;
Cachegrind comparison remains waived. Runtime validation is Linux x86-64.
Other declarations in this package remain individually unreviewed.

## Stdin interactivity

`Stdlib.Cli.Stdin.isInteractive` in
`packages/darklang/stdlib/cli/stdin.dark` failed compilation because
`Builtin.stdinIsInteractive` was missing. PR #38 implements the builtin with
the existing direct syscall terminal probe and adds the public wrapper.
It matches the interpreter: either stdin or stdout being a terminal makes the
process interactive. `TERM` and `NO_COLOR` do not affect this API.

Both focused E2E cases reproduce the missing builtin on the baseline and pass
after the fix. Native regressions check all four stdin/stdout terminal
combinations, files, pipes, both descriptors closed, and each closed descriptor
paired with a terminal; both builtin and public API are checked, with clean
leak diagnostics and unchanged terminal attributes. Full host suite:
11943/11943 passed. `dune runtest` and all 58 benchmark compile/run workloads
passed. Cachegrind comparison remains waived. Runtime validation is Linux
x86-64; no new syscall lowering was required.

## Stdin streams and key input

PR #39 implements `Builtin.stdinReadAll`, `Builtin.stdinReadExactly`, and
`Builtin.stdinReadKey`, plus public `Stdin.readLine`, `readAll`, and `readSecret`.
The existing public `readKey` was a placeholder returning Escape; it now reads
real terminal events. A private decoder uses direct read, ioctl, poll, clock and
signal syscalls. Shared input state occupies 512 bytes beside runtime process
metadata, retaining buffered bytes across line, counted and whole-stream reads.
There is no libc, shell or package-server dependency.

Line reads accept CR, LF and CRLF, preserve remaining input, and return an empty
string at EOF. All/count reads decode UTF-8 with replacement for malformed input;
counted reads use the interpreter's UTF-16 unit count and reject negative or
out-of-range counts. A count splitting an astral character returns replacement
for its isolated surrogate; the remaining surrogate is retained for the next
read and also renders as replacement in native UTF-8 strings. Raw UTF-16 string
identity is not represented by the compiler.

Key reads temporarily disable canonical mode, echo and terminal signal
processing; decode text, common CSI/SS3 keys and modifiers; coalesce repeats and
pastes with 4 ms quiet / 40 ms total bounds; preserve the next differing control
key; and return NoName on a resize while waiting. Redirected stdin returns Escape,
as in the interpreter. Terminal attributes and signal masks are restored.
Secret input uses owned text chunks across calls, preserving Enter, grapheme
Backspace and paste behavior. It tests stdin itself for the redirected fallback;
the original package's isInteractive OR condition would otherwise loop on Escape
when only stdout is a terminal.

Validation checkpoint: 12 focused E2E cases pass. Native regressions pass for
mixed reads, UTF-16 boundaries, malformed UTF-8, EOF, closed input, invalid counts,
200 KB input, real key/modifier/repeat events, resize, paste, grapheme Backspace,
no echo, unchanged terminal attributes and clean leak diagnostics. Final complete
host, Dune and benchmark gates are pending. Runtime validation is Linux x86-64;
Linux ARM64/macOS lowering is implemented but not executed here.

## Remaining observed compiler issues outside stdin

The large-input regression initially crashed in the test's `String.toBlob`
conversion at 100 KB and above. Reading and printing the same input directly
succeeds with clean leak checks. This conversion issue remains to investigate;
it was not changed by the stdin implementation.

The original String-accumulator secret loop returned correct edited text but
underflowed its reference count on repeated separate key reads. Standalone
`String.dropLast` on runtime input passed. The compiler implementation uses owned
chunks to avoid that failure; the original loop's ownership bug remains to
investigate and is not claimed fixed generally.

## Completion rule

At the end of the package review, enumerate every skipped package and unresolved
function, test, or value, retaining the reason and the user's decision. A source
inventory match alone must not be reported as a passing compile or runtime test.
