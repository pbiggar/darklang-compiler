# CLI presentation parity

The explicit printing prelude was revalidated between compiler starting HEAD
`c609b56ce1ec488afc3146c585b6f45a2fcf22a8` and darklang/dark
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. Implementation comparison commit
`ab81ead7f4b232b4ffa181d8ddb71e9381c510c8` was tested against that same exact
interpreter revision. Compiler evidence revision
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3`, DCB1 report commit `8a402797`, and
the previous parity document were discovery aids only. The focused matrix
passed 30/30 at the implementation comparison commit. Performance is outside
this contract unless it changes observable behavior.

The pinned interpreter sources are
`packages/darklang/stdlib/cli/log.dark` and
`packages/darklang/stdlib/cli/ui/{color,progress,prompt,spinner,table}.dark`.
The runtime primitives are in
`backend/src/Builtins/Builtins.Cli/Libs/Output.fs:18-50` and
`Stdin.fs:394-408`. The starting compiler revision had no corresponding
modules; that absent public surface is classified as a parity gap, not an
intentional divergence.

## Presentation contract

All control bytes are unconditional. Redirection does not remove ANSI escapes
or carriage returns, and there is no `isatty` branch, animation timer,
alternate screen, cursor cleanup, or terminal-width calculation.

| Surface | Destination and exact byte shape | Newline | Return and input behavior | Classification |
| --- | --- | --- | --- | --- |
| `Builtin.print(String): Unit` | stdout; input UTF-8 bytes unchanged | none | `Unit`; ordered effect | Shared |
| `Builtin.printLine(String): Unit` | stdout; input UTF-8 bytes followed by `0a` | exactly one LF | `Unit`; ordered effect | Shared |
| `Stdlib.print(String): Unit` | forwards the supplied UTF-8 bytes to `Builtin.print` | none | `Unit`; ordered effect | Shared |
| `Stdlib.printLine(String): Unit` | forwards the supplied UTF-8 bytes to `Builtin.printLine` | exactly one LF | `Unit`; ordered effect | Shared |
| `Stdlib.printLines(List<String>): Unit` | invokes `Stdlib.printLine` once per element, head to tail | one LF per element | empty list has no effect; `Unit` | Shared |
| `Builtin.stdinReadLine(Unit): String` | reads fd 0; accepts LF or CRLF and omits the terminator | none added | preserves unread bytes, returns a partial final line, and returns `""` at immediate EOF | Shared |
| `UI.Color.esc(String, String): String` | `1b 5b`, code, `6d`, text, `1b 5b 30 6d` | none | wraps empty text too; an inner reset remains observable | Shared |
| Color helpers | `red=31`, `green=32`, `yellow=33`, `blue=34`, `magenta=35`, `cyan=36`, `white=37`, `gray=90`, `bold=1`, `dim=2`, `italic=3`, `underline=4`, `strikethrough=9`, `bgRed=41`, `bgGreen=42`, `bgYellow=43`, `bgBlue=44` | none | return the wrapped string | Shared |
| `Log.info/warn/error/debug/success(String): Unit` | stdout only; colored `[INFO]`, `[WARN]`, `[ERROR]`, `[DEBUG]`, or `[OK]`, one space, then the original message | exactly one LF | `Unit`; stderr is untouched | Shared |
| `UI.Progress.bar(Int, Int, String): Unit` | stdout; CR, `[`, `#` fill, `-` fill, `] `, truncated percentage, `% `, label; width is 30 | LF only when `current >= total` | `Unit`; total zero produces zero fill and zero percent; out-of-range arithmetic is not clamped | Shared |
| `UI.Prompt.ask(String): String` | stdout prompt is `question + " "` and is visible before the read | no prompt LF | returns the line; EOF returns `""` | Shared |
| `UI.Prompt.confirm(String): Bool` | stdout prompt is `question + " [y/N] "` | no prompt LF | only case-insensitive `y` or `yes` is true; EOF is false | Shared |
| `UI.Prompt.select(String, List<String>): String` | stdout question plus LF, bold one-based choices plus LF, then `Enter number: ` | retries add exactly one colored error line | surrounding whitespace and leading `+` follow pinned `Int.parse`; invalid numbers use `Please enter a number`, invalid indices use `Invalid selection`; EOF remains the pinned retry loop | Shared |
| `UI.Spinner.run<a>(String, Unit -> a): a` | stdout `message + "... "`, then the function, then green `done` | completion has exactly one LF | invokes once and returns the untouched result; failure or signal emits no fabricated completion | Shared |
| `UI.Table.print(List<String>, List<List<String>>): Unit` | stdout bold header, hyphen separator, and rows; two spaces separate columns | one LF per emitted line | widths are maximum EGC counts; cells are right-padded, short rows use empty cells, surplus cells are ignored | Shared |
| Final `Unit` | no automatic bytes | none | explicit effects remain visible; non-Unit results retain normal rendering | Shared |

The public prelude accepts only String (or List<String>). It never renders an
arbitrary value. Implicit final-result rendering is a separate compiler-driver
stage: non-Unit final results use their typed renderer, while a final Unit is
suppressed. Explicit output bytes are not re-rendered or followed by implicit
Unit text.

Writes use fd 1 and reads use fd 0. Native writes retry `EINTR` and continue
after partial writes. Line reads are UTF-8 byte preserving, retain bytes after a
terminator for the next call, and deterministically map EOF to the behaviors in
the table. Default OS signal termination is retained: bytes already written
remain observable, while no synthetic completion or cleanup sequence is added.

## Intentional phase and syntax differences

The compiler still rejects statically invalid programs during AOT type
checking even where the interpreter would reach an equivalent error during
evaluation. That timing is the intentional phase difference; presentation
functions do not add runtime type dispatch to imitate it.

The packaged implementations are copied into compiler syntax with fully
qualified names, parenthesized comma-separated applications, and explicit
generic arguments where required. Compiler integer literal suffixes such as
`0I`, plus the private Int-to-Int64 adapters used by the compiler's current
List/String internals, are compiler-only syntax and implementation extensions.
They do not change the public `Int` signatures. No other legacy compiler
presentation behavior is retained as public parity.

## Native implementation anchors

The typed intrinsic registry is
`src/DarkCompiler/Stdlib.fs:101-113`. Effect nodes begin at
`ANF.fs:234`, `MIR.fs:97`, and `LIR.fs:122`; intrinsic lowering starts at
`passes/2_AST_to_ANF.fs:157`. Both native implementations are at
`passes/arm64/6_CodeGen.fs:4367-4515` and
`passes/x64/6_CodeGen.fs:2827-2950`. They are separate from the final-result
print instructions and participate in optimization, liveness, allocation, and
IR printing as ordered effects.

The root presentation source is `stdlib/Print.dark:3-15`, loaded immediately
after List at `CompilerLibrary.fs:1133-1136`. Its `printLines` composition uses
the portable ordered recursion at `stdlib/List.dark:441-446`; no native list
traversal was added. The package load order is recorded in `CompilerLibrary.fs` and the
adapted sources are `stdlib/CliColor.dark`, `CliLog.dark`, `CliProgress.dark`,
`CliPrompt.dark`, `CliSpinner.dark`, and `CliTable.dark`. EGC measurement is
routed through `stdlib/String.dark:419`; signed selection parsing is aligned at
`stdlib/Int.dark:405`. Unit suppression is in
`passes/2.6_PrintInsertion.fs`, and the CLI's inherited-stream run path is
`CompilerLibrary.fs:1987` plus `Program.fs:481`. Captured execution remains a
separate test path at `CompilerLibrary.fs:1860`.

## Revision-stamped probes

The executable matrix is
`src/Tests/e2e/interpreter/cli_presentation.e2e`. Public prelude probes at
lines 9-20 cover empty and Unicode strings, embedded/trailing newlines, empty
and multi-element lists, mixed ordered writes, optimized and unoptimized
execution, zero stderr, and final Unit suppression. The fixture uses the explicit
closed-or-bytes stdin model and exact-byte output mode defined in
`TestDSL/E2EFormat.fs:18-27`; comparison occurs without trimming in
`Runners/E2ETestRunner.fs:992-1004`. The 18 pinned upstream color cases are
enabled at `TestRunner.fs:493`.

At the revision pair named above, a redirected same-source ask probe with input
`Ada\n` produced stdout hex
`4e616d653f2022416461220a` (`Name? "Ada"\n`) and empty stderr. A native
same-source interruption probe ran
`Spinner.run "Waiting" (fun () -> Prompt.ask "Value?")` with stdin held open,
then delivered SIGINT after the prompt became readable. It terminated by
signal 2, produced stdout hex
`57616974696e672e2e2e2056616c75653f20` (`Waiting... Value? `), produced no
stderr, and did not print `done`. These redirected bytes are identical to the
pinned package algorithm; the native signal result verifies the documented OS
termination boundary rather than introducing an interpreter-specific cleanup
path.
