# Embedded library source inventory

The complete interpreter corpus at the revision in `porting/sources.yaml` lives
under `upstream/`, including all package declarations and both test forms.
Those original files carry per-item compiler decisions. The compiler embeds
all imported package sources and selects original declarations or compatible
replacements before checking bodies. `library-sources.list` lists the compiler
replacement and supplemental implementations in dependency order; the table
below describes that implementation layer and its historical source origins.
See [the import design](../porting/upstream-import.md) for selection and tests.

The existing implementation layer originally targeted interpreter **v0.0.35**, revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460` in `darklang/dark`.
This inventory was checked against that revision, including nested modules.
Paths in the interpreter column are relative to the interpreter repository.
All compiler library paths now follow the interpreter's `packages/` owner and
directory casing. Existing split files and compiler-only helpers remain explicit;
the move preserves their source bytes and declaration order. Env, File, Process,
Sys, and root Cli execution under `packages/darklang/stdlib/cli/` are byte-identical
to the porting pin in `porting/sources.yaml`. Moving files does not advance the
general historical source baseline or establish compatibility with that pin.
Option, Result, Tuple2, Tuple3, Fun, Duration, and Retry are also byte-identical
to the interpreter revision in `porting/sources.yaml`, including their public
documentation. These packages use the compiler's existing native dependencies.
Individual portable functions and types in List, Dict, String, Char, numeric,
HTTP, HTML, Diff, and JSON modules also track that pin; engine-specific adapters
remain native. Sqlite contains only the shared cell type and pure accessors,
not a database adapter. Nested public fragments retain upstream module nesting
and declaration text. The declaration ledger records exact per-item identity.
The native implementations of `List.uniqueBy`, `List.dedup`,
`List.groupByWithKey`, and `String.endsWith` remain in place;
their upstream replacements did not pass execution/resource checks.
The compatibility ledgers own API coverage and behavioral differences; this
inventory records source placement and origin, not complete API parity.

`packages/darklang/stdlib/` contains `Darklang.Stdlib`. Other packages live under
`packages/` with the interpreter's owner and package paths. See
[source organization](source-organization.md#standard-library-layout).
The rows follow `library-sources.list` declaration order.

The kernel-library migration is described in [kernel-stdlib.md](kernel-stdlib.md).
Its shared CLI and clock algorithms instead track interpreter PR #5907 at
`26cab9978740a7f9c9d56ea643f3fade813f079e`; the general compatibility pin above
still applies to other packages. Check this subset with
`python3 scripts/sync-kernel-stdlib.py INTERPRETER_CHECKOUT --check`.
Its native Builtin adapters under `packages/darklang/stdlib/__builtin/` are
`__syscall.dark`, `__runtime.dark`, `__kernelRecords.dark`, `__fileSyscall.dark`,
`__pathSyscall.dark`, `__systemSyscall.dark`, `__clockSyscall.dark`,
`__watchSyscall.dark`, and `__childRuntime.dark`; shared Dark
algorithms are checked by `scripts/sync-kernel-stdlib.py`.

Shared algorithms are private `__` declarations in the existing File, Process,
Posix, Sys and DateTime modules. Syscall and runtime Builtins are called directly;
there are no public forwarding packages for them. DateTime's clock block is
shared while each engine retains its own DateTime representation implementation.

Trait declarations and generic utilities in `packages/darklang/stdlib/traits.dark` are unchanged
from interpreter `a9d93efc853486244630dad693fe79f6f4882cf6`. Trait implementations
live beside each type's existing functions, as upstream does. Their methods
alias native compiler operations rather than duplicate the interpreter's F#
builtin adapters. See [trait compatibility](../compatibility/language/traits.md)
for static selection and hosted-package limitations.

A `__` filename marks compiler support or a private implementation fragment.
It does not rename the declared module or enforce language visibility: that
is controlled by declaration names and the frontend's internal-mode checks.
For example, the compiler's `Cli.__Args` helpers are an extension,
while `packages/darklang/stdlib/__builtin.dark` bridges real interpreter builtins implemented in F#.
Public fragments such as `http/request.dark`, `cli/os.dark`, and
`list/sortByComparatorHelpers.dark` retain normal filenames because their
nested modules exist in the pinned interpreter. Private terminal-text
fragments use `__` even where the interpreter also has private helpers.

| Compiler file | Declared package | Interpreter source or compiler role |
|---|---|---|
| `packages/darklang/stdlib/__types.dark` | (root declarations) | Compiler representation of the interpreter’s root Uuid type (`backend/src/LibExecution/RuntimeTypes.fs`) |
| `packages/darklang/stdlib/noModule.dark` | `Darklang.Stdlib`  | `packages/darklang/stdlib/noModule.dark` |
| `packages/darklang/stdlib/traits.dark` | `Darklang.Stdlib` | Unchanged `packages/darklang/stdlib/traits.dark` at `a9d93efc853486244630dad693fe79f6f4882cf6` |
| `packages/darklang/stdlib/int8.dark` | `Darklang.Stdlib.Int8`  | `packages/darklang/stdlib/int8.dark` |
| `packages/darklang/stdlib/int16.dark` | `Darklang.Stdlib.Int16`  | `packages/darklang/stdlib/int16.dark` |
| `packages/darklang/stdlib/int32.dark` | `Darklang.Stdlib.Int32`  | `packages/darklang/stdlib/int32.dark` |
| `packages/darklang/stdlib/int64.dark` | `Darklang.Stdlib.Int64`  | `packages/darklang/stdlib/int64.dark` |
| `packages/darklang/stdlib/int/__integer.dark` | `Darklang.Stdlib.Int`  | Compiler arbitrary-precision integer representation; package defined in `packages/darklang/stdlib/int.dark` |
| `packages/darklang/stdlib/int.dark` | `Darklang.Stdlib.Int`  | `packages/darklang/stdlib/int.dark` |
| `packages/darklang/stdlib/int128.dark` | `Darklang.Stdlib.Int128`  | `packages/darklang/stdlib/int128.dark` |
| `packages/darklang/stdlib/uint8.dark` | `Darklang.Stdlib.UInt8`  | `packages/darklang/stdlib/uint8.dark` |
| `packages/darklang/stdlib/uint16.dark` | `Darklang.Stdlib.UInt16`  | `packages/darklang/stdlib/uint16.dark` |
| `packages/darklang/stdlib/uint32.dark` | `Darklang.Stdlib.UInt32`  | `packages/darklang/stdlib/uint32.dark` |
| `packages/darklang/stdlib/uint64.dark` | `Darklang.Stdlib.UInt64`  | `packages/darklang/stdlib/uint64.dark` |
| `packages/darklang/stdlib/uint128.dark` | `Darklang.Stdlib.UInt128`  | `packages/darklang/stdlib/uint128.dark` |
| `packages/darklang/stdlib/bool.dark` | `Darklang.Stdlib.Bool`  | `packages/darklang/stdlib/bool.dark` |
| `packages/darklang/stdlib/cli/posix/modes.dark` | `Darklang.Stdlib.Cli.Posix.Modes`  | `packages/darklang/stdlib/cli/posix.dark` |
| `packages/darklang/stdlib/cli/posix/errno.dark` | `Darklang.Stdlib.Cli.Posix.Errno`  | `packages/darklang/stdlib/cli/posix.dark` |
| `packages/darklang/stdlib/cli/posix/statMode.dark` | `Darklang.Stdlib.Cli.Posix.StatMode`  | `packages/darklang/stdlib/cli/posix.dark` |
| `packages/darklang/stdlib/cli/fileSystem.dark` | `Darklang.Stdlib.Cli.FileSystem`  | `packages/darklang/stdlib/cli/fileSystem.dark` |
| `packages/darklang/stdlib/cli/fileSystem/fileError.dark` | `Darklang.Stdlib.Cli.FileSystem.FileError`  | `packages/darklang/stdlib/cli/fileSystem.dark` |
| `packages/darklang/stdlib/env.dark` | `Darklang.Stdlib.Env`  | `packages/darklang/stdlib/env.dark` |
| `packages/darklang/stdlib/__builtin.dark` | `Builtin`  | Portable interpreter builtin bridges plus native adapter helpers (`backend/src/Builtins/Builtins.Pure/Libs/{UInt64,String}.fs` and `backend/src/Builtins/Builtins.Cli/Libs/{Directory,File,Environment}.fs`) |
| `packages/darklang/stdlib/__builtin/__posix.dark` | `Builtin` | Interpreter POSIX builtin bridge (`backend/src/Builtins/Builtins.Cli/Libs/Posix.fs`) |
| `packages/darklang/stdlib/__builtin/__terminal.dark` | `Builtin` | Native terminal facts, size, color policy, text inspection and stdin interactivity (`backend/src/Builtins/Builtins.Cli/Libs/{Terminal,Stdin}.fs`) |
| `packages/darklang/stdlib/__builtin/__stdin.dark` | `Builtin` | Interpreter stdin builtin wrappers (`backend/src/Builtins/Builtins.Cli/Libs/Stdin.fs`) |
| `packages/darklang/stdlib/__builtin/__clockSyscall.dark` | `Builtin` | One-shot clock/sleep kernel adapters and raw event-loop runtime inputs |
| `packages/darklang/stdlib/__builtin/__watchSyscall.dark` | `Builtin` | Single-call event queue registration and pollfd ABI adaptation |
| `packages/darklang/stdlib/__builtin/__childRuntime.dark` | `Builtin` | Owned argv launch, supervision and opaque child byte cells |
| `packages/darklang/stdlib/__builtin/__buildInfo.dark` | `Builtin` | Compiler revision substituted at build time, corresponding to interpreter `getBuildHash` (`backend/src/Builtins/Builtins.Cli/Libs/Environment.fs`) |
| `packages/darklang/stdlib/__builtin/__executable.dark` | `Builtin` | Runtime executable path via Linux readlinkat or Darwin proc_info, with captured AT_EXECFN/argv/cwd/PATH fallbacks (`backend/src/Builtins/Builtins.Cli/Libs/Directory.fs`) |
| `packages/darklang/stdlib/tuple2.dark` | `Darklang.Stdlib.Tuple2`  | `packages/darklang/stdlib/tuple2.dark` |
| `packages/darklang/stdlib/tuple3.dark` | `Darklang.Stdlib.Tuple3`  | `packages/darklang/stdlib/tuple3.dark` |
| `packages/darklang/stdlib/result.dark` | `Darklang.Stdlib.Result`  | `packages/darklang/stdlib/result.dark` |
| `packages/darklang/stdlib/option.dark` | `Darklang.Stdlib.Option`  | `packages/darklang/stdlib/option.dark` |
| `packages/darklang/stdlib/list/sortByComparatorHelpers.dark` | `Darklang.Stdlib.List.SortByComparatorHelpers`  | `packages/darklang/stdlib/list.dark` |
| `packages/darklang/stdlib/list.dark` | `Darklang.Stdlib.List`  | `packages/darklang/stdlib/list.dark` |
| `packages/darklang/stdlib/__print.dark` | `Darklang.Stdlib`  | `packages/darklang/stdlib/print.dark` |
| `packages/darklang/stdlib/fun.dark` | `Darklang.Stdlib.Fun`  | `packages/darklang/stdlib/fun.dark` |
| `packages/darklang/stdlib/float.dark` | `Darklang.Stdlib.Float`  | `packages/darklang/stdlib/float.dark` |
| `packages/darklang/stdlib/cli/__posix.dark` | `Darklang.Stdlib.Cli.__Posix` | Private native syscall boundary for interpreter POSIX operations |
| `packages/darklang/stdlib/cli/posix.dark` | `Darklang.Stdlib.Cli.Posix`  | `packages/darklang/stdlib/cli/posix.dark` |
| `packages/darklang/stdlib/cli/posix/__error.dark` | `Darklang.Stdlib.Cli.Posix` | Private conversion from the existing native CLI error record |
| `packages/darklang/stdlib/cli/posix/openFlags.dark` | `Darklang.Stdlib.Cli.Posix.OpenFlags` | `packages/darklang/stdlib/cli/posix.dark` |
| `packages/darklang/stdlib/retry.dark` | `Darklang.Stdlib.Retry`  | `packages/darklang/stdlib/retry.dark` |
| `packages/darklang/stdlib/cli/path.dark` | `Darklang.Stdlib.Cli.Path`  | `packages/darklang/stdlib/cli/path.dark` |
| `packages/darklang/stdlib/cli/file.dark` | `Darklang.Stdlib.Cli.File`  | `packages/darklang/stdlib/cli/file.dark` (unchanged copy at the porting pin) |
| `packages/darklang/stdlib/cli/dir.dark` | `Darklang.Stdlib.Cli.Dir` | `packages/darklang/stdlib/cli/dir.dark` |
| `packages/darklang/stdlib/string/__unicode/__data.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index00.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index01.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index02.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index03.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index04.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index05.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__index06.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table00.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table00`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table01.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table01`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table02.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table02`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table03.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table03`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table04.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table04`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table05.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table05`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table06.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table06`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table07.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table07`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table08.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table08`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table09.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table09`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table10.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table10`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table11.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table11`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table12.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table12`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table13.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table13`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table14.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table14`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table15.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table15`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table16.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table16`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table17.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table17`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table18.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table18`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table19.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table19`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table20.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table20`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table21.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table21`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table22.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table22`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table23.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table23`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table24.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table24`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table25.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table25`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table26.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table26`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table27.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table27`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table28.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table28`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table29.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table29`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table30.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table30`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table31.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table31`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table32.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table32`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table33.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table33`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table34.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table34`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table35.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table35`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table36.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table36`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table37.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table37`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table38.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table38`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table39.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table39`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table40.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table40`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table41.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table41`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table42.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table42`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table43.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table43`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table44.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table44`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table45.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table45`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table46.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table46`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table47.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table47`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table48.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table48`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table49.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table49`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table50.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table50`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table51.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table51`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table52.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table52`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table53.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table53`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table54.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table54`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table55.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table55`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table56.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table56`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table57.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table57`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table58.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table58`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table59.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table59`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table60.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table60`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table61.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table61`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table62.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table62`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode/__data/__table63.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table63`  | Generated compiler Unicode lookup data |
| `packages/darklang/stdlib/string/__unicode.dark` | `Darklang.Stdlib.String.__Unicode`  | Compiler Unicode normalization, casing and segmentation |
| `packages/darklang/stdlib/string.dark` | `Darklang.Stdlib.String`  | `packages/darklang/stdlib/string.dark` |
| `packages/darklang/stdlib/__hash.dark` | (root declarations) | Compiler hashing helpers |
| `packages/darklang/stdlib/dict.dark` | `Darklang.Stdlib.Dict`  | `packages/darklang/stdlib/dict.dark` |
| `packages/darklang/stdlib/sqlite.dark` | `Darklang.Stdlib.Sqlite` | `packages/darklang/stdlib/sqlite.dark`: Value and six pure accessors only; database operations remain unsupported |
| `packages/darklang/stdlib/dict/__hamt.dark` | `Darklang.Stdlib.Dict`  | Compiler dictionary representation; package defined in `packages/darklang/stdlib/dict.dark` |
| `packages/darklang/stdlib/uuid.dark` | `Darklang.Stdlib.Uuid`  | `packages/darklang/stdlib/uuid.dark` |
| `packages/darklang/stdlib/diff.dark` | `Darklang.Stdlib.Diff`  | `packages/darklang/stdlib/diff.dark` |
| `packages/darklang/languageTools/programTypes.dark` | `Darklang.LanguageTools.ProgramTypes`  | `packages/darklang/languageTools/programTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/base.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/fqTypeName.dark` | `Darklang.LanguageTools.RuntimeTypes.FQTypeName`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/fqFnName.dark` | `Darklang.LanguageTools.RuntimeTypes.FQFnName`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/fqValueName.dark` | `Darklang.LanguageTools.RuntimeTypes.FQValueName`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/typeReference.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/prettyPrinter/runtimeTypes.dark` | `Darklang.PrettyPrinter.RuntimeTypes`, `Dval`  | `packages/darklang/prettyPrinter/runtimeError.dark`, `packages/darklang/prettyPrinter/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/valueType.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/runtimeTypes/dval.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/prettyPrinter/runtimeTypes/runtimeError.dark` | `Darklang.PrettyPrinter.RuntimeTypes.RuntimeError`  | `packages/darklang/prettyPrinter/runtimeError.dark` |
| `packages/darklang/languageTools/runtimeTypes/__valueTypeSupport.dark` | `Darklang.LanguageTools.RuntimeTypes`  | Compiler custom-type catalog matching helper; package defined in `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/darklang/languageTools/packageManager.dark` | `Darklang.LanguageTools.PackageManager`  | `packages/darklang/languageTools/packageManager.dark` |
| `packages/darklang/languageTools/packageManager/pickContext.dark` | `Darklang.LanguageTools.PackageManager.PickContext`  | `packages/darklang/languageTools/packageManager.dark` |
| `packages/darklang/scm/branch.dark` | `Darklang.SCM.Branch`  | `packages/darklang/scm/branch.dark` |
| `packages/darklang/stdlib/valueSearch.dark` | `Darklang.Stdlib.ValueSearch`  | `packages/darklang/stdlib/valueSearch.dark` |
| `packages/darklang/stdlib/dateTime.dark` | `Darklang.Stdlib.DateTime`  | `packages/darklang/stdlib/dateTime.dark` |
| `packages/darklang/stdlib/duration.dark` | `Darklang.Stdlib.Duration`  | `packages/darklang/stdlib/duration.dark` |
| `packages/darklang/stdlib/blob.dark` | `Darklang.Stdlib.Blob`  | `packages/darklang/stdlib/blob.dark` |
| `packages/darklang/stdlib/stream.dark` | `Darklang.Stdlib.Stream`  | `packages/darklang/stdlib/stream.dark` |
| `packages/darklang/stdlib/__async.dark` | `Darklang.Stdlib.__Async` | Compiler support: typed native promise cells, reference-counted lifecycle ownership, and waiter handoff |
| `packages/darklang/stdlib/__task.dark` | `Darklang.Stdlib.__Task` | Compiler support: private cooperative task scheduler, promise forcing, timers and nonblocking socket producers |
| `packages/darklang/stdlib/html.dark` | `Darklang.Stdlib.Html`  | `packages/darklang/stdlib/html.dark` |
| `packages/darklang/stdlib/http.dark` | `Darklang.Stdlib.Http`  | `packages/darklang/stdlib/http.dark` |
| `packages/darklang/stdlib/http/request.dark` | `Darklang.Stdlib.Http.Request`  | `packages/darklang/stdlib/http.dark` |
| `packages/darklang/stdlib/httpclient.dark` | `Darklang.Stdlib.HttpClient`  | `packages/darklang/stdlib/httpclient.dark` |
| `packages/darklang/stdlib/httpClient/contentType.dark` | `Darklang.Stdlib.HttpClient.ContentType`  | `packages/darklang/stdlib/httpclient.dark` |
| `packages/darklang/stdlib/httpClient/sse.dark` | `Darklang.Stdlib.HttpClient.Sse`  | `packages/darklang/stdlib/httpclient.dark` |
| `packages/darklang/stdlib/httpServer/config.dark` | `Darklang.Stdlib.HttpServer.Config`  | `packages/darklang/stdlib/httpserver.dark` |
| `packages/darklang/stdlib/httpserver.dark` | `Darklang.Stdlib.HttpServer`  | `packages/darklang/stdlib/httpserver.dark` |
| `packages/darklang/stdlib/__http1Server.dark` | `Darklang.Stdlib.__Http1Server` | Compiler support: bounded persistent HTTP/1.1 server owners and fair plaintext I/O |
| `packages/darklang/stdlib/__network.dark` | `Darklang.Stdlib.__Network`  | Compiler support: Owned native TCP/UDP sockets, listeners, deadlines, and scoped shutdown signals |
| `packages/darklang/stdlib/__datagram.dark` | `Darklang.Stdlib.__Datagram` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__httpWire.dark` | `Darklang.Stdlib.__HttpWire`  | Compiler support: Byte-bounded HTTP/1.1 and URL primitives written in Dark |
| `packages/darklang/stdlib/__dnsWire.dark` | `Darklang.Stdlib.__DnsWire`  | Compiler support: Bounded DNS query and answer wire format implemented in Dark |
| `packages/darklang/stdlib/__httpConnect.dark` | `Darklang.Stdlib.__HttpConnect`  | Compiler support: Check resolved IP bytes before opening HTTP transport |
| `packages/darklang/stdlib/__http2Wire.dark` | `Darklang.Stdlib.__Http2Wire` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__hpack.dark` | `Darklang.Stdlib.__Hpack` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__tlsAlpn.dark` | `Darklang.Stdlib.__TlsAlpn` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__httpTransport.dark` | `Darklang.Stdlib.__HttpTransport` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http2Fields.dark` | `Darklang.Stdlib.__Http2Fields` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http2.dark` | `Darklang.Stdlib.__Http2` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http2Connection.dark` | `Darklang.Stdlib.__Http2Connection` | Compiler support: reusable HTTP/2 state and bounded stream scheduling |
| `packages/darklang/stdlib/__http2Server.dark` | `Darklang.Stdlib.__Http2Server` | Compiler support: shared HTTP/2 server state, stream windows and scoped failures |
| `packages/darklang/stdlib/__quicWire.dark` | `Darklang.Stdlib.__QuicWire` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicCrypto.dark` | `Darklang.Stdlib.__QuicCrypto` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicPacket.dark` | `Darklang.Stdlib.__QuicPacket` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicFrames.dark` | `Darklang.Stdlib.__QuicFrames` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicAck.dark` | `Darklang.Stdlib.__QuicAck` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicApplication.dark` | `Darklang.Stdlib.__QuicApplication` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicReassembly.dark` | `Darklang.Stdlib.__QuicReassembly` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicStreams.dark` | `Darklang.Stdlib.__QuicStreams` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicFlow.dark` | `Darklang.Stdlib.__QuicFlow` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicRecovery.dark` | `Darklang.Stdlib.__QuicRecovery` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicSpace.dark` | `Darklang.Stdlib.__QuicSpace` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicHandshakePacket.dark` | `Darklang.Stdlib.__QuicHandshakePacket` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicIdle.dark` | `Darklang.Stdlib.__QuicIdle` | Compiler support: negotiated QUIC application idle lifetime |
| `packages/darklang/stdlib/__quicParameters.dark` | `Darklang.Stdlib.__QuicParameters` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicTls.dark` | `Darklang.Stdlib.__QuicTls` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicKeys.dark` | `Darklang.Stdlib.__QuicKeys` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicClient.dark` | `Darklang.Stdlib.__QuicClient` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicServer.dark` | `Darklang.Stdlib.__QuicServer` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicConnection.dark` | `Darklang.Stdlib.__QuicConnection` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__qpack.dark` | `Darklang.Stdlib.__Qpack` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__qpackDecoder.dark` | `Darklang.Stdlib.__QpackDecoder` | Compiler support: bounded dynamic QPACK decoding and encoder-stream instructions |
| `packages/darklang/stdlib/__qpackEncoder.dark` | `Darklang.Stdlib.__QpackEncoder` | Compiler support: bounded outgoing dynamic QPACK, reference pins and decoder feedback |
| `packages/darklang/stdlib/__http3Wire.dark` | `Darklang.Stdlib.__Http3Wire` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3Stream.dark` | `Darklang.Stdlib.__Http3Stream` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3Message.dark` | `Darklang.Stdlib.__Http3Message` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3Control.dark` | `Darklang.Stdlib.__Http3Control` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3Uni.dark` | `Darklang.Stdlib.__Http3Uni` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3.dark` | `Darklang.Stdlib.__Http3` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3Client.dark` | `Darklang.Stdlib.__Http3Client` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http3Connection.dark` | `Darklang.Stdlib.__Http3Connection` | Compiler support: shared QUIC/QPACK request scheduler |
| `packages/darklang/stdlib/__http3Server.dark` | `Darklang.Stdlib.__Http3Server` | Compiler support: bounded multiplexed HTTP/3 server request owners |
| `packages/darklang/stdlib/__http3Discovery.dark` | `Darklang.Stdlib.__Http3Discovery` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__altSvc.dark` | `Darklang.Stdlib.__AltSvc` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__altSvcCache.dark` | `Darklang.Stdlib.__AltSvcCache` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__http1Pool.dark` | `Darklang.Stdlib.__Http1Pool` | Compiler support: bounded HTTP/1.1 session connection ownership and response reuse |
| `packages/darklang/stdlib/__http2Pool.dark` | `Darklang.Stdlib.__Http2Pool` | Compiler support: bounded persistent HTTP/2 session owners |
| `packages/darklang/stdlib/__http3Pool.dark` | `Darklang.Stdlib.__Http3Pool` | Compiler support: bounded persistent HTTP/3 session owners |
| `packages/darklang/stdlib/__httpSessionBatch.dark` | `Darklang.Stdlib.__HttpSessionBatch` | Compiler support: session batch protocol selection and owner checkout |
| `packages/darklang/stdlib/httpClientSession.dark` | `Darklang.Stdlib.HttpClientSession` | Compiler extension: HTTP transport and secure server support |
| `packages/darklang/stdlib/__httpsService.dark` | `Darklang.Stdlib.__HttpsService` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__httpsDns.dark` | `Darklang.Stdlib.__HttpsDns` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/pretty.dark` | `Darklang.Stdlib.Pretty`  | `packages/darklang/stdlib/pretty.dark` |
| `packages/darklang/stdlib/char.dark` | `Darklang.Stdlib.Char`  | `packages/darklang/stdlib/char.dark` |
| `packages/darklang/stdlib/regex.dark` | `Darklang.Stdlib.Regex`  | `packages/darklang/stdlib/regex.dark` |
| `packages/darklang/stdlib/base64.dark` | `Darklang.Stdlib.Base64`  | `packages/darklang/stdlib/base64.dark` |
| `packages/darklang/stdlib/x509.dark` | `Darklang.Stdlib.X509`  | `packages/darklang/stdlib/x509.dark` |
| `packages/darklang/stdlib/__x509Identity.dark` | `Darklang.Stdlib.__X509Identity`  | Compiler support: bounded DER leaf identity and DNS hostname checks |
| `packages/darklang/stdlib/__rsaSpki.dark` | `Darklang.Stdlib.__RsaSpki`  | Compiler support: strict DER rsaEncryption SubjectPublicKeyInfo parsing |
| `packages/darklang/stdlib/crypto.dark` | `Darklang.Stdlib.Crypto`  | `packages/darklang/stdlib/crypto.dark` |
| `packages/darklang/stdlib/__rsaPss.dark` | `Darklang.Stdlib.__RsaPss`  | Compiler support: RSA-PSS/SHA-256 public signature verification for TLS 1.3 |
| `packages/darklang/stdlib/__rsaMontgomery.dark` | `Darklang.Stdlib.__RsaMontgomery` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__rsaPrivate.dark` | `Darklang.Stdlib.__RsaPrivate` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__rsaSigning.dark` | `Darklang.Stdlib.__RsaSigning` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/tlsServerIdentity.dark` | `Darklang.Stdlib.TlsServerIdentity` | Compiler extension: HTTP transport and secure server support |
| `packages/darklang/stdlib/__tls13ServerHello.dark` | `Darklang.Stdlib.__Tls13ServerHello` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__tls13ServerRetry.dark` | `Darklang.Stdlib.__Tls13ServerRetry` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__tls13ServerHandshake.dark` | `Darklang.Stdlib.__Tls13ServerHandshake` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__tls13KeyUpdate.dark` | `Darklang.Stdlib.__Tls13KeyUpdate` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__tls13ServerTcp.dark` | `Darklang.Stdlib.__Tls13ServerTcp` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__tls13ServerPoll.dark` | `Darklang.Stdlib.__Tls13ServerPoll` | Compiler support: incremental nonblocking TLS server handshake ownership |
| `packages/darklang/stdlib/__tlsServer.dark` | `Darklang.Stdlib.__TlsServer` | Compiler support: fair TLS handshake and persistent HTTP/1.1–2 transport owners |
| `packages/darklang/stdlib/__quicServerTls.dark` | `Darklang.Stdlib.__QuicServerTls` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/__quicServerRetry.dark` | `Darklang.Stdlib.__QuicServerRetry` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `packages/darklang/stdlib/httpServer/tls.dark` | `Darklang.Stdlib.HttpServer.Tls` | Compiler extension: HTTP transport and secure server support |
| `packages/darklang/stdlib/httpServer/quic.dark` | `Darklang.Stdlib.HttpServer.Quic` | Compiler extension: HTTP transport and secure server support |
| `packages/darklang/stdlib/httpServer/secure.dark` | `Darklang.Stdlib.HttpServer.Secure` | Compiler extension: HTTP transport and secure server support |
| `packages/darklang/stdlib/__p256.dark` | `Darklang.Stdlib.__P256`  | Compiler support: bounded P-256 ECDSA/SHA-256 verification for TLS signatures |
| `packages/darklang/stdlib/__rsaPkcs1.dark` | `Darklang.Stdlib.__RsaPkcs1`  | Compiler support: RSA PKCS#1 v1.5 SHA-256/384 certificate signature checks |
| `packages/darklang/stdlib/__x509Chain.dark` | `Darklang.Stdlib.__X509Chain`  | Compiler support: bounded X.509 path checks before HTTPS identity is trusted |
| `packages/darklang/stdlib/__aesGcm.dark` | `Darklang.Stdlib.__AesGcm`  | Compiler support: AES-128/256 and GCM authenticated encryption for TLS records |
| `packages/darklang/stdlib/__chacha20Poly1305.dark` | `Darklang.Stdlib.__Chacha20Poly1305`  | Compiler support: RFC 8439 ChaCha20 and Poly1305 authenticated encryption |
| `packages/darklang/stdlib/__tls13.dark` | `Darklang.Stdlib.__Tls13`  | Compiler support: TLS 1.3 key derivation and bounded record framing |
| `packages/darklang/stdlib/__tls13Handshake.dark` | `Darklang.Stdlib.__Tls13Handshake`  | Compiler support: TLS 1.3 handshake secrets, server Finished, and bounded reassembly |
| `packages/darklang/stdlib/__tls13Certificate.dark` | `Darklang.Stdlib.__Tls13Certificate`  | Compiler support: bounded TLS 1.3 certificate flight framing |
| `packages/darklang/stdlib/__tls13Client.dark` | `Darklang.Stdlib.__Tls13Client`  | Compiler support: authenticated TLS 1.3 exchange over the Dark TCP transport |
| `packages/darklang/stdlib/math.dark` | `Darklang.Stdlib.Math`  | `packages/darklang/stdlib/math.dark` |
| `packages/darklang/stdlib/__x25519.dark` | `Darklang.Stdlib.__X25519`  | Compiler support: RFC 7748 Montgomery ladder over fixed 16-bit field limbs |
| `packages/darklang/stdlib/list/__skewList.dark` | `Darklang.Stdlib.List`  | Compiler persistent-list representation; package defined in `packages/darklang/stdlib/list.dark` |
| `packages/darklang/stdlib/list/__listArray.dark` | `Darklang.Stdlib.List`  | Compiler contiguous-list operations; package defined in `packages/darklang/stdlib/list.dark` |
| `packages/darklang/stdlib/cli/ui/colors.dark` | `Darklang.Stdlib.Cli.UI.Colors`  | `packages/darklang/stdlib/cli/ui/colors.dark` |
| `packages/darklang/stdlib/cli/ui/textfield.dark` | `Darklang.Stdlib.Cli.UI.TextField`  | `packages/darklang/stdlib/cli/ui/textfield.dark` |
| `packages/darklang/stdlib/cli/tui/terminalSession/ansi.dark` | `Darklang.Stdlib.Cli.Tui.TerminalSession.Ansi`  | `packages/darklang/stdlib/cli/tui/terminalSession.dark` |
| `packages/darklang/stdlib/cli/tui/terminalSupport.dark` | `Darklang.Stdlib.Cli.Tui.TerminalSupport` | `packages/darklang/stdlib/cli/tui/terminalSupport.dark` |
| `packages/darklang/cli/terminal/size.dark` | `Darklang.Cli.Terminal` | Public getSize fragment of `packages/darklang/cli/utils/terminal.dark` |
| `packages/darklang/cli/terminal/color.dark` | `Darklang.Cli.Terminal` | Public colorEnabled fragment of `packages/darklang/cli/utils/terminal.dark` |
| `packages/darklang/stdlib/cli/tui/text/__escape.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | Private escape-scanning fragment of Cli.Tui.Text; package defined in `packages/darklang/stdlib/cli/tui/text.dark` |
| `packages/darklang/stdlib/cli/tui/text/__width.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | Private width fragment of Cli.Tui.Text; package defined in `packages/darklang/stdlib/cli/tui/text.dark` |
| `packages/darklang/stdlib/cli/tui/text/__clip.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | Private clipping fragment of Cli.Tui.Text; package defined in `packages/darklang/stdlib/cli/tui/text.dark` |
| `packages/darklang/stdlib/cli/tui/text.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | `packages/darklang/stdlib/cli/tui/text.dark` |
| `packages/darklang/stdlib/cli/log.dark` | `Darklang.Stdlib.Cli.Log`  | `packages/darklang/stdlib/cli/log.dark` |
| `packages/darklang/stdlib/cli/ui/progress.dark` | `Darklang.Stdlib.Cli.UI.Progress`  | `packages/darklang/stdlib/cli/ui/progress.dark` |
| `packages/darklang/stdlib/cli/ui/prompt.dark` | `Darklang.Stdlib.Cli.UI.Prompt`  | `packages/darklang/stdlib/cli/ui/prompt.dark` |
| `packages/darklang/stdlib/cli/ui/spinner.dark` | `Darklang.Stdlib.Cli.UI.Spinner`  | `packages/darklang/stdlib/cli/ui/spinner.dark` |
| `packages/darklang/stdlib/cli/ui/table.dark` | `Darklang.Stdlib.Cli.UI.Table`  | `packages/darklang/stdlib/cli/ui/table.dark` |
| `packages/darklang/stdlib/cli/execution.dark` | `Darklang.Stdlib.Cli`  | `packages/darklang/stdlib/cli/execution.dark` (unchanged copy at the porting pin) |
| `packages/darklang/stdlib/cli/os.dark` | `Darklang.Stdlib.Cli.OS`  | `packages/darklang/stdlib/cli/host.dark` |
| `packages/darklang/stdlib/cli/architecture.dark` | `Darklang.Stdlib.Cli.Architecture`  | `packages/darklang/stdlib/cli/host.dark` |
| `packages/darklang/stdlib/cli/shell.dark` | `Darklang.Stdlib.Cli.Shell`  | `packages/darklang/stdlib/cli/host.dark` |
| `packages/darklang/stdlib/cli/host.dark` | `Darklang.Stdlib.Cli.Host`  | `packages/darklang/stdlib/cli/host.dark` |
| `packages/darklang/stdlib/cli/env.dark` | `Darklang.Stdlib.Cli.Env`  | `packages/darklang/stdlib/cli/env.dark` (unchanged copy at the porting pin) |
| `packages/darklang/stdlib/cli/__args.dark` | `Darklang.Stdlib.Cli.__Args`  | Compiler positional CLI argument helpers |
| `packages/darklang/stdlib/cli/process.dark` | `Darklang.Stdlib.Cli.Process`  | `packages/darklang/stdlib/cli/process.dark` (unchanged copy at the porting pin) |
| `packages/darklang/stdlib/cli/__nativeRuntime.dark` | `Darklang.Stdlib.Cli` | Private native startup/environment/kernel prototypes |
| `packages/darklang/stdlib/cli/sys.dark` | `Darklang.Stdlib.Cli.Sys`  | `packages/darklang/stdlib/cli/sys.dark` (unchanged copy at the porting pin) |
| `packages/darklang/stdlib/cli/stdin/key.dark` | `Darklang.Stdlib.Cli.Stdin.Key`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `packages/darklang/stdlib/cli/stdin/modifiers.dark` | `Darklang.Stdlib.Cli.Stdin.Modifiers`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `packages/darklang/stdlib/cli/stdin/keyRead.dark` | `Darklang.Stdlib.Cli.Stdin.KeyRead`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `packages/darklang/stdlib/cli/__stdin.dark` | `Darklang.Stdlib.Cli.__Stdin` | Native shared stdin decoding and terminal key events (`backend/src/Builtins/Builtins.Cli/Libs/Stdin.fs`) |
| `packages/darklang/stdlib/cli/stdin.dark` | `Darklang.Stdlib.Cli.Stdin`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `packages/darklang/stdlib/altJson/parseError.dark` | `Darklang.Stdlib.AltJson.ParseError`  | `packages/darklang/stdlib/alt-json.dark` |
| `packages/darklang/stdlib/altJson.dark` | `Darklang.Stdlib.AltJson`  | `packages/darklang/stdlib/alt-json.dark` |
| `packages/darklang/stdlib/altJson/helpers.dark` | `Darklang.Stdlib.AltJson.Helpers`  | `packages/darklang/stdlib/alt-json.dark` |
| `packages/darklang/stdlib/altJson/builder.dark` | `Darklang.Stdlib.AltJson.Builder`  | `packages/darklang/stdlib/alt-json.dark` |
| `packages/darklang/languageTools/common.dark` | `Darklang.LanguageTools`  | `packages/darklang/languageTools/common.dark` |
| `packages/darklang/stdlib/json/parseError/jsonPath/part.dark` | `Darklang.Stdlib.Json.ParseError.JsonPath.Part`  | `packages/darklang/stdlib/json.dark` |
| `packages/darklang/stdlib/json/parseError/jsonPath.dark` | `Darklang.Stdlib.Json.ParseError.JsonPath`  | `packages/darklang/stdlib/json.dark` |
| `packages/darklang/stdlib/json/parseError.dark` | `Darklang.Stdlib.Json.ParseError`  | `packages/darklang/stdlib/json.dark` |
| `packages/darklang/stdlib/json.dark` | `Darklang.Stdlib.Json`  | `packages/darklang/stdlib/json.dark` |
| `packages/darklang/stdlib/test.dark` | `Darklang.Stdlib.Test` | Native runtime-error and equality assertions with statically generated value diagnostics; original Result, pass, fail, expect and all come from the imported source |
| `packages/darklang/stdlib/__testAssertions.dark` | `Darklang.Stdlib.Test.Tests` | Replacement for the upstream assertion-result comparison helper; tagged test-diagnostics |
