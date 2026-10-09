# Embedded library source inventory

The compiler targets interpreter **v0.0.35**, revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460` in `darklang/dark`.
This inventory was checked against that revision, including nested modules.
Paths in the interpreter column are relative to the interpreter repository.
The compatibility ledgers own API coverage and behavioral differences; this
inventory records source placement and origin, not complete API parity.

`StdLib/` implicitly represents `Darklang.Stdlib`. Other packages live under
`packages/` with their full owner and package names. See
[source organization](source-organization.md#standard-library-layout).
The rows follow `library-sources.list` declaration order.

A `__` filename marks compiler support or a private implementation fragment.
It does not rename the declared module or enforce language visibility: that
is controlled by declaration names and the frontend's internal-mode checks.
For example, the compiler's `Cli.__Args` helpers are an extension,
while `StdLib/Builtin.dark` bridges real interpreter builtins implemented in F#.
Public fragments such as `Http/Request.dark`, `Cli/OS.dark`, and
`List/SortByComparatorHelpers.dark` retain normal filenames because their
nested modules exist in the pinned interpreter. Private terminal-text
fragments use `__` even where the interpreter also has private helpers.

| Compiler file | Declared package | Interpreter source or compiler role |
|---|---|---|
| `StdLib/__Types.dark` | (root declarations) | Compiler representation of the interpreter’s root Uuid type (`backend/src/LibExecution/RuntimeTypes.fs`) |
| `StdLib/Root.dark` | `Darklang.Stdlib`  | `packages/darklang/stdlib/noModule.dark` |
| `StdLib/Int8.dark` | `Darklang.Stdlib.Int8`  | `packages/darklang/stdlib/int8.dark` |
| `StdLib/Int16.dark` | `Darklang.Stdlib.Int16`  | `packages/darklang/stdlib/int16.dark` |
| `StdLib/Int32.dark` | `Darklang.Stdlib.Int32`  | `packages/darklang/stdlib/int32.dark` |
| `StdLib/Int64.dark` | `Darklang.Stdlib.Int64`  | `packages/darklang/stdlib/int64.dark` |
| `StdLib/Int/__Integer.dark` | `Darklang.Stdlib.Int`  | Compiler arbitrary-precision integer representation; package defined in `packages/darklang/stdlib/int.dark` |
| `StdLib/Int.dark` | `Darklang.Stdlib.Int`  | `packages/darklang/stdlib/int.dark` |
| `StdLib/Int128.dark` | `Darklang.Stdlib.Int128`  | `packages/darklang/stdlib/int128.dark` |
| `StdLib/UInt8.dark` | `Darklang.Stdlib.UInt8`  | `packages/darklang/stdlib/uint8.dark` |
| `StdLib/UInt16.dark` | `Darklang.Stdlib.UInt16`  | `packages/darklang/stdlib/uint16.dark` |
| `StdLib/UInt32.dark` | `Darklang.Stdlib.UInt32`  | `packages/darklang/stdlib/uint32.dark` |
| `StdLib/UInt64.dark` | `Darklang.Stdlib.UInt64`  | `packages/darklang/stdlib/uint64.dark` |
| `StdLib/UInt128.dark` | `Darklang.Stdlib.UInt128`  | `packages/darklang/stdlib/uint128.dark` |
| `StdLib/Bool.dark` | `Darklang.Stdlib.Bool`  | `packages/darklang/stdlib/bool.dark` |
| `StdLib/Cli/Posix/Modes.dark` | `Darklang.Stdlib.Cli.Posix.Modes`  | `packages/darklang/stdlib/cli/posix.dark` |
| `StdLib/Cli/Posix/Errno.dark` | `Darklang.Stdlib.Cli.Posix.Errno`  | `packages/darklang/stdlib/cli/posix.dark` |
| `StdLib/Cli/Posix/StatMode.dark` | `Darklang.Stdlib.Cli.Posix.StatMode`  | `packages/darklang/stdlib/cli/posix.dark` |
| `StdLib/Cli/FileSystem.dark` | `Darklang.Stdlib.Cli.FileSystem`  | `packages/darklang/stdlib/cli/fileSystem.dark` |
| `StdLib/Cli/FileSystem/FileError.dark` | `Darklang.Stdlib.Cli.FileSystem.FileError`  | `packages/darklang/stdlib/cli/fileSystem.dark` |
| `StdLib/Cli/FileSystem/__Packed.dark` | `Darklang.Stdlib.Cli.FileSystem` | Private decoder for native directory adapter |
| `StdLib/Env.dark` | `Darklang.Stdlib.Env`  | `packages/darklang/stdlib/env.dark` |
| `StdLib/Builtin.dark` | `Builtin`  | Portable interpreter builtin bridges plus native adapter helpers (`backend/src/Builtins/Builtins.Pure/Libs/UInt64.fs` and `backend/src/Builtins/Builtins.Cli/Libs/{Directory,File,Environment}.fs`) |
| `StdLib/Builtin/__Posix.dark` | `Builtin` | Interpreter POSIX builtin bridge (`backend/src/Builtins/Builtins.Cli/Libs/Posix.fs`) |
| `StdLib/Builtin/__Terminal.dark` | `Builtin` | Native terminal facts, size, color policy, text inspection and stdin interactivity (`backend/src/Builtins/Builtins.Cli/Libs/{Terminal,Stdin}.fs`) |
| `StdLib/Builtin/__Stdin.dark` | `Builtin` | Interpreter stdin builtin wrappers (`backend/src/Builtins/Builtins.Cli/Libs/Stdin.fs`) |
| `StdLib/Builtin/__Time.dark` | `Builtin` | Interpreter monotonic milliseconds backed by the existing native clock helper (`backend/src/Builtins/Builtins.Time/Libs/Time.fs`) |
| `StdLib/Builtin/__BuildInfo.dark` | `Builtin` | Compiler revision substituted at build time, corresponding to interpreter `getBuildHash` (`backend/src/Builtins/Builtins.Cli/Libs/Environment.fs`) |
| `StdLib/Tuple2.dark` | `Darklang.Stdlib.Tuple2`  | `packages/darklang/stdlib/tuple2.dark` |
| `StdLib/Tuple3.dark` | `Darklang.Stdlib.Tuple3`  | `packages/darklang/stdlib/tuple3.dark` |
| `StdLib/Result.dark` | `Darklang.Stdlib.Result`  | `packages/darklang/stdlib/result.dark` |
| `StdLib/Option.dark` | `Darklang.Stdlib.Option`  | `packages/darklang/stdlib/option.dark` |
| `StdLib/List/SortByComparatorHelpers.dark` | `Darklang.Stdlib.List.SortByComparatorHelpers`  | `packages/darklang/stdlib/list.dark` |
| `StdLib/List.dark` | `Darklang.Stdlib.List`  | `packages/darklang/stdlib/list.dark` |
| `StdLib/Print.dark` | `Darklang.Stdlib`  | `packages/darklang/stdlib/print.dark` |
| `StdLib/Fun.dark` | `Darklang.Stdlib.Fun`  | `packages/darklang/stdlib/fun.dark` |
| `StdLib/Float.dark` | `Darklang.Stdlib.Float`  | `packages/darklang/stdlib/float.dark` |
| `StdLib/Cli/__Posix.dark` | `Darklang.Stdlib.Cli.__Posix` | Private native syscall boundary for interpreter POSIX operations |
| `StdLib/Cli/__Fnmatch.dark` | `Darklang.Stdlib.Cli.__Fnmatch` | Private POSIX pattern matching implementation |
| `StdLib/Cli/Posix.dark` | `Darklang.Stdlib.Cli.Posix`  | `packages/darklang/stdlib/cli/posix.dark` |
| `StdLib/Cli/Posix/__Error.dark` | `Darklang.Stdlib.Cli.Posix` | Private conversion from the existing native CLI error record |
| `StdLib/Cli/Posix/OpenFlags.dark` | `Darklang.Stdlib.Cli.Posix.OpenFlags` | `packages/darklang/stdlib/cli/posix.dark` |
| `StdLib/Retry.dark` | `Darklang.Stdlib.Retry`  | `packages/darklang/stdlib/retry.dark` |
| `StdLib/Cli/Path.dark` | `Darklang.Stdlib.Cli.Path`  | `packages/darklang/stdlib/cli/path.dark` |
| `StdLib/Cli/File.dark` | `Darklang.Stdlib.Cli.File`  | `packages/darklang/stdlib/cli/file.dark` |
| `StdLib/Cli/Dir.dark` | `Darklang.Stdlib.Cli.Dir` | `packages/darklang/stdlib/cli/dir.dark` |
| `StdLib/String/__Unicode/__Data.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index00.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index01.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index02.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index03.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index04.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index05.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Index06.dark` | `Darklang.Stdlib.String.__Unicode.__Data`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table00.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table00`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table01.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table01`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table02.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table02`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table03.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table03`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table04.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table04`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table05.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table05`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table06.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table06`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table07.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table07`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table08.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table08`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table09.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table09`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table10.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table10`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table11.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table11`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table12.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table12`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table13.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table13`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table14.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table14`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table15.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table15`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table16.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table16`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table17.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table17`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table18.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table18`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table19.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table19`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table20.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table20`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table21.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table21`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table22.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table22`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table23.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table23`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table24.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table24`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table25.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table25`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table26.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table26`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table27.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table27`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table28.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table28`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table29.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table29`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table30.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table30`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table31.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table31`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table32.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table32`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table33.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table33`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table34.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table34`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table35.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table35`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table36.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table36`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table37.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table37`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table38.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table38`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table39.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table39`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table40.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table40`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table41.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table41`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table42.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table42`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table43.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table43`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table44.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table44`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table45.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table45`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table46.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table46`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table47.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table47`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table48.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table48`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table49.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table49`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table50.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table50`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table51.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table51`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table52.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table52`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table53.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table53`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table54.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table54`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table55.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table55`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table56.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table56`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table57.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table57`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table58.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table58`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table59.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table59`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table60.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table60`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table61.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table61`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table62.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table62`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode/__Data/__Table63.dark` | `Darklang.Stdlib.String.__Unicode.__Data.__Table63`  | Generated compiler Unicode lookup data |
| `StdLib/String/__Unicode.dark` | `Darklang.Stdlib.String.__Unicode`  | Compiler Unicode normalization, casing and segmentation |
| `StdLib/String.dark` | `Darklang.Stdlib.String`  | `packages/darklang/stdlib/string.dark` |
| `StdLib/__Hash.dark` | (root declarations) | Compiler hashing helpers |
| `StdLib/Dict.dark` | `Darklang.Stdlib.Dict`  | `packages/darklang/stdlib/dict.dark` |
| `StdLib/Dict/__HAMT.dark` | `Darklang.Stdlib.Dict`  | Compiler dictionary representation; package defined in `packages/darklang/stdlib/dict.dark` |
| `StdLib/Uuid.dark` | `Darklang.Stdlib.Uuid`  | `packages/darklang/stdlib/uuid.dark` |
| `StdLib/Diff.dark` | `Darklang.Stdlib.Diff`  | `packages/darklang/stdlib/diff.dark` |
| `packages/Darklang/LanguageTools/ProgramTypes.dark` | `Darklang.LanguageTools.ProgramTypes`  | `packages/darklang/languageTools/programTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/Base.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/FQTypeName.dark` | `Darklang.LanguageTools.RuntimeTypes.FQTypeName`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/FQFnName.dark` | `Darklang.LanguageTools.RuntimeTypes.FQFnName`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/FQValueName.dark` | `Darklang.LanguageTools.RuntimeTypes.FQValueName`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/TypeReference.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/PrettyPrinter/RuntimeTypes.dark` | `Darklang.PrettyPrinter.RuntimeTypes`, `Dval`  | `packages/darklang/prettyPrinter/runtimeError.dark`, `packages/darklang/prettyPrinter/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/ValueType.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/Dval.dark` | `Darklang.LanguageTools.RuntimeTypes`  | `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/PrettyPrinter/RuntimeTypes/RuntimeError.dark` | `Darklang.PrettyPrinter.RuntimeTypes.RuntimeError`  | `packages/darklang/prettyPrinter/runtimeError.dark` |
| `packages/Darklang/LanguageTools/RuntimeTypes/__ValueTypeSupport.dark` | `Darklang.LanguageTools.RuntimeTypes`  | Compiler custom-type catalog matching helper; package defined in `packages/darklang/languageTools/runtimeTypes.dark` |
| `packages/Darklang/LanguageTools/PackageManager.dark` | `Darklang.LanguageTools.PackageManager`  | `packages/darklang/languageTools/packageManager.dark` |
| `packages/Darklang/LanguageTools/PackageManager/PickContext.dark` | `Darklang.LanguageTools.PackageManager.PickContext`  | `packages/darklang/languageTools/packageManager.dark` |
| `packages/Darklang/SCM/Branch.dark` | `Darklang.SCM.Branch`  | `packages/darklang/scm/branch.dark` |
| `StdLib/ValueSearch.dark` | `Darklang.Stdlib.ValueSearch`  | `packages/darklang/stdlib/valueSearch.dark` |
| `StdLib/DateTime.dark` | `Darklang.Stdlib.DateTime`  | `packages/darklang/stdlib/dateTime.dark` |
| `StdLib/Duration.dark` | `Darklang.Stdlib.Duration`  | `packages/darklang/stdlib/duration.dark` |
| `StdLib/Blob.dark` | `Darklang.Stdlib.Blob`  | `packages/darklang/stdlib/blob.dark` |
| `StdLib/Stream.dark` | `Darklang.Stdlib.Stream`  | `packages/darklang/stdlib/stream.dark` |
| `StdLib/Html.dark` | `Darklang.Stdlib.Html`  | `packages/darklang/stdlib/html.dark` |
| `StdLib/Http.dark` | `Darklang.Stdlib.Http`  | `packages/darklang/stdlib/http.dark` |
| `StdLib/Http/Request.dark` | `Darklang.Stdlib.Http.Request`  | `packages/darklang/stdlib/http.dark` |
| `StdLib/HttpClient.dark` | `Darklang.Stdlib.HttpClient`  | `packages/darklang/stdlib/httpclient.dark` |
| `StdLib/HttpClient/ContentType.dark` | `Darklang.Stdlib.HttpClient.ContentType`  | `packages/darklang/stdlib/httpclient.dark` |
| `StdLib/HttpClient/Sse.dark` | `Darklang.Stdlib.HttpClient.Sse`  | `packages/darklang/stdlib/httpclient.dark` |
| `StdLib/HttpServer/Config.dark` | `Darklang.Stdlib.HttpServer.Config`  | `packages/darklang/stdlib/httpserver.dark` |
| `StdLib/HttpServer.dark` | `Darklang.Stdlib.HttpServer`  | `packages/darklang/stdlib/httpserver.dark` |
| `StdLib/__Network.dark` | `Darklang.Stdlib.__Network`  | Compiler support: Owned native TCP/UDP sockets, listeners, deadlines, and scoped shutdown signals |
| `StdLib/__Datagram.dark` | `Darklang.Stdlib.__Datagram` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__HttpWire.dark` | `Darklang.Stdlib.__HttpWire`  | Compiler support: Byte-bounded HTTP/1.1 and URL primitives written in Dark |
| `StdLib/__DnsWire.dark` | `Darklang.Stdlib.__DnsWire`  | Compiler support: Bounded DNS query and answer wire format implemented in Dark |
| `StdLib/__HttpConnect.dark` | `Darklang.Stdlib.__HttpConnect`  | Compiler support: Check resolved IP bytes before opening HTTP transport |
| `StdLib/__Http2Wire.dark` | `Darklang.Stdlib.__Http2Wire` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Hpack.dark` | `Darklang.Stdlib.__Hpack` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__TlsAlpn.dark` | `Darklang.Stdlib.__TlsAlpn` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__HttpTransport.dark` | `Darklang.Stdlib.__HttpTransport` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http2Fields.dark` | `Darklang.Stdlib.__Http2Fields` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http2.dark` | `Darklang.Stdlib.__Http2` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicWire.dark` | `Darklang.Stdlib.__QuicWire` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicCrypto.dark` | `Darklang.Stdlib.__QuicCrypto` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicPacket.dark` | `Darklang.Stdlib.__QuicPacket` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicFrames.dark` | `Darklang.Stdlib.__QuicFrames` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicAck.dark` | `Darklang.Stdlib.__QuicAck` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicApplication.dark` | `Darklang.Stdlib.__QuicApplication` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicReassembly.dark` | `Darklang.Stdlib.__QuicReassembly` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicStreams.dark` | `Darklang.Stdlib.__QuicStreams` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicFlow.dark` | `Darklang.Stdlib.__QuicFlow` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicRecovery.dark` | `Darklang.Stdlib.__QuicRecovery` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicSpace.dark` | `Darklang.Stdlib.__QuicSpace` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicHandshakePacket.dark` | `Darklang.Stdlib.__QuicHandshakePacket` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicParameters.dark` | `Darklang.Stdlib.__QuicParameters` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicTls.dark` | `Darklang.Stdlib.__QuicTls` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicKeys.dark` | `Darklang.Stdlib.__QuicKeys` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicClient.dark` | `Darklang.Stdlib.__QuicClient` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicServer.dark` | `Darklang.Stdlib.__QuicServer` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicConnection.dark` | `Darklang.Stdlib.__QuicConnection` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Qpack.dark` | `Darklang.Stdlib.__Qpack` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Wire.dark` | `Darklang.Stdlib.__Http3Wire` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Stream.dark` | `Darklang.Stdlib.__Http3Stream` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Message.dark` | `Darklang.Stdlib.__Http3Message` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Control.dark` | `Darklang.Stdlib.__Http3Control` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Uni.dark` | `Darklang.Stdlib.__Http3Uni` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3.dark` | `Darklang.Stdlib.__Http3` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Client.dark` | `Darklang.Stdlib.__Http3Client` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Server.dark` | `Darklang.Stdlib.__Http3Server` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Http3Discovery.dark` | `Darklang.Stdlib.__Http3Discovery` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__AltSvc.dark` | `Darklang.Stdlib.__AltSvc` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__AltSvcCache.dark` | `Darklang.Stdlib.__AltSvcCache` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/HttpClientSession.dark` | `Darklang.Stdlib.HttpClientSession` | Compiler extension: HTTP transport and secure server support |
| `StdLib/__HttpsService.dark` | `Darklang.Stdlib.__HttpsService` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__HttpsDns.dark` | `Darklang.Stdlib.__HttpsDns` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/Pretty.dark` | `Darklang.Stdlib.Pretty`  | `packages/darklang/stdlib/pretty.dark` |
| `StdLib/Char.dark` | `Darklang.Stdlib.Char`  | `packages/darklang/stdlib/char.dark` |
| `StdLib/Regex.dark` | `Darklang.Stdlib.Regex`  | `packages/darklang/stdlib/regex.dark` |
| `StdLib/Base64.dark` | `Darklang.Stdlib.Base64`  | `packages/darklang/stdlib/base64.dark` |
| `StdLib/X509.dark` | `Darklang.Stdlib.X509`  | `packages/darklang/stdlib/x509.dark` |
| `StdLib/__X509Identity.dark` | `Darklang.Stdlib.__X509Identity`  | Compiler support: bounded DER leaf identity and DNS hostname checks |
| `StdLib/__RsaSpki.dark` | `Darklang.Stdlib.__RsaSpki`  | Compiler support: strict DER rsaEncryption SubjectPublicKeyInfo parsing |
| `StdLib/Crypto.dark` | `Darklang.Stdlib.Crypto`  | `packages/darklang/stdlib/crypto.dark` |
| `StdLib/__RsaPss.dark` | `Darklang.Stdlib.__RsaPss`  | Compiler support: RSA-PSS/SHA-256 public signature verification for TLS 1.3 |
| `StdLib/__RsaMontgomery.dark` | `Darklang.Stdlib.__RsaMontgomery` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__RsaPrivate.dark` | `Darklang.Stdlib.__RsaPrivate` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__RsaSigning.dark` | `Darklang.Stdlib.__RsaSigning` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/TlsServerIdentity.dark` | `Darklang.Stdlib.TlsServerIdentity` | Compiler extension: HTTP transport and secure server support |
| `StdLib/__Tls13ServerHello.dark` | `Darklang.Stdlib.__Tls13ServerHello` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Tls13ServerRetry.dark` | `Darklang.Stdlib.__Tls13ServerRetry` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Tls13ServerHandshake.dark` | `Darklang.Stdlib.__Tls13ServerHandshake` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Tls13KeyUpdate.dark` | `Darklang.Stdlib.__Tls13KeyUpdate` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__Tls13ServerTcp.dark` | `Darklang.Stdlib.__Tls13ServerTcp` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicServerTls.dark` | `Darklang.Stdlib.__QuicServerTls` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/__QuicServerRetry.dark` | `Darklang.Stdlib.__QuicServerRetry` | Compiler support: HTTP/2, HTTP/3, QUIC, or TLS protocol implementation |
| `StdLib/HttpServer/Tls.dark` | `Darklang.Stdlib.HttpServer.Tls` | Compiler extension: HTTP transport and secure server support |
| `StdLib/HttpServer/Quic.dark` | `Darklang.Stdlib.HttpServer.Quic` | Compiler extension: HTTP transport and secure server support |
| `StdLib/HttpServer/Secure.dark` | `Darklang.Stdlib.HttpServer.Secure` | Compiler extension: HTTP transport and secure server support |
| `StdLib/__P256.dark` | `Darklang.Stdlib.__P256`  | Compiler support: bounded P-256 ECDSA/SHA-256 verification for TLS signatures |
| `StdLib/__RsaPkcs1.dark` | `Darklang.Stdlib.__RsaPkcs1`  | Compiler support: RSA PKCS#1 v1.5 SHA-256/384 certificate signature checks |
| `StdLib/__X509Chain.dark` | `Darklang.Stdlib.__X509Chain`  | Compiler support: bounded X.509 path checks before HTTPS identity is trusted |
| `StdLib/__AesGcm.dark` | `Darklang.Stdlib.__AesGcm`  | Compiler support: AES-128/256 and GCM authenticated encryption for TLS records |
| `StdLib/__Chacha20Poly1305.dark` | `Darklang.Stdlib.__Chacha20Poly1305`  | Compiler support: RFC 8439 ChaCha20 and Poly1305 authenticated encryption |
| `StdLib/__Tls13.dark` | `Darklang.Stdlib.__Tls13`  | Compiler support: TLS 1.3 key derivation and bounded record framing |
| `StdLib/__Tls13Handshake.dark` | `Darklang.Stdlib.__Tls13Handshake`  | Compiler support: TLS 1.3 handshake secrets, server Finished, and bounded reassembly |
| `StdLib/__Tls13Certificate.dark` | `Darklang.Stdlib.__Tls13Certificate`  | Compiler support: bounded TLS 1.3 certificate flight framing |
| `StdLib/__Tls13Client.dark` | `Darklang.Stdlib.__Tls13Client`  | Compiler support: authenticated TLS 1.3 exchange over the Dark TCP transport |
| `StdLib/Math.dark` | `Darklang.Stdlib.Math`  | `packages/darklang/stdlib/math.dark` |
| `StdLib/__X25519.dark` | `Darklang.Stdlib.__X25519`  | Compiler support: RFC 7748 Montgomery ladder over fixed 16-bit field limbs |
| `StdLib/List/__SkewList.dark` | `Darklang.Stdlib.List`  | Compiler persistent-list representation; package defined in `packages/darklang/stdlib/list.dark` |
| `StdLib/List/__ListArray.dark` | `Darklang.Stdlib.List`  | Compiler contiguous-list operations; package defined in `packages/darklang/stdlib/list.dark` |
| `StdLib/Cli/UI/Colors.dark` | `Darklang.Stdlib.Cli.UI.Colors`  | `packages/darklang/stdlib/cli/ui/colors.dark` |
| `StdLib/Cli/UI/TextField.dark` | `Darklang.Stdlib.Cli.UI.TextField`  | `packages/darklang/stdlib/cli/ui/textfield.dark` |
| `StdLib/Cli/Tui/TerminalSession/Ansi.dark` | `Darklang.Stdlib.Cli.Tui.TerminalSession.Ansi`  | `packages/darklang/stdlib/cli/tui/terminalSession.dark` |
| `StdLib/Cli/Tui/TerminalSupport.dark` | `Darklang.Stdlib.Cli.Tui.TerminalSupport` | `packages/darklang/stdlib/cli/tui/terminalSupport.dark` |
| `packages/Darklang/Cli/Terminal/Size.dark` | `Darklang.Cli.Terminal` | Public getSize fragment of `packages/darklang/cli/utils/terminal.dark` |
| `packages/Darklang/Cli/Terminal/Color.dark` | `Darklang.Cli.Terminal` | Public colorEnabled fragment of `packages/darklang/cli/utils/terminal.dark` |
| `StdLib/Cli/Tui/Text/__Escape.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | Private escape-scanning fragment of Cli.Tui.Text; package defined in `packages/darklang/stdlib/cli/tui/text.dark` |
| `StdLib/Cli/Tui/Text/__Width.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | Private width fragment of Cli.Tui.Text; package defined in `packages/darklang/stdlib/cli/tui/text.dark` |
| `StdLib/Cli/Tui/Text/__Clip.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | Private clipping fragment of Cli.Tui.Text; package defined in `packages/darklang/stdlib/cli/tui/text.dark` |
| `StdLib/Cli/Tui/Text.dark` | `Darklang.Stdlib.Cli.Tui.Text`  | `packages/darklang/stdlib/cli/tui/text.dark` |
| `StdLib/Cli/Log.dark` | `Darklang.Stdlib.Cli.Log`  | `packages/darklang/stdlib/cli/log.dark` |
| `StdLib/Cli/UI/Progress.dark` | `Darklang.Stdlib.Cli.UI.Progress`  | `packages/darklang/stdlib/cli/ui/progress.dark` |
| `StdLib/Cli/UI/Prompt.dark` | `Darklang.Stdlib.Cli.UI.Prompt`  | `packages/darklang/stdlib/cli/ui/prompt.dark` |
| `StdLib/Cli/UI/Spinner.dark` | `Darklang.Stdlib.Cli.UI.Spinner`  | `packages/darklang/stdlib/cli/ui/spinner.dark` |
| `StdLib/Cli/UI/Table.dark` | `Darklang.Stdlib.Cli.UI.Table`  | `packages/darklang/stdlib/cli/ui/table.dark` |
| `StdLib/Cli.dark` | `Darklang.Stdlib.Cli`  | `packages/darklang/stdlib/cli/execution.dark`, `packages/darklang/stdlib/cli/host.dark` |
| `StdLib/Cli/OS.dark` | `Darklang.Stdlib.Cli.OS`  | `packages/darklang/stdlib/cli/host.dark` |
| `StdLib/Cli/Architecture.dark` | `Darklang.Stdlib.Cli.Architecture`  | `packages/darklang/stdlib/cli/host.dark` |
| `StdLib/Cli/Shell.dark` | `Darklang.Stdlib.Cli.Shell`  | `packages/darklang/stdlib/cli/host.dark` |
| `StdLib/Cli/Host.dark` | `Darklang.Stdlib.Cli.Host`  | `packages/darklang/stdlib/cli/host.dark` |
| `StdLib/Cli/Env.dark` | `Darklang.Stdlib.Cli.Env`  | `packages/darklang/stdlib/cli/env.dark` |
| `StdLib/Cli/__Args.dark` | `Darklang.Stdlib.Cli.__Args`  | Compiler positional CLI argument helpers |
| `StdLib/Cli/Process.dark` | `Darklang.Stdlib.Cli.Process`  | `packages/darklang/stdlib/cli/process.dark` |
| `StdLib/Cli/Sys.dark` | `Darklang.Stdlib.Cli.Sys`  | `packages/darklang/stdlib/cli/sys.dark` |
| `StdLib/Cli/Stdin/Key.dark` | `Darklang.Stdlib.Cli.Stdin.Key`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `StdLib/Cli/Stdin/Modifiers.dark` | `Darklang.Stdlib.Cli.Stdin.Modifiers`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `StdLib/Cli/Stdin/KeyRead.dark` | `Darklang.Stdlib.Cli.Stdin.KeyRead`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `StdLib/Cli/__Stdin.dark` | `Darklang.Stdlib.Cli.__Stdin` | Native shared stdin decoding and terminal key events (`backend/src/Builtins/Builtins.Cli/Libs/Stdin.fs`) |
| `StdLib/Cli/Stdin.dark` | `Darklang.Stdlib.Cli.Stdin`  | `packages/darklang/stdlib/cli/stdin.dark` |
| `StdLib/AltJson/ParseError.dark` | `Darklang.Stdlib.AltJson.ParseError`  | `packages/darklang/stdlib/alt-json.dark` |
| `StdLib/AltJson.dark` | `Darklang.Stdlib.AltJson`  | `packages/darklang/stdlib/alt-json.dark` |
| `StdLib/AltJson/Helpers.dark` | `Darklang.Stdlib.AltJson.Helpers`  | `packages/darklang/stdlib/alt-json.dark` |
| `StdLib/AltJson/Builder.dark` | `Darklang.Stdlib.AltJson.Builder`  | `packages/darklang/stdlib/alt-json.dark` |
| `packages/Darklang/LanguageTools.dark` | `Darklang.LanguageTools`  | `packages/darklang/languageTools/common.dark` |
| `StdLib/Json/ParseError/JsonPath/Part.dark` | `Darklang.Stdlib.Json.ParseError.JsonPath.Part`  | `packages/darklang/stdlib/json.dark` |
| `StdLib/Json/ParseError/JsonPath.dark` | `Darklang.Stdlib.Json.ParseError.JsonPath`  | `packages/darklang/stdlib/json.dark` |
| `StdLib/Json/ParseError.dark` | `Darklang.Stdlib.Json.ParseError`  | `packages/darklang/stdlib/json.dark` |
| `StdLib/Json.dark` | `Darklang.Stdlib.Json`  | `packages/darklang/stdlib/json.dark` |
