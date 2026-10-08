# POSIX host API coverage

The full POSIX package boundary is implemented against darklang/dark
`v0.0.35`, commit `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`.
All 29 Builtin names referenced by
`packages/darklang/stdlib/cli/posix.dark` have implementations.
`Cli.Posix`, `Cli.File`, `Cli.Dir`, and `Cli.FileSystem` use the pinned
interpreter package source; nested modules are split into compiler source
units. Daemon is excluded.

## Builtin boundary

| Interpreter builtins | Implementation |
| --- | --- |
| `posixOpenFlag`, `posixOpen` | Target flags; descriptor-relative open with no symlink traversal |
| `posixFdRead`, `posixFdWrite`, `posixFdSeek`, `posixFdClose` | Native descriptor I/O, full-write loop, signed 64-bit offsets, close |
| `posixGetcwd`, `posixChdir` | Native cwd lookup and directory-descriptor chdir |
| `posixStat`, `posixFileOwner` | Target stat layouts; mode, size, mtime and UID; passwd username lookup |
| `posixMkdir`, `posixRmdir`, `posixUnlink`, `posixRename` | Descriptor-relative mutations |
| `posixChmod`, `posixUtimesNow` | Held-descriptor metadata updates on Linux; nofollow *at operations on Darwin |
| `posixSymlink`, `posixReadlink` | Descriptor-relative links; literal link target retained |
| `posixMkstemp`, `posixMkdtemp` | Random six-character suffix, exclusive creation, 0600/0700 modes |
| `posixListDir` | Native directory entries, excluding dot and dot-dot |
| `posixFlock` | Exclusive lock and unlock on an open descriptor |
| `posixGetenv`, `posixSetenv`, `posixUnsetenv` | Existing native environment vector, shared with child processes |
| `posixKill`, `posixUname`, `timeSleep` | Existing native signal, host and interrupt-resuming sleep operations |
| `posixFnmatch` | C-locale byte matcher: wildcards, pathname flag, ranges, named classes, escapes and C-string termination |

`StdLib/Builtin/__Posix.dark` adapts interpreter signatures to private native
operations in `StdLib/Cli/__Posix.dark`. The 19 additional POSIX operations are
explicit effects through ANF, MIR and LIR, emitted for Linux x86_64, Linux ARM64
and macOS ARM64. These operations return a signed result or negative errno;
Dark code builds the public Result and Error values. Checked Int narrowing
prevents silent descriptor or flag truncation.

Parent traversal holds and closes directory descriptors, opens every ancestor
with O_DIRECTORY/O_NOFOLLOW, and rejects embedded NUL in paths. Public stat
and readlink inspect the final link itself; open and metadata updates reject
it. Variable-size syscall buffers use mapped allocation and release.
Linux metadata updates first use AT_EMPTY_PATH on the held O_PATH descriptor;
older kernels fall back to the held descriptor's procfs path. Darwin timestamp
updates build the attrlist and timespec buffer for setattrlistat.

## Imported algorithms and compiler repair

The imported code supplies recursive directories, temporary-resource cleanup,
copy/move, atomic writes, locks, globbing, line/tail reads, polling watchers,
and structured filesystem errors. The source manifest orders extracted values
before the functions that refer to them.

Named function values in newly lifted lambda bodies were missing the closure
adapter. They could receive the closure object as their first argument.
`src/passes/preparation/LiftFunctions.ml` now collects and rewrites references
in lifted bodies as well as original declarations. Three focused regression
cases reproduce the previous failure and pass with the repair.

Blob equality intentionally remains handle identity. Consequently the pinned
upstream `File.areIdentical` compares separate Blob handles and can return
false for identical file contents. The import preserves that behavior.

## Verification and limits

On 2026-10-08, the Linux x86_64 native build passed, the complete host suite
passed **11,043/11,043**, and `dune runtest` passed all additional regression
checks. The focused POSIX corpus passed **178/178**, including fixed cases
compared with host libc fnmatch; the new closure corpus passed **3/3**.

The parent benchmark gate stopped with
`snapshot workload contract digest is incompatible`. This branch changes no
benchmark workloads, PARITY contracts, profile arguments or expected outputs;
no performance ratio is claimed and no baseline was reset.
The VM subsequently exhausted its disk, preventing further filesystem commands
and ARM64 cross-compilation checks. ARM64 and macOS runtime parity is not claimed.

Account lookup uses `/etc/passwd` with a numeric UID fallback, consistently with
the standalone native host implementation; it does not consult NSS or macOS
Directory Services. Fnmatch uses C-locale byte semantics rather than a runtime
locale database. Darwin errno text comes from target definitions and has not
been compared on a macOS host.

The canonical benchmark leak gate was launched separately. Its result is not
included in the pass claims above.
