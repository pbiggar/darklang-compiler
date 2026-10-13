# Shared kernel StdLib

The shared source reference is interpreter
[PR #5907](https://github.com/darklang/dark/pull/5907), commit
`26cab9978740a7f9c9d56ea643f3fade813f079e`. The 17 synchronized units include
the final `Cli.Env.home` UID/passwd fallback using `Posix.__decimal`.

The migrated public CLI libraries use the same Dark implementation in the
compiler and interpreter. Existing public APIs compose private `__` helpers
within File, Process, Posix, Sys and DateTime. Those helpers call Builtins directly;
there are no public syscall/runtime wrapper packages or separate algorithm modules.
`Builtin.syscallX` means a kernel operation plus unavoidable ABI adaptation and
trusted authorization. `Builtin.runtimeX` is not a claim that an OS syscall exists.
There is no generic syscall-number/pointer escape hatch and no new fork primitive.

Kernel failures are `Result<value, Int>` with a positive numeric errno. Shared
Dark code supplies error messages, record decoding, retries, complete writes,
temporary-entry naming, and file-copy algorithms. Buffers returned by adapters
must contain only initialized bytes. No native pointer may reach public Dark code.

Run `python3 scripts/sync-kernel-stdlib.py INTERPRETER_CHECKOUT --check` to check
shared source equality. Without `--check` it synchronizes these files. Posix's
nested modules are split into source units for embedding; their code is unchanged.
The script intentionally does not copy native ABI adapters.

File copy opens both descriptors without truncating the destination, rejects
identical device/inode pairs, truncates, attempts cloning, then kernel copying,
then bounded positional reads/writes. A fallback resumes at the amount already
copied. EOF is determined by reads, not just the stat size (virtual files can have
size zero). The fallback retains at most one 64 KiB data chunk. Close failures are
reported only if there is no earlier failure. Copy is not an atomic snapshot of a
concurrently mutable file. Darwin first attempts `fclonefileat` against the held
source and checked destination parent, with no-follow/no-owner-copy flags. Existing
destinations and unsupported filesystems use the descriptor-identity/bounded-copy
path. Recursive directory cloning is not authorized by a single-file capability.

The interpreter must retain path authorization, protected-store checks, and
no-follow traversal at the trusted boundary. Raw numeric OS descriptors remain
Native-gated. Execution-owned descriptor tokens are unguessable and non-transferable
between executions. Every operation rechecks its current path grants and open
mode. Close consumes ownership even on EINTR. Per-descriptor leases prevent reuse
while a call is in flight without blocking unrelated descriptors behind a wait.

Clock records, uname/sysctl/affinity records, paths and directory records are decoded
in shared Dark. Sleep retries use the kernel remainder; browser targets use raw
event-loop runtime inputs. Linux file watches use inotify and ppoll, and Darwin
uses kqueue with typed vnode registrations. Watched resource grants remain attached
to the queue and are checked again on every read/wait. Mixed raw-vnode registration
into a scoped queue is rejected. Dark owns notification coalescing and rearming.
Linux held-inode registration fails closed without real procfs; the public file
watch then retains its checked polling fallback. Darwin execution requires a
Darwin host; cross-compilation alone does not validate it.

Process capture, partial writes, simultaneous stdout/stderr draining, output
limits, deadlines, UTF-8 conversion, and interactive history now live in shared
private `Cli.Process.__child*` helpers. The runtime leaves launch argv directly, expose
owned pipe descriptors, supervise only their own child, and own two opaque byte
cells. They do not implement capture loops or text decoding. Closing resources
is always permitted for cleanup; other uses recheck retained process authority.
Child waiting releases the ownership lease before blocking so cleanup can kill
and unblock it. Execution teardown disposes unreleased handles and byte cells.

The two history cells remain necessary for the existing opaque integer
ProcessHandle API: communicate returns each new response, while terminate returns
the accumulated history across earlier calls. An immutable local accumulator
cannot persist between those calls without changing the public handle contract.
Builtin names stay `syscallX`/`runtimeX`; Builtins are already private. The unused
`syscallFsync` and its compiler lowering have been removed; the interpreter's
internal durable-filesystem implementation still uses fsync where required.

The interpreter uses .NET's process supervisor, never raw waitpid on its children.
The compiler's private launch adapter performs pipe/CLOEXEC/exec-error-handshake
setup and reaping, without exposing fork. Pipe endpoints are lifted above stdio
before duplication, including when the parent starts with closed stdio slots.
The native owned-child runtime ignores SIGPIPE in its parent so closed-pipe
writes return EPIPE, and restores the default disposition in children before
exec. This is private runtime setup, not an exposed signal or fork primitive.
Inherited-terminal children expose no owned pipe descriptors in either engine.
Darwin and Linux launch adapters are generated; Darwin execution remains untested
in this Linux VM. Windows/browser captured processes are explicitly unsupported.

The former native execute/run/spawn/communicate/terminate effects and their
monolithic assembly helpers have been removed, along with packed-directory,
hostname, CPU-count and exclusive-create effects. Those behaviors are composed
in shared Dark over the kernel leaves. Optional backend tests now target the
owned-launch handshake rather than the retired process-output wire records.

## Public surface and runtime differences

There are no public additions to the interpreter's migrated API. The six
removed functions are `Cli.Env.set`, `Cli.Env.unset`, `Cli.Posix.setenv`,
`Cli.Posix.unsetenv`, `Cli.Posix.fnmatch`, and `Cli.Posix.OpenFlags.flag`.
Named open flags remain. Glob matching is a private Dark algorithm. Environment
reads use runtime inputs, not fictitious environment syscalls.

Account fallback parses `/etc/passwd`; it does not emulate NSS. `Cli.Env.home`
uses HOME first, then the real UID's home field. CPU count uses Linux affinity
or Darwin `hw.ncpu`; it does not separately apply cgroup quotas. Darwin
`utimesNow` supplies whole seconds. DateTime.today uses a UTC day boundary.

The compiler emits native syscall instructions and ABI adapters; it does not
compile or link the interpreter's C transport. Its native executables have no
interpreter capability policy or effect-replay store. Consequently the compiler's
`runtimeAuthorizeReadPath` returns the candidate path, while the interpreter
checks its retained grants. Shared algorithms do not imply identical host
security policies. There is no new public fork or arbitrary-syscall API.

Compiler-only private source fragments remain necessary for native lowering.
Their filenames do not add public modules. The interpreter's removed VM
scaffolding is not copied here: the compiler's pre-existing `scripts/vm` remains
its documented native-toolchain bootstrap.

## Earlier validation and remaining platform gates

At compiler commit `d72e328c0`, the Linux x86-64 suite passed 13,238 checks, including compiled leak
checks and a closed-pipe regression that explicitly restores default SIGPIPE
before re-executing Dark. `dune runtest` also passes. Interpreter CLI-library
validation passes 358 checks in a reloaded, integrity-checked isolated test store
(three tests of retired error-normalization helpers were removed). Parser and
round-trip validation passes 583 checks, including private type/function naming;
DateTime validation passes 222; Builtin validation passes 11.
Host/security-boundary validation passes 18, with
one ignored. The 17 shared source units
are checked byte-for-byte, apart from the documented nested-module splitting.
An owned-child capture/deadline/interactive smoke program compiles for Linux
x86-64, Linux ARM64 and macOS ARM64; its Linux x86-64 execution passes with leak
checking enabled. The optional native-runtime diagnostic executable also builds.

The reduced API introduces no public top-level StdLib declarations relative to
the pre-migration interpreter. All 16 added helper/forwarding modules are removed;
necessary algorithms and their two implementation records use private `__` names
in existing modules. Compiler tests verify that user code cannot call the private
helpers or the deleted forwarding packages. Builtin names remain `syscallX` and
`runtimeX`, without `__`.
The interpreter parser/name resolver accepts private record and function names,
matching the compiler's existing convention. Temporary interpreter VM launchers
used during the earlier validation are no longer part of that PR.

The final interpreter commit above passed CI: 12,574 tests, four ignored, and
all 13 build gates. Interpreter allocation budgets do not apply to native compiler
benchmarks and are not copied into this repository.

This is not a fully cross-platform-verified release. Darwin and Linux ARM64
execution remain to be tested on suitable hosts. The earlier compiler parent
performance gate rejected an incompatible workload-contract snapshot; that
failure did not justify replacing its benchmark baseline. Current compiler
validation is reported separately in the compiler PR.

For the final interpreter alignment, `env -u LD_PRELOAD ./build --ai` passed,
`TZ=UTC ./run-tests --ai` passed 13,267/13,267 tests, `dune runtest` passed
(including explicit/empty HOME and real-UID passwd fallback), and
`python3 scripts/check_compiled_leaks.py` passed all 58 workloads. A shared
process/home/clock/system smoke program compiled for Linux x86-64, Linux ARM64
and macOS ARM64; its Linux x86-64 execution passed with clean leak accounting.
The 17-source equality and 63 referenced syscall/runtime adapter checks passed.
`./benchmarks/run_benchmarks.sh --verify-parent full` failed preflight with
`snapshot workload contract digest is incompatible`; it produced no performance
measurement. No tracked benchmark data differs from main.
