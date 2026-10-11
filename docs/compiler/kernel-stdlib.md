# Shared kernel StdLib

The migrated public CLI libraries use the same Dark implementation in the
compiler and interpreter. `Cli.Syscall` contains kernel operations, `Cli.Runtime`
contains target/startup inputs, and ordinary CLI APIs compose these primitives.
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
watch then retains its checked polling fallback. This VM cannot validate that
held-inode event path or execute Darwin code.

Process capture, partial writes, simultaneous stdout/stderr draining, output
limits, deadlines, UTF-8 conversion, and interactive history now live in shared
`Cli.ChildIO` and `Cli.Process`. The runtime leaves launch argv directly, expose
owned pipe descriptors, supervise only their own child, and own two opaque byte
cells. They do not implement capture loops or text decoding. Closing resources
is always permitted for cleanup; other uses recheck retained process authority.
Child waiting releases the ownership lease before blocking so cleanup can kill
and unblock it. Execution teardown disposes unreleased handles and byte cells.

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

## Verification and remaining platform gates

The Linux x86-64 compiler suite passes 13,231 checks, including compiled leak
checks and a closed-pipe regression that explicitly restores default SIGPIPE
before re-executing Dark. `dune runtest` also passes. Interpreter CLI-library
validation passes 361 checks in a freshly seeded, isolated test store;
host-boundary validation passes 18, with one ignored. The 32 shared source units
are checked byte-for-byte, apart from the documented nested-module splitting.
An owned-child capture/deadline/interactive smoke program compiles for Linux
x86-64, Linux ARM64 and macOS ARM64; its Linux x86-64 execution passes with leak
checking enabled. The optional native-runtime diagnostic executable also builds.

This is not a fully cross-platform-verified release. Darwin and Linux ARM64
execution remain to be tested on suitable hosts. Full interpreter validation
was initially blocked by a security review of an HTTP-test destination. The
user-approved isolated retry of `httpclient/line125` subsequently ran, returning
`Timeout` after five seconds instead of `NetworkError`. This VM lacks the
fixture's required `banned-loopback.dark.test` to `127.0.0.1` hosts mapping, so
the retry does not establish loopback-denial coverage. The expected result was
not weakened and no production HTTP code was changed. A separate
install-status check cannot obtain .NET executable identity without procfs in
this VM. The parent performance gate rejects the existing snapshot's workload
contract digest; Cachegrind also cannot run without real `/proc/self/maps`.
No benchmark baseline was replaced to hide either limitation.
