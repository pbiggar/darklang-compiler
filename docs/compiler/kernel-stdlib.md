# Shared kernel StdLib (migration in progress)

The public CLI libraries are moving onto the same Dark implementation in the
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
concurrently mutable file. Darwin's create-by-path clone acceleration is still to
be wired; its current descriptor clone reports unsupported and uses the fallback.

The interpreter must retain path authorization, protected-store checks, and
no-follow traversal at the trusted boundary. Raw numeric OS descriptors remain
Native-gated. This migration is not ready to replace scoped-only FileSystem APIs
until descriptor ownership can preserve their scoped permissions. It must not
solve that problem by granting Native implicitly or weakening authorization.

Remaining migration work includes the scoped FileSystem boundary, raw system and
clock records, efficient file watching/readiness, non-fork process I/O, removal of
superseded host algorithms, cross-platform verification, and final regression and
performance gates. Checkpoints on this branch are not a completed release.
