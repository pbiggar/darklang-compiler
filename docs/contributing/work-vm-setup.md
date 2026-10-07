# ChatGPT Work VM setup

Use the conversation's writable workspace for an isolated checkout, toolchain
and logs. The local macOS coordination checkout and merge train do not apply
to a Work Linux VM. `Dockerfile` and root `dependencies.lock` own the pins;
the VM scripts reproduce the native prerequisites without installing into
system directories.

## Checkout and task branch

If a checkout is already attached, inspect its root and working tree before
making changes and preserve existing work. Otherwise, from the writable
workspace:

```bash
git clone https://github.com/pbiggar/darklang-compiler.git darklang-compiler
cd darklang-compiler
git rev-parse --show-toplevel
git status --short
git switch -c TASK_BRANCH origin/main
```

Replace `TASK_BRANCH` with a name for the task. Read `docs/index.md` and
`AGENTS.md`, and reuse this checkout throughout the task.

## Native toolchain

The rootless bootstrap supports Linux x86-64. It requires Git, a C build
toolchain, Python 3, curl, ripgrep, archive utilities and network access to the
pinned upstream downloads. Use the Docker development environment on other
hosts. Choose an absolute toolchain path under the writable workspace, ideally
beside the checkout, and run these commands from the repository root:

```bash
bash scripts/vm/setup-native-toolchain /absolute/writable/toolchains
source /absolute/writable/toolchains/activate
ocamlc -version
dune --version
ocaml -I +unix unix.cma /absolute/writable/toolchains/unix-smoke.ml
```

The expected versions are OCaml 5.5.1 and Dune 3.24.2. The bootstrap builds
OCaml serially, verifies archive checksums before reuse and tests Unix path
resolution and child-process creation. Locked OCaml dependencies live in its
`opam/dark` switch; Ubuntu prerequisites are checksum-verified against snapshot
20260828T000000Z and extracted into a workspace sysroot.

Source `activate` in each new shell before building or testing. It sets the
compiler and package paths, native header/library paths, writable temporary
directory and VM adapter. Downloads, opam state, temporary files and full setup
logs stay under the toolchain directory.
Opam build directories are retained there to avoid recursive-cleanup failures
on restricted Work filesystems.

The VM adapter supplies stack attributes when procfs is unavailable and maps
`/tmp` paths into the writable temporary directory. Configure probes run
without a preload. This adapter is specific to the restricted VM; normal
hosts use their native environment.

## Build and verify

```bash
env -u LD_PRELOAD ./build --ai
./run-tests --ai
dune runtest
python3 scripts/check_compiled_leaks.py
./benchmarks/run_benchmarks.sh --verify-parent full
```

Build again after changing sources or Dune files. `./run-tests` only executes
the already-built suite. Automated build/test failure logs are retained under
`TestResults/ai/`; see [verification policy](verification.md) for the complete
gates and benchmark prerequisites. Configure the task branch's upstream as
`origin/main` before the parent benchmark gate if it has none:

```bash
git branch --set-upstream-to=origin/main
```

The default host suite executes generated x86-64 binaries directly. Only
declare another target when the task includes it. The executable under
`test/runtime-execution/` explicitly exercises both architectures and is not
part of the default host suite.

The compiler embeds its standard library and Unicode tables. Adding or moving
a source requires updating `library-sources.list` and rebuilding; no source
installation step is required. See [source organization](../compiler/source-organization.md)
and the [complete file inventory](../compiler/library-sources.md).

## Optional QEMU setup

Instruction-count or explicit cross-target checks that use QEMU require the
Docker-pinned emulators and plugin separately from the native bootstrap:

```bash
bash scripts/vm/setup-qemu /absolute/writable/toolchains
source /absolute/writable/toolchains/activate
export PORT_QEMU_DIRECTORY=/absolute/writable/toolchains/qemu/build
export LD_PRELOAD=/absolute/writable/toolchains/qemu-deps/exec-path.so:/absolute/writable/toolchains/vm-compat.so
```

QEMU uses the Docker-pinned revision, Meson 1.11.1, Ninja 1.13.2 and the same
Ubuntu snapshot. Its execution adapter maps `/opt/dcb/qemu/` to the workspace
binaries. Unset `LD_PRELOAD` for native toolchain configure/build commands.
Installing QEMU does not widen the task's verification target. The native
bootstrap does not provision the full Docker benchmark environment.

## Troubleshooting and handoff

- **Bootstrap failure:** inspect the relevant `ocaml-configure.log`,
  `ocaml-build.log`, `native-packages.log` or `opam-*.log` under the toolchain
  directory. Rerun the bootstrap after addressing the reported error. Do not
  promote an incomplete `.download` file or bypass a checksum check.
- **Missing or wrong toolchain:** source `activate`, then check both versions
  and run the Unix smoke check. A successful `ocamlc -version` alone does not
  establish a usable compiler in this VM.
- **Temporary-path or stack errors:** confirm the activation file was sourced
  for test execution and its temporary directory is writable. Keep the VM
  adapter out of configure probes and the compiler build.
- **Unavailable benchmark tools or downloads:** retain bounded diagnostics
  and report the uncompleted gate explicitly.
- **Benchmark smoke gate reports a missing `/dev/fd/` path:** its Bash process
  substitution requires file-descriptor paths that some restricted VMs do not
  expose. Run the performance gate in the Docker development environment or
  another host providing those paths and Cachegrind. The native host suite and
  compiled leak check can still run in the VM.

Commit completed repository changes locally and report the exact verification
commands and any limitations. Work does not have the macOS `./land` workflow;
use a GitHub branch or PR only when the user authorizes that handoff. Do not
push or merge `main` as part of setup.
