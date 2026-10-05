# OCaml port VM toolchains

This setup is for the restricted Linux x86-64 VM used to restart the OCaml
port. It installs the exact .NET SDK pinned in `global.json` (including F#)
and OCaml 5.5.1, with its source and compiler libraries. This is the upstream stable version selected for the accepted migration plan.
Build OCaml and its C dependencies without the .NET compatibility preload;
apply the VM compatibility preload when executing the oracle or native tests.

```bash
bash scripts/vm/setup-port-toolchains /absolute/writable/toolchains
source /absolute/writable/toolchains/activate
./build --ai
```

The installer checks `Unix.realpath` and child-process creation/waiting before
reusing OCaml. If a previous configure run omitted these Unix operations, it
rebuilds the toolchain instead of accepting its version number alone.

Restore the pinned native dependencies and emulators with:

```bash
bash scripts/vm/setup-ocaml-dependencies /absolute/writable/toolchains
bash scripts/vm/setup-qemu /absolute/writable/toolchains
export OCAMLPATH=/absolute/writable/toolchains/opam/port-5.5.1/lib
export C_INCLUDE_PATH=/absolute/writable/toolchains/qemu-deps/sysroot/usr/include
export LIBRARY_PATH=/absolute/writable/toolchains/qemu-deps/sysroot/usr/lib/x86_64-linux-gnu
env -u LD_PRELOAD /absolute/writable/toolchains/opam/port-5.5.1/bin/dune build --root ocaml -j1
```

QEMU uses the exact Docker-pinned revision, Meson 1.11.1, Ninja 1.13.2 and
Ubuntu snapshot 20260828T000000Z development packages. Package downloads are
checked against the snapshot index SHA256 values and extracted locally. Both
Linux-user targets and the instruction-count plugin are built. Before native
execution tests, source `/absolute/writable/toolchains/activate-native`. This
maps the existing `/opt/dcb/qemu/` executable paths into the workspace and
retains the `/tmp` adapter for fixture subprocesses. Replacing the compatibility
preload with the QEMU adapter alone leaves hardcoded `/tmp` fixtures unable to
create their files. Both adapters are VM-only; native builds still unset them.

The setup needs Bash, Python 3, curl, tar, GCC, and make. All installations,
caches, temporary files, and full OCaml build logs stay in the supplied
directory. OCaml is built serially. The script materializes .NET SDK archive
links as regular files because the VM does not reliably preserve them.

The native compatibility library is scoped to processes using `activate`:

- Recover initial thread stack attributes from mapped stack pages and the
  configured stack resource limit when glibc's procfs lookup fails.
- Redirect hardcoded `/tmp` paths to `PORT_VM_TMPDIR` inside the workspace.

It does not change the SDK, compiler sources, or host filesystem configuration.
`DOTNET_PROCESSOR_COUNT=1` keeps MSBuild from creating additional build nodes
that stall in this VM. Diagnostics are disabled because procfs is unavailable.
These settings are for this VM and should not be applied to a normal host.
