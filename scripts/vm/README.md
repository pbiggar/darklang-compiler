# ChatGPT Work VM setup

Use `Dockerfile` and root `dependencies.lock` as the pinned toolchain sources.
The native-only bootstrap installs OCaml 5.5.1, Dune 3.24.2 and locked OCaml
packages in the supplied writable directory. Ubuntu development prerequisites
are checksum-verified against snapshot 20260828T000000Z and extracted locally.
The bootstrap contains only the native compiler toolchain and its dependencies.

```bash
bash scripts/vm/setup-native-toolchain /absolute/writable/toolchains
source /absolute/writable/toolchains/activate
env -u LD_PRELOAD ./build --ai
./run-tests --ai
dune runtest
```

Downloads, opam state, temporary files and full setup logs remain inside that
workspace directory. The bootstrap checks archive hashes before reuse and
checks both OCaml compiler versions and bytecode/native Unix child-process
operations. Empty compiler outputs left by interrupted links are preserved
under `tmp/ocaml-interrupted.*` before rebuilding them. Configure
probes run without a preload. The VM adapter supplies missing procfs stack
attributes and redirects `/tmp` paths to the writable temporary directory.
Activation also sets `PORT_VM_DUNE_TEST_STAMPS=1`: the adapter keeps empty
`runtest-<32 hex digits>` files beneath the current checkout's `_build/.actions/`
owner-writable. In this VM, old read-only stamps can remain visible after a
successful unlink, preventing Dune from recording a completed test action.
If a legacy stamp reappears read-only, the adapter makes that empty stamp
owner-writable and retries Dune's failed create/truncate open once.
The workaround excludes nonempty files, symlinks, other build outputs and
source files. Bootstrap tests both the enabled behavior and these exclusions.
Do not apply that adapter on a normal host.

For a fresh verification directory, pass its absolute path to Dune's
`--build-dir` and to `./run-tests --build-dir=PATH`. Set
`PORT_VM_DUNE_BUILD_DIR=PATH` when running Dune tests so the same narrowly
scoped empty-stamp workaround covers that directory. Nonempty files,
unselected build directories, source files and symlinks remain excluded.

Cachegrind is an optional benchmark prerequisite, restored from the same
checksum-verified snapshot:

```bash
python3 scripts/vm/setup-native-packages.py /absolute/writable/toolchains --cachegrind
export VALGRIND_LIB=/absolute/writable/toolchains/qemu-deps/sysroot/usr/libexec/valgrind
```

Valgrind requires a VM with `/proc/self/maps`; installing its package cannot
supply that kernel interface. Benchmark argument loading uses regular files
so the smoke gate can also run on VMs without `/dev/fd`.

The default host test suite executes generated x86-64 binaries directly.
The additional runtime executable under `test/runtime-execution/` exercises
both architectures and is an explicit cross-target diagnostic, not part of
the default host suite. Its private helpers live under `test/runtime-support/`;
the helpers do not enter the production build graph.

For cross-target or instruction-count verification, build the Docker-pinned
QEMU emulators and plugin separately:

```bash
bash scripts/vm/setup-qemu /absolute/writable/toolchains
```

QEMU uses the Docker-pinned revision, Meson 1.11.1, Ninja 1.13.2 and the same
Ubuntu snapshot. The generated execution adapter maps the conventional
`/opt/dcb/qemu/` paths to those workspace binaries; set
`PORT_QEMU_DIRECTORY=/absolute/writable/toolchains/qemu/build` and preload its
`qemu-deps/exec-path.so` before `vm-compat.so` when explicitly running those
checks. Native toolchain configure/build commands must unset the QEMU adapter.

A Work VM uses its writable workspace for a checkout and task branch. The
macOS coordination checkout and local merge train are not available there;
see the Work-specific instructions in root `AGENTS.md`.
