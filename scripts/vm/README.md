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
checks both the OCaml version and Unix child-process operations. Configure
probes run without a preload. The VM adapter supplies missing procfs stack
attributes and redirects `/tmp` paths to the writable temporary directory.
Do not apply that adapter on a normal host.

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
