# ChatGPT app VM setup and recovery

ChatGPT app / Work VMs are disposable. GitHub is the recovery source for work;
local commits, toolchains, logs and build outputs can disappear between turns.
The local Codex setup uses macOS worktrees, `./land`, mergetrain and the
integrator. None of those handoff tools apply in the app. Root
[`AGENTS.md`](../../AGENTS.md) owns the rules for both environments.

## Start or recover a checkout

Use the writable workspace supplied by the conversation, not a remembered VM
path. Keep the checkout and toolchain as siblings. The examples below use a
placeholder; replace it with the actual absolute workspace path.

For a **new task**:

```bash
workspace=/absolute/writable/workspace
branch=chatgpt/your-task
cd "$workspace"
git clone https://github.com/pbiggar/darklang-compiler.git
cd darklang-compiler
git switch -c "$branch" origin/main
```

After a **VM reset**, recover the branch reported in the previous turn:

```bash
workspace=/absolute/writable/workspace
branch=chatgpt/your-existing-task
cd "$workspace"
git clone --branch "$branch" https://github.com/pbiggar/darklang-compiler.git
cd darklang-compiler
git status --short
git log -5 --oneline
git diff --stat origin/main...HEAD
```

If the checkout survived, inspect its branch and working tree first. Preserve
uncommitted work; do not reset or recreate it. Read the existing task changes
and continue from them. Read `docs/index.md` and `AGENTS.md` before editing.

## Preserve progress before setup and between turns

Commit and push intended repository edits before a long operation, after
meaningful progress, and before ending a turn. Use explicit paths when staging.
A checkpoint may contain unfinished work; its message and report must say so.
Do not include credentials, toolchains, downloads or generated results.

```bash
git add AGENTS.md scripts/vm/README.md # Replace with the intended changed paths.
git commit -m 'Checkpoint: describe progress and remaining work'
git push -u origin HEAD
branch=$(git branch --show-current)
local_commit=$(git rev-parse HEAD)
remote_commit=$(git ls-remote origin "refs/heads/$branch" | cut -f1)
test "$local_commit" = "$remote_commit"
```

Record the branch URL, pushed SHA, verification results and remaining work in
the response. Do not force-push or push `main`. A branch push preserves work;
it does not merge it. If Git authentication is unavailable, the authorized
GitHub connector can create the branch and commit the intended files. Verify
its remote commit/tree before claiming preservation. Respect any approval
rejection; do not switch transports to bypass one. Report a blocked push
prominently because a local commit will not survive a VM reset.

If committing fails because this VM has no Git identity, set a truthful
agent identity for this checkout only (`git config user.name` and
`git config user.email`). Do not change global settings or impersonate the user.

## Bootstrap the native toolchain

The rootless bootstrap supports Linux x86-64. Other hosts use the
[`Dockerfile`](../../Dockerfile). It requires `bash`, `gcc`, `make`, `curl`,
`tar`, `sha256sum`, `python3` (3.9+ with HTTPS and lzma), `dpkg-deb`, `flock` and
`grep`, plus the host C compiler's normal libc headers and linker. No `sudo`,
Docker daemon or system package installation is used. The VM must allow HTTPS
access to the pinned GitHub archives/releases, Ubuntu snapshot, opam repository
and package source hosts; cloning successfully does not prove downloads work.

From the recovered or new checkout:

```bash
bash scripts/vm/setup-native-toolchain "$workspace/toolchains"
source "$workspace/toolchains/activate"
bash scripts/vm/check-native-toolchain "$workspace/toolchains"
env -u LD_PRELOAD ./build --ai
./run-tests --ai
dune runtest
```

`workspace` must be set again in a fresh shell. Likewise, source `activate` in
**every new shell/tool invocation** before building or testing; activation in
one command does not activate future commands. Setup runs only setup and smoke
checks, never the repository build or suite. `./run-tests` executes an
already-built binary, so build first and rebuild after source changes.

`Dockerfile` supplies the OCaml version and archive hash;
[`dependencies.lock`](../../dependencies.lock) supplies Dune and OCaml package
versions. Do not copy a second set of OCaml/Dune pins into setup commands.
Ubuntu prerequisites come from frozen snapshot `20260828T000000Z`; package
archives are verified against its package-index hashes. The opam executable
is version 2.6.0 with a verified checksum.

The script prints its current stage, keeps complete logs in the toolchain
root, installs serially, and prevents concurrent setup in the same directory.
Rerun the same command after an interruption: valid cached downloads and a
working compiler are reused; partial/corrupt archives are replaced; a failed
compiler build starts from clean extracted sources. Activation is written
atomically after installation. Do not trust an old activation file until the
setup or check command succeeds for the current lock.

`check-native-toolchain` verifies both OCaml compilers, Dune, every locked opam
package, native executable compilation, Unix child processes, temporary-file
operations, GMP/SQLite headers and runtime libraries, and pkg-config discovery.
It creates and removes only its own smoke-test directory; it does not build
the repository. Do not accept version output as proof the toolchain works.

Downloads, opam state, sources, logs and a writable `TMPDIR` stay under the
supplied toolchain directory. The VM adapter supplies missing procfs stack
attributes and redirects `/tmp` paths to that temporary directory. Configure
probes run without a preload. Do not use this adapter on a normal host.

## Troubleshooting

| Symptom | Next step |
| --- | --- |
| Missing prerequisite or unsupported architecture | Read the preflight error; use a compatible VM or the Docker environment. Do not install outside permitted paths. |
| Setup interrupted | Rerun the same setup command and directory. Cached files are checked before reuse. |
| Download denied, timeout, TLS error or HTTP 502 | Read the named download log. Check permitted network access; report blocked setup and preserve the branch. Never disable TLS or checksum checks. |
| OCaml configure/build/install fails | Read the named log, starting with the bounded tail printed by setup. Keep serial compilation and the VM adapter; rerun setup after fixing the cause. |
| Another setup holds the lock | Wait for that invocation to end. The OS releases the lock when it exits; deleting the lock file is unnecessary and unsafe. |
| Existing opam switch has a different compiler | Use a new sibling toolchain directory, then source its activation. Keep the old directory until recovery succeeds. |
| Wrong version, missing package, Unix or native-library check fails | Rerun setup against the current checkout, then rerun the check. Do not use a partially working environment for test claims. |
| `dune` missing in a later command | Source the activation file in that command's shell. |
| VM disappeared | Clone the pushed task branch; bootstrap again in the new workspace. Local artifacts are not recovery state. |

To validate changes to the bootstrap itself without downloading or building:

```bash
python3 scripts/vm/test_setup.py
bash -n scripts/vm/setup-native-toolchain scripts/vm/check-native-toolchain
```

After changing setup, also attempt a fresh bootstrap, run the environment
check, and rerun setup to verify reuse when network access permits. Report
which checks actually ran and any download blocker.

## Optional cross-target tooling

The default host suite executes generated x86-64 binaries directly. The
additional runtime executable under `test/runtime-execution/` exercises both
architectures and is an explicit diagnostic, not part of the default suite.
Its helpers under `test/runtime-support/` do not enter the production graph.

For explicitly requested cross-target or instruction-count verification:

```bash
bash scripts/vm/setup-qemu "$workspace/toolchains"
```

QEMU uses the Docker-pinned revision, Meson 1.11.1, Ninja 1.13.2 and the same
Ubuntu snapshot. The generated execution adapter maps conventional
`/opt/dcb/qemu/` paths to workspace binaries. Set
`PORT_QEMU_DIRECTORY="$workspace/toolchains/qemu/build"` and preload
`qemu-deps/exec-path.so` before `vm-compat.so` when running those checks.
Native toolchain configure/build commands must unset the QEMU adapter.
Do not spend fresh-VM setup time building QEMU for ordinary host tests.
