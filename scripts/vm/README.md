# OCaml port VM toolchains

This setup is for the restricted Linux x86-64 VM used to restart the OCaml
port. It installs the exact .NET SDK pinned in `global.json` (including F#)
and OCaml 5.3.0, with its source and compiler libraries. The OCaml version
is provisional until the previous conversation export is available.

```bash
bash scripts/vm/setup-port-toolchains /absolute/writable/toolchains
source /absolute/writable/toolchains/activate
./build --ai
```

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
