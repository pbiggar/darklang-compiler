# Known Issues

Language and standard-library differences from the interpreter are tracked in
the [compatibility overview](../compatibility/overview.md) and its linked
ledgers. Incomplete engineering work without a reproducer is tracked in the
[roadmap](roadmap.md).

## Mach-O text segment capacity

The macOS ARM64 emitter fixes the `__TEXT` segment at 16,384 bytes. Larger
programs fail during emission with `MachO: __TEXT file size 16384 is too small
for code and data ending at ...`. For example, on a macOS ARM64 compiler host:

```bash
./dark benchmarks/problems/factorial/dark/main.dark -o /tmp/factorial
```

The 14-line factorial benchmark should compile, but it exceeds that segment
capacity. Local migration probes reproduce the same rejection and exact
diagnostic in frozen F# and OCaml across all 29 canonical Dark benchmarks.
Small arithmetic and function programs produce identical complete Mach-O
images. This is an existing backend limitation, not an OCaml regression.

The responsible code is
[`Binary_Generation_MachO.ml`](../../ocaml/lib/backend/arm64/Binary_Generation_MachO.ml).
Fixing segment sizing is outside the faithful port's scope. No failing macOS
regression test is added to this migration: the agreed scope excludes fixes
to existing compiler bugs, and this Linux workspace cannot execute the result
or verify Apple signing. Add a correct-behavior test when fixing this issue.

Add an issue here only when it has:

- a minimal source reproduction;
- expected and actual behavior;
- the affected target or compiler pass; and
- a focused failing test, or a reason the test cannot yet be committed.

Move the durable behavior into the relevant compiler or compatibility document
and delete the issue entry when it is fixed. Git history retains the
investigation.
