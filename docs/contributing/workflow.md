# Adding Features to the Dark Compiler

Build the pinned OCaml toolchain with `./build --ai` and run the already-built
suite with `./run-tests --ai`. Follow [the coding guidelines](coding-guidelines.md)
and [the source organization](../compiler/source-organization.md).

## Start with language behavior

Add a focused failing E2E to `src/Tests/e2e/` before changing compiler behavior.
Include a representative successful input and the relevant boundary case.
Use `compileerror=` when a failure must occur ahead of time. Native emission
must preserve standalone executables and the selected target's ABI.

## Follow the pipeline

For a new operator, update both the implementation and its `.mli` interface:

1. Add the written syntax in `ocaml/lib/frontend/interpreter/WrittenTypes` and
   the parser's precedence/normalization handling.
2. Add the semantic operation to `ocaml/lib/AST` and resolve it at the written
   and checked frontend boundaries.
3. Update checked preparation, HIR and ANF lowering where the new operation
   needs a distinct representation or ownership behavior.
4. Add the corresponding MIR/LIR operation and define its use/definition facts.
5. Update instruction selection, encoding and runtime helpers for each target
   included in the change.
6. Add the new syntax to formatter/DSL support where it is independently
   observable. Let exhaustive-match compiler warnings identify every affected
   dispatcher rather than adding a catch-all default.

For a standard-library feature, add its Dark implementation to
`ocaml/share/stdlib/`, add the ordered source registration in
`ocaml/lib/driver/StdlibCompilation.ml`, and register the file in the share
installation stanza. Public generic uses should rely on inference unless
explicit type arguments are necessary.

## Validate the final change

Rebuild after source changes. Run the complete applicable host suite and the
verification gates in [verification.md](verification.md). Use another target
only when the task explicitly includes it. Keep verbose artifacts under
`TestResults/`; report bounded failure excerpts and their paths.

Compiler source is in `ocaml/lib/`, translated unit tests and tooling are in
`ocaml/tests/`, and unchanged language/DSL fixtures remain in `src/Tests/`.
The retired F# compiler is available in Git history; migration comparisons
materialize that reference into a disposable workspace.
