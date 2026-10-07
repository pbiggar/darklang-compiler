# OCaml Coding Guidelines

The compiler is implemented in OCaml 5.5.1. Repository-specific requirements
are in [AGENTS.md](../../AGENTS.md).

## Interfaces and state

Each implementation has an explicit `.mli` interface. Preserve module
responsibilities and encode distinct states in closed variants. Use options
for semantic absence and results for recoverable errors. Complete migrations
instead of retaining hidden fallbacks or superseded representations.

Compiler algorithms should remain functional. Local mutation is appropriate
for parser cursors, output buffers and bounded compilation caches. Keep that
mutation inside the owning module; do not add compiler parallelism.

Use `Crash.crash` for an impossible internal state and explicit matching for
lookups. Host adapters may preserve host-API exceptions when their callers
catch them and return a recoverable result. Do not use partial option getters.

```ocaml
let ( let* ) = Result.bind
let load_and_parse path =
  let* source = load_source path in
  Result.map_error (fun message -> "Unable to parse " ^ path ^ ": " ^ message)
    (parse source)
```

## Semantic compatibility

Use explicit fixed-width integer operations and `FixedInteger` for Dark numeric
semantics. OCaml machine integers do not implement every Dark integer width.
Use the shared text and float-formatting utilities for Unicode source ranges
and precise source literals. Use OCaml libraries directly for I/O, JSON,
hashing and clocks; keep schema validation beside its consumer. Compiler name maps use `StringOrder` and
opaque function identities use `FunctionIdMap`.

Preserve ahead-of-time match validation. Exhaustiveness, pattern validity,
binding consistency, guards and arm result types must fail during compilation.

## Checks

Build with `./build --ai`; warnings are errors. Run the already-built host suite
with `./run-tests --ai`. `dune runtest` runs the additional
regression checks and does not replace the complete host suite.

Create a focused failing E2E before changing observable compiler behavior.
Test results, diagnostics and language behavior rather than incidental helper
names. Use benchmarks for performance changes. Preserve useful file-purpose
comments and explain assumptions at their owning boundary.
