# Darklang compatibility ledger

The ledger lists test files and individual tests that fail in the compiler.

See the [current test results](current-audit.md) for the complete execution of
all whole-file and individual-test exclusions. Each failing file links to its
individual test identities and observed diagnostics. Passing excluded tests
and stale exclusion entries are recorded separately.

[Upstream gate inventory](upstream-test-inventory.md) records the runner’s
configured exclusions. A gate alone is not evidence that a test fails.

`Builtin.testRuntimeError` is unsupported interpreter test infrastructure.
Compiler-owned library code, benchmarks, and tests use the public
`Builtin.crash : String -> Never` operation. Imported interpreter assertions
that depend on `testRuntimeError` are retained and excluded individually; their
identities are recorded in the [failure ledger](current-audit.md).
