# Application benchmark research

Microbenchmarks remain useful for isolating compiler regressions, but mature
language implementations also use larger programs to expose interactions among
parsing, allocation, collections, strings, dispatch, and control flow.

## Practices in other language projects

- [rustc-perf](https://github.com/rust-lang/rustc-perf) measures the Rust
  compiler on a corpus of real crates in check, debug, and optimized build
  modes. This is the strongest model once Dark can compile a representative
  Dark package graph.
- [Python's pyperformance](https://github.com/python/pyperformance) combines
  focused workloads with application-shaped programs and third-party-library
  behavior. It favors repeatable in-process workloads over external services.
- [Swift's benchmark suite](https://github.com/swiftlang/swift/tree/main/benchmark)
  distinguishes single-source tests from multi-source programs, retaining both
  diagnostic precision and whole-program coverage.
- [LLVM's test suite](https://github.com/llvm/llvm-test-suite) likewise contains
  SingleSource, MultiSource, and external application suites. External suites
  offer realism but add licensing, availability, and reproducibility costs.
- [Go's x/benchmarks repository](https://github.com/golang/benchmarks) includes
  the `sweet` application benchmarks, using pinned real packages and workloads
  to complement the standard library's package-local benchmarks.

The common pattern is a layered suite: keep small kernels, add pinned real
programs, and treat large external suites as a separate reproducibility and
licensing problem. Compiler-throughput corpora are especially valuable, but
Dark does not yet have a sufficiently large stable package ecosystem for that
to be the first application benchmark.

## Selected first application

TinyTemplate 1.2.1 is small enough to audit and port completely while still
behaving like a library used by an application. Its compiler and interpreter
exercise nested data, parsing, instruction construction, template and formatter
registries, path lookup, escaping, conditionals, iteration, scoping, and calls.
The benchmark renders an inventory report through that complete surface.

The Rust implementation is the complete published 1.2.1 crate source, not a
facsimile or a dependency on a moving release. The Dark implementation is a
functional port of the same value model, grammar, public registry operations,
rendering semantics, and error categories. Routine and quick modes invoke the
same source with profile-declared row and repetition arguments.

Future additions should prefer another pinned real codebase with a different
shape—such as a parser, serializer, or persistent-data application—before
adding more template workloads. A Dark self-hosting or multi-package compile
corpus should supersede synthetic compiler-throughput proxies once available.
