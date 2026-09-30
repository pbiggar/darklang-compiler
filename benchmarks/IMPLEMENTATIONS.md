# Reference implementation provenance

All seven reference languages implement the same 29 parameterized workloads. The interpreter executes all 29 existing Dark sources
through the argument/compatibility adapter. `profiles.json` owns arguments and
expected stdout; sources are shared between quick and full profiles.

Existing implementations were used where available. The table records direct
upstream adaptations; other ports translate the existing Rust/Python/OCaml
implementations in `problems/<workload>/`, including their algorithms, constants,
checksums, and repetition counts. These are adaptations to our workload, not
reproductions of the Benchmarks Game's complete execution protocol.

| Language/workload | Upstream source | Adaptation |
|---|---|---|
| Haskell binary_trees | [Koka's Game implementation](https://github.com/koka-lang/koka/blob/046254c139ea33ce132823599f860f5395c1de21/test/bench/haskell/binarytrees.hs), Don Stewart, Roman Kashitsyn, Izaak Weiss | Keep strict tree allocation and traversal; replace parallel depth sweep with requested depth/repetition and one total. |
| Haskell quicksort | [Koka qsort](https://github.com/koka-lang/koka/blob/046254c139ea33ce132823599f860f5395c1de21/test/bench/haskell/qsort.hs) | Keep STArray sorting and median-of-three partitioning; use the shared LCG, argument convention, and weighted checksum. |
| Haskell nbody | [Game implementation mirrored by Energy-Languages](https://github.com/greensoftwarelab/Energy-Languages/blob/1356528173d6bb07fb2512037c0ed8e2279ce440/Haskell/n-body/nbody.ghc-2.ghc), Branimir Maksimovic | Keep planet constants and pointer-based integration; print truncated final energy only, free allocation, and update layout for current GHC. |
| Haskell fannkuch | [Game implementation mirrored by Energy-Languages](https://github.com/greensoftwarelab/Energy-Languages/blob/1356528173d6bb07fb2512037c0ed8e2279ce440/Haskell/fannkuch-redux/fannkuchredux.ghc-3.ghc), Louis Wasserman | Keep permutation/flip core; use one sequential worker, print maximum flips, and add modern Semigroup instance. |
| Haskell spectral_norm | [Game implementation mirrored by Energy-Languages](https://github.com/greensoftwarelab/Energy-Languages/blob/1356528173d6bb07fb2512037c0ed8e2279ce440/Haskell/spectral-norm/spectralnorm.ghc-4.ghc), Don Stewart, Gabriel Gonzalez, Ryan Trinkle, Louis Wasserman | Keep matrix products/eigenvalue accumulation; remove threading and GHC internals, parameterize iterations, and print scaled integer. |
| Koka binary_trees; Roc binary_trees | [Koka binarytrees](https://github.com/koka-lang/koka/blob/046254c139ea33ce132823599f860f5395c1de21/test/bench/koka/binarytrees.kk) | Use recursive make/check; remove task scheduling and depth sweep, supply depth/repetitions, print one count. |
| Koka tak; Haskell tak; Roc tak | [Koka tak-int](https://github.com/koka-lang/koka/blob/046254c139ea33ce132823599f860f5395c1de21/test/bench/koka/tak-int.kk) | Keep recursive Takeuchi function; parameterize arguments and repetitions. |
| Roc quicksort | [Roc Quicksort module](https://github.com/roc-lang/roc/blob/d73ea109cc21442da01387c1e5e911607c74692d/crates/cli/tests/benchmarks/Quicksort.roc) | Keep upstream sorting module; add the shared LCG and checksum. |
| Roc nqueen | [Roc n_queens](https://github.com/roc-lang/roc/blob/d73ea109cc21442da01387c1e5e911607c74692d/crates/cli/tests/benchmarks/n_queens.roc) | Keep list-based search; replace stdin host with argument/stdout platform. |

Koka-derived code is covered by [Apache-2.0](licenses/Koka-Apache-2.0.txt),
copyright Microsoft Research and Daan Leijen. Roc-derived code is covered by
[UPL-1.0](licenses/Roc-UPL-1.0.txt), copyright Richard Feldman and subsequent Roc
authors. The Energy-Languages mirror provides an
[MIT notice](licenses/Energy-Languages-MIT.txt); Game contributor attribution is
retained in the Haskell sources.

## Comparison limits

Exact stdout validation is required for every recorded measurement. It does
not certify equivalent optimization opportunities. These ports remain
unaudited in `REFERENCE-PARITY.json`; their ratios are informational.

In particular, Roc's upstream quicksort partitions an array-like list using a
last-element pivot, while Rust uses middle-pivot filtering. Haskell quicksort
retains the upstream mutable-array implementation and median-of-three pivot. Roc's upstream
n-queens constructs lists of solutions, while Rust searches bitmasks and counts
solutions. Haskell fannkuch retains the Game's permutation/flip machinery.
Koka uses its standard arbitrary-precision integers for most workloads;
Merkle hashes explicitly wrap at 64 bits. Haskell uses strictness and explicit
IO repetitions to avoid sharing results across repeated runs, with
`-fno-full-laziness` recorded in build flags. The combined
`-fno-full-laziness -fno-cse` flags triggered a GHC 9.10.3 optimization bug
(wrong FFT output and a `SpecConstr` runtime error in the template renderer);
the ports validate with `-fno-full-laziness` alone. Data-structure and
optimization differences need human review before marking parity comparable.

## Validated toolchains

The new native ports were validated against GHC 9.10.3, Koka 3.2.9 (C target),
and Roc nightly 2025-09-09 (`d73ea10`) with basic-cli 0.20.0. The basic-cli
platform URL in each source is content-addressed. Roc downloads that platform
on its first build; prepare the cache before an offline refresh. Sources use
that Roc generation's syntax. The adapter recognizes both legacy and newer
compiler flag interfaces, but a compiler syntax migration may still require
source changes. A failed build preserves the previous snapshot.

## Eight application and string workloads

The FFT, Huffman, Myers diff, raytracer, regex-lite, string equality,
TinyTemplate, and Warden ports adapt the existing repository Rust and Dark
implementations. These are custom parameterized workloads, not Benchmarks Game
programs. Upstream searches did not locate compatible application ports that
preserved these inputs and checksums. The recursive FFT is radix-2
Cooley–Tukey; Python uses native complex numbers, OCaml and Haskell their
standard complex types, and Node, Roc, and Koka explicit complex values.

Python Huffman uses `heapq` and `Counter`; the other ports use deterministic
sorted queues and explicit trees. All retain weight/minimum-symbol tie-breaking,
MSB-first bit encoding, actual decoding, and roundtrip validation. Myers ports
retain the complete frontier trace checksum, including every endpoint in the
final layer: Python/Node use maps, OCaml/Haskell ordered integer maps, and
Roc/Koka indexed frontiers. Raytracer ports retain double precision, the fixed
sphere scene, intersection/shadow tests, and pixel quantization.

Python/Node regex ports use their standard regex engines with matching anchored
at each byte position (all workload text is ASCII). OCaml/Haskell/Roc/Koka
parse the small grammar into terms and search valid repetition endpoints.
These implementation differences are informational until parity review.
Warden ports lex once and recursively parse/evaluate each statement for every
iteration; they do not precompute an AST or replace evaluation with a formula.
String equality uses the language's standard equality operation for all ten
cases per round; compiler optimizations and representations can differ.

The six new TinyTemplate ports implement the grammar exercised by this workload
as a parsed AST and template/formatter registry: comments, substitutions,
conditionals, loops and metadata, scoped `with`, template calls, whitespace
trimming, HTML escaping, currency, and unescaped formatting. They are workload
ports, not complete replacements for the vendored Rust TinyTemplate 1.2.1 crate
or its full API, error diagnostics, and JSON formatting semantics. Templates
are parsed and report data built once; rendering and checksum run per iteration.
Python/Node use dictionaries/objects, OCaml buffers and associations,
Haskell immutable maps, Roc dictionaries, and Koka algebraic values and lists.

The interpreter refresh is pinned to Darklang CLI v0.0.35
(`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`), which accepts the shared
benchmark sources through the compatibility adapter. Upgrading the interpreter
may require updating that adapter; its source is included in measurement
identity so an adapter change invalidates stored interpreter counts.
