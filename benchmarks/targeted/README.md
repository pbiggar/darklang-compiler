# Targeted Compiler Benchmarks

These diagnostic suites isolate compiler/runtime subsystems. They are separate
from the audited application workloads in `../problems`, do not participate in
the canonical `full` profile, and never update its baselines.

Run one suite and write its measurements to a JSON file:

```bash
./benchmarks/targeted/run.sh json /tmp/json-benchmark.json
./benchmarks/targeted/run.sh integer128 /tmp/integer128-benchmark.json
```

Run every targeted suite into one directory:

```bash
./benchmarks/targeted/run.sh all /tmp/dark-targeted-benchmarks
```

Each case is compiled once for measurement, checked for its exact expected
output, sampled seven times, and compiled separately with leak checking. Results
include compilation time, executable size, median runtime, normalized operation
cost where applicable, and leak-check status.

The `integer128` suite covers signed and unsigned arithmetic, comparisons and
bitwise operations, decimal parsing/formatting, UUID parsing/formatting,
generation and equality, and collection storage/copying. It is intended to
measure the fixed-block `Int128` and `UInt128` representation and guard its
arithmetic, textual-boundary, UUID, ownership, and collection costs.

## Standard-mode compilation latency

The compile-latency probe starts a fresh compiler process for every sample and
compiles a representative program through the ordinary CLI. It therefore does
not benefit from the batch compiler's prepared context or in-process caches:

```bash
./build --ai
python3 benchmarks/targeted/compile-latency/measure.py \
  --output /tmp/dark-compile-latency.json
```

The JSON result records both end-to-end process wall time and the compiler's
reported pipeline time for every sample, together with their medians, the
source hash, and compiler commit. This separates standard-library/process setup
from work on the requested program. It is a diagnostic latency measurement
rather than the canonical runtime-performance gate.

## Closed-list array diagnostics

The independent `list-array` comparison uses built Debug compilers from two
worktrees and pinned QEMU instruction counts. It checks unique reuse, surviving
old versions, and a captured-list persistent fallback. Both compilers receive
the exact same source files. Each case also runs with leak checking. Reports
include source/assembly hashes, commit and dirty-state attribution, instruction
counts, compile times, and executable sizes. They do not update canonical
snapshots; single compile-time samples are diagnostic, not a timing gate.
Build both compilers before starting the comparison; changing either assembly
during measurement invalidates the run. Completed cases are saved incrementally,
but only a report with `complete: true` covers all `requested_cases`.
Normal execution must succeed for both compilers; leak checking must succeed
for the candidate. Baseline leak-instrumentation failures are reported and
retained in JSON independently of the ordinary instruction comparison.
Cross-target execution controls additionally cover empty/singleton inputs,
retained aliases, scalar captures, and callback-effect ordering.

```bash
python3 benchmarks/targeted/list-array/compare.py \
  --baseline=/path/to/baseline-worktree --candidate=/path/to/candidate-worktree \
  --target=arm64 --output=/tmp/list-array-arm64.json
# Repeat with --target=x86_64 and a different output file.
```

`large-unique` and `large-shared` exercise the mapped-array loop kernels with
64-element source literals. The report includes individual native wall-time
samples and their median when the requested target matches the Linux host
(`--native-runs`, default 5). These timings include process startup; use the
repeated workloads, not tiny controls, to assess mapping syscall costs.
`runtime-unique` and `runtime-shared` construct 256/257-element repeats using
function-parameter counts. `runtime-small` exercises lengths 0–3 for 10,000
iterations to expose mapping overhead above process-startup noise;
`cross-function` routes the same workload as `main` through a selected
map/reverse helper and must retain the closed pipeline's instruction count;
`runtime-effects` checks count/value evaluation,
callback order, and normalization of zero and huge negative counts on both
targets.
`branch-unique` exercises consuming transformations on mutually exclusive
paths. `branch-shared` retains the original list after a scalar join, requiring
a copy only on the mutating path. Both alternate branches over 1,000 iterations
with 128-element runtime buffers and require candidate leak checks on both
native backends.
`mapped-buffer.dark` is a candidate-only internal probe: compile with
`--allow-internal --emit-result --leak-check` and expect `5376` with no leaks.

The separate allocator probe must print `434` with no leak report on a compiler
supporting this storage class. It deliberately inspects allocator contents and
is not a source-semantics benchmark. On the persistent baseline it prints `1431`.

```bash
./dark --allow-internal --emit-result --leak-check --disable-opt-inline \
  benchmarks/targeted/list-array/storage-probe.dark -o /tmp/list-array-probe
/tmp/list-array-probe
```
