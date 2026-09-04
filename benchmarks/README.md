# Darklang Benchmarks

This system compares full Dark programs with human-audited, algorithmically
equivalent Rust references. The routine profile is the canonical performance
gate; diagnostic workloads do not contribute to its ratio.

## Run

```bash
# Verify the current compiler without changing tracked results.
./benchmarks/run_benchmarks.sh --verify routine

# Record a complete routine result. Only an aggregate improvement advances the
# architecture-specific Dark snapshot and regenerates RESULTS.md.
./benchmarks/run_benchmarks.sh routine

# Refresh audited Rust references as a separate, deliberate operation.
./benchmarks/run_benchmarks.sh --refresh-baseline=rust routine

# Establish a compatible Dark snapshot after an intentional contract reset.
./benchmarks/run_benchmarks.sh --reset-dark-baseline routine

# Check reduced Dark/Rust pairs or an ARM64-hosted x86_64 guest track.
./benchmarks/quick_check.sh --build
./benchmarks/x86_64_check.py --help
```

Run these inside the supported development environment. Dependencies and
implementation details are provided by the devcontainer; do not install them
on the host for repository work.

## Typed JSON diagnostics

The test executable provides two opt-in JSON diagnostics. Build it first with
`./run-tests --ai --build-only`, then run:

```bash
bin/Tests/Debug/net10.0/Tests --ai --filter=json \
  --timings-json=/tmp/json-timings.json \
  --codegen-profile-json=/tmp/json-codegen.json

bin/Tests/Debug/net10.0/Tests \
  --json-benchmark=/tmp/json-benchmark.json
```

The timing JSON includes exclusive top-level compiler/test phases plus
diagnostic overlapping subphases for JSON planning and ARM64 code generation.
The codegen profile separates metadata analysis, function collection, runtime
helper generation, instruction-list assembly, and the final symbolic peephole
pass; it also reports ARM64-function and canonical JSON-plan cache hits.
Suite-context timings split test/preamble planning from stdlib-specialization
and preamble-build overhead.

The codegen profile attributes ARM64 cache misses by function and reports the
remaining whole-program codegen time separately. Profiling is disabled unless
the output flag is present. The focused benchmark validates and times scalar,
1 KiB flat-record, 1 KiB collection, and 64 KiB nested record/sum decodes; it
also records executable size and performs a separate leak-check build of each
case. These are diagnostic comparisons and do not replace the canonical
routine benchmark gate.

## Contract

`PARITY.json` locks full and quick Dark/Rust source hashes, expected output, and
comparability. The ordered files under `profiles/` define profile membership.
The runner rejects a missing or incompatible snapshot rather than inferring a
replacement. Targeted, partial, hyperfine, and diagnostic runs cannot advance
or reset the canonical Dark snapshot.

Routine decisions compare the product of positive instruction counts exactly;
the displayed equal-weight geometric `current/baseline` ratio is below 1 for an
improvement and above 1 for a regression. Individual regressions may be offset
by larger improvements across the routine suite.

## Current references

- [`RESULTS.md`](RESULTS.md) is the generated, current comparison from the
  compatible Dark snapshot and audited Rust instruction counts.
- [`BASELINES.md`](BASELINES.md) is the generated audited Rust reference data.
- `baselines/dark-*-routine-cachegrind.json` is the canonical recoverable Dark
  state. It includes profile, architecture, measurement policy, contract,
  compiler attribution, timestamp, and ordered counts.

`infrastructure/history_updater.py` is the sole writer for the generated tables
and snapshots. Benchmark run output is diagnostic evidence, not a repository
history record.
