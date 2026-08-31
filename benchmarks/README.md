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
