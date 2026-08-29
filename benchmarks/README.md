# Darklang Benchmarks

Cross-language benchmarking system to measure compiled Darklang performance
against human-audited Rust reference implementations. Other language programs
remain available for diagnostics but do not contribute canonical ratios.
See [APPLICATION-BENCHMARKS.md](APPLICATION-BENCHMARKS.md) for the research and
selection criteria behind full-application workloads.

## Prerequisites

Install before running benchmarks:

```bash
# valgrind (instruction count benchmarks)
sudo apt-get install valgrind  # Linux
brew install valgrind  # macOS (may require extra setup)

# hyperfine (timing benchmarks)
brew install hyperfine  # macOS
sudo apt-get install hyperfine  # Linux

# Rust compiler
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Python 3 (used by benchmark orchestration)
python3 --version
```

## Quick Start

```bash
# Record the routine profile. Improved Dark suites advance automatically;
# equal suites keep the snapshot; regressed suites are logged and fail.
./benchmarks/run_benchmarks.sh

# Run every benchmark, including diagnostic-only workloads, without recording it
./benchmarks/run_benchmarks.sh all

# Run a specific benchmark
./benchmarks/run_benchmarks.sh fib

# Run with hyperfine for timing instead
./benchmarks/run_benchmarks.sh --hyperfine
./benchmarks/run_benchmarks.sh --hyperfine fib

# Run benchmarks in parallel
./benchmarks/run_benchmarks.sh --jobs 4

# Verify the canonical routine profile without updating tracked files
./benchmarks/run_benchmarks.sh --verify routine

# Establish a Dark baseline after an intentional contract/policy reset
./benchmarks/run_benchmarks.sh --reset-dark-baseline routine
./benchmarks/quick_check.sh --reset-dark-baseline

# Validate every reduced Dark/Rust pair and compare instruction counts
./benchmarks/quick_check.sh --build
```

## Benchmark Modes

### Parity contract

`PARITY.json` records whether each full and quick Dark/Rust pair is comparable
and locks the SHA-256 hashes of both audited sources and their expected output.
The full Rust programs define the reference workload: parity fixes change Dark
or mark the pair non-comparable. A correctness fix may change Rust only while
preserving the original workload and algorithm. Every run checks the hashes
before recording numbers. The ordered `profiles/routine.txt`,
`profiles/quick.txt`, and `profiles/quick-fast.txt` files make suite membership
and discovery order contractual. The routine profile accepts only `comparable` full
pairs; reduced, Dark-only, and incomparable programs remain available for
diagnostics without contributing to canonical ratios.

Rust single-file programs are built with `rustc -C opt-level=3`; vendored
multi-file applications use a release Cargo profile with `opt-level = 3` and
standardized `benchmark-full`/`benchmark-quick` binaries. Dark is built with its
normal native compiler pipeline. Fairness here means equivalent
algorithms, workloads, observable results, and source-level optimization
opportunities—not forcing either compiler to miss legitimate optimizations.

### Cachegrind Mode (default)

Uses **Valgrind Cachegrind** to count instructions. Slower (~50x) but deterministic - same input always produces identical counts. Useful for:

- Detecting performance regressions in CI
- Comparing instruction efficiency between languages
- Tracking optimization improvements over time

This is the primary way we are tracking performance.

The `routine` profile is the canonical comparable benchmark set and currently
contains 20 pairs. It excludes incomparable nsieve and the reduced Dark
fannkuch workload. Binary trees now uses the same recursive allocation and
traversal shape as Rust. Full-size quicksort and spectral norm are included. A
completed routine Cachegrind run records its measurements in
the architecture-specific canonical JSON snapshot and `HISTORY.md`; targeted
runs and `all` are diagnostic and do not update canonical files. `RESULTS.md` is
presentation regenerated from that routine snapshot plus `BASELINES.md`'s
audited Rust references. Recording accepts an optional `--machine` ID from
the registry in `HISTORY.md`; omitted machine metadata is left blank rather than
guessing the runner's identity. Verification does not update history.

Dark snapshots live under `baselines/` and contain a schema version, suite and
profile identity, normalized architecture, measurement-policy identifier,
ordered names/counts, full compiler attribution, timestamp, and SHA-256 workload
contract. The contract digest is derived from the ordered profile and relevant
PARITY hashes/statuses. A profile, PARITY contract, Cachegrind-policy, extraction,
schema, or architecture mismatch is never inferred or repaired: the command
fails with explicit reset instructions. Only architectures with a trusted full
run have a snapshot.

All Dark decisions use the equal-weight geometric suite ratio, displayed as
`current/baseline`: below 1 is improved, 1 is equal, and above 1 is regressed.
Classification itself is exact: the tools compare arbitrary-precision integer
products, then use logarithms only to display the ratio. Consequently a slower
individual benchmark can be compensated by larger gains elsewhere. Every row's
absolute and percentage delta is still reported.

### Quick correctness and regression mode

Every problem has `dark/quick.dark`, `rust/quick.rs`, and a shared
`quick_expected_output.txt`. Quick variants preserve their full implementation's
algorithm and optimization opportunities while reducing only workload
parameters. `quick_check.sh` builds the current Dark compiler, compiles Rust with
the same `rustc -C opt-level=3` setting as the full suite, validates both native
outputs, and counts both binaries under the same Cachegrind options. The entire
selected run is validated before comparison or mutation. A complete improved
`quick` run atomically replaces the whole architecture snapshot; equality passes
without rewriting; regression fails and preserves the stronger snapshot.
Missing, malformed, incomplete, or incompatible snapshots fail and require an
explicit complete `--reset-dark-baseline` run. Rust counts and Dark/Rust ratios
remain correctness/comparison diagnostics and never enter the Dark decision.
Pass `--decision-json=PATH` to retain the machine-readable quick decision;
routine runs retain the same document in their generated results directory.

`--fast` uses the declared five-workload `quick-fast` profile and projects those
names from the compatible complete quick snapshot. It applies the same aggregate
pass/fail decision but never advances or resets the complete snapshot. Fast,
targeted, `all`, hyperfine, failed, and partial runs are ineligible for reset.

### Verification Mode (`--verify`)

`./benchmarks/run_benchmarks.sh --verify routine` compares a complete successful
run to the routine snapshot using the shared aggregate rule. Equal and improved
runs pass; regressions fail. It writes only generated run artifacts (including a
machine-readable decision) and leaves the snapshot, `RESULTS.md`, `BASELINES.md`,
and `HISTORY.md` unchanged.

Normal routine recording appends every valid Dark run to `HISTORY.md` with a
unique timestamp/run identity and decision. An improvement atomically advances
the snapshot and regenerates every Dark `RESULTS.md` row; equality changes only
history; regression changes only history and returns failure. Thus snapshots are
the best-known compatible complete run, not necessarily the newest compiler
commit. `--verify-fresh` is the integration variant: unlike ordinary read-only
verification, it fails on an improvement because that better run must first be
recorded. `scripts/land-on-main.sh` uses this freshness check.

`--refresh-baseline=rust` is separate from Dark reset/advancement. It refreshes
only the audited reference data in `BASELINES.md` after a complete successful
run; Rust values do not affect the Dark monotonic decision.

### Timing Mode (`--hyperfine`)

Uses **hyperfine** to measure wall-clock execution time. Fast but results vary between runs.

## Parallelism (`--jobs`)

Benchmarks can execute concurrently:

```bash
./benchmarks/run_benchmarks.sh --jobs 8
```

The default is `--jobs 1`. Cachegrind instruction counts are reproducible when
benchmarks run concurrently, so routine recording and verification may use
more jobs. The runner builds the current Dark compiler once before spawning
benchmark jobs, preventing stale compiler artifacts from being measured.
Hyperfine should remain at one job when avoiding timing skew matters.

## Directory Structure

```
benchmarks/
  run_benchmarks.sh          # Main entry point
  quick_check.sh             # Complete reduced-workload monotonic gate
  README.md                  # This file
  baselines/                 # Typed architecture-specific Dark snapshots
  profiles/                  # Ordered contractual suite membership

  infrastructure/
    build_all.sh             # Compile Dark and Rust implementations
    cachegrind_runner.sh     # Run cachegrind + output validation
    hyperfine_runner.sh      # Run hyperfine timing benchmarks
    result_processor.py      # Generate timing summary
    cachegrind_processor.py  # Generate instruction count summary
    benchmark_baseline.py    # Snapshot contract and exact shared comparison
    history_updater.py       # Monotonic routine recorder and history writer

  problems/
    fib/                     # Each benchmark has its own directory
      expected_output.txt    # Expected output for validation
      quick_expected_output.txt # Shared reduced-workload output
      dark/main.dark         # Dark implementation
      dark/quick.dark        # Reduced Dark implementation
      rust/main.rs           # Rust implementation or Cargo application driver
      rust/quick.rs           # Matching reduced Rust implementation
      python/main.py         # Python implementation

  results/                   # Benchmark results by timestamp
    YYYY-MM-DD_HHMMSS/
      compiler_version.txt   # Git commit of compiler
      cachegrind/            # Temporary cachegrind output files (cleaned after run)
      *_hyperfine.json       # Raw hyperfine output
      *_summary.md           # Per-benchmark markdown
      summary.md             # Overall summary
```

## Output Example

After running benchmarks, you'll see output like:

```
| Language | Mean | Stddev | vs Baseline |
|----------|------|--------|-------------|
| Rust     | 45.2 ms | +/- 1.2 ms | baseline |
| Dark     | 89.4 ms | +/- 2.1 ms | 1.98x slower |
| Python   | 2.34 s  | +/- 0.05 s | 51.8x slower |
```

## Adding New Benchmarks

1. Create a new directory under `problems/`:

   ```bash
   mkdir -p benchmarks/problems/new_bench/{dark,rust,python}
   ```

2. Implement in each language:

   - `dark/main.dark` - Dark implementation
   - `rust/main.rs` - Rust implementation
   - `python/main.py` - Python implementation

3. Create `expected_output.txt` with the expected stdout output.

4. Run: `./benchmarks/run_benchmarks.sh new_bench`

## Benchmark Guidelines

- **Single-threaded**: All implementations must be single-threaded for fair comparison
- **Same algorithm**: Use equivalent algorithms across languages
- **Same optimization opportunities**: Preserve deliberate constants,
  invariants, and other source-level opportunities present in the Rust
  reference when translating the workload to Dark
- **Immutable Rust references**: Never change Rust to make Dark look more
  competitive; improve Dark or classify the pair as non-comparable
- **Parity re-audit**: Update source hashes only after reviewing the complete
  pair and confirming its algorithm, workload, result, and source-level
  optimization opportunities remain equivalent
- **Output validation**: Cachegrind runs verify output matches `expected_output.txt` (with Dark-specific override if present)
- **Sufficient runtime**: Benchmarks should run for at least 100ms to minimize startup overhead
