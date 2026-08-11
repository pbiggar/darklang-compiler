# Darklang Benchmarks

Cross-language benchmarking system to measure compiled Darklang performance
against human-audited Rust reference implementations. Other language programs
remain available for diagnostics but do not contribute canonical ratios.

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
# Record the canonical routine profile (instruction counts via cachegrind)
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
before recording numbers. The routine profile accepts only `comparable` full
pairs; reduced, Dark-only, and incomparable programs remain available for
diagnostics without contributing to canonical ratios.

Rust is built with the suite's original `rustc -C opt-level=3` command. Dark is
built with its normal native compiler pipeline. Fairness here means equivalent
algorithms, workloads, observable results, and source-level optimization
opportunities—not forcing either compiler to miss legitimate optimizations.

### Cachegrind Mode (default)

Uses **Valgrind Cachegrind** to count instructions. Slower (~50x) but deterministic - same input always produces identical counts. Useful for:

- Detecting performance regressions in CI
- Comparing instruction efficiency between languages
- Tracking optimization improvements over time

This is the primary way we are tracking performance.

The `routine` profile is the canonical comparable benchmark set and currently
contains 19 pairs. It excludes incomparable nsieve and the reduced Dark
fannkuch workload. Binary trees now uses the same recursive allocation and
traversal shape as Rust. Full-size quicksort and spectral norm are included. A
completed routine Cachegrind run records its measurements in
`RESULTS.md` and `HISTORY.md`; targeted runs and `all` are diagnostic and do not
update those canonical files. Recording accepts an optional `--machine` ID from
the registry in `HISTORY.md`; omitted machine metadata is left blank rather than
guessing the runner's identity. Verification does not update history.

### Quick correctness and regression mode

Every problem has `dark/quick.dark`, `rust/quick.rs`, and a shared
`quick_expected_output.txt`. Quick variants preserve their full implementation's
algorithm and optimization opportunities while reducing only workload
parameters. `quick_check.sh` builds the current Dark compiler, compiles Rust with
the same `rustc -C opt-level=3` setting as the full suite, validates both native
outputs, and counts both binaries under the same Cachegrind options. A committed
architecture baseline additionally gates Dark compiler regressions when one is
available; Rust counts are reported with a ratio for comparable pairs and
marked diagnostic-only for incomparable pairs.

### Verification Mode (`--verify`)

`./benchmarks/run_benchmarks.sh --verify routine` runs the exact profile named by
`RESULTS.md` and compares each full-size Dark instruction count with its
committed value. It fails for missing, unexpected, or changed results and never
updates tracked files. Verification cannot be combined with `--hyperfine` or
`--refresh-baseline`.

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
  README.md                  # This file

  infrastructure/
    build_all.sh             # Compile Dark and Rust implementations
    cachegrind_runner.sh     # Run cachegrind + output validation
    hyperfine_runner.sh      # Run hyperfine timing benchmarks
    result_processor.py      # Generate timing summary
    cachegrind_processor.py  # Generate instruction count summary
    history_updater.py       # Append results to HISTORY.md

  problems/
    fib/                     # Each benchmark has its own directory
      expected_output.txt    # Expected output for validation
      quick_expected_output.txt # Shared reduced-workload output
      dark/main.dark         # Dark implementation
      dark/quick.dark        # Reduced Dark implementation
      rust/main.rs           # Rust implementation
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
