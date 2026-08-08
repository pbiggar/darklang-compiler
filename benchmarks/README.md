# Darklang Benchmarks

Cross-language benchmarking system to measure runtime performance of compiled Darklang code against Rust and Python.

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

# Python 3 (usually pre-installed)
python3 --version
```

## Quick Start

```bash
# Record the canonical routine profile (instruction counts via cachegrind)
./benchmarks/run_benchmarks.sh

# Run every benchmark, including compiler-focused microbenchmarks, without recording it
./benchmarks/run_benchmarks.sh all

# Run a specific benchmark
./benchmarks/run_benchmarks.sh fib

# Run with hyperfine for timing instead
./benchmarks/run_benchmarks.sh --hyperfine
./benchmarks/run_benchmarks.sh --hyperfine fib

# Run benchmarks in parallel (defaults to CPU count if omitted)
./benchmarks/run_benchmarks.sh --jobs 4

# Verify the canonical routine profile without updating tracked files
./benchmarks/run_benchmarks.sh --verify routine
```

## Benchmark Modes

### Cachegrind Mode (default)

Uses **Valgrind Cachegrind** to count instructions. Slower (~50x) but deterministic - same input always produces identical counts. Useful for:

- Detecting performance regressions in CI
- Comparing instruction efficiency between languages
- Tracking optimization improvements over time

This is the primary way we are tracking performance.

The `routine` profile is the canonical full-size benchmark set. A completed
routine Cachegrind run records its measurements in `RESULTS.md` and
`HISTORY.md`. Targeted runs and `all` are diagnostic and do not update those
canonical files.

### Verification Mode (`--verify`)

`./benchmarks/run_benchmarks.sh --verify routine` runs the exact profile named by
`RESULTS.md` and compares each full-size Dark instruction count with its
committed value. It fails for missing, unexpected, or changed results and never
updates tracked files. Verification cannot be combined with `--hyperfine` or
`--refresh-baseline`.

### Timing Mode (`--hyperfine`)

Uses **hyperfine** to measure wall-clock execution time. Fast but results vary between runs.

## Parallelism (`--jobs`)

Benchmarks are deterministic, so you can run multiple benchmarks at once:

```bash
./benchmarks/run_benchmarks.sh --jobs 8
```

Set `BENCHMARK_JOBS` to change the default without passing a flag.
For `--hyperfine`, the default is `--jobs 1` to avoid timing skew.

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
      dark/main.dark         # Dark implementation
      rust/main.rs           # Rust implementation
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
- **Output validation**: Cachegrind runs verify output matches `expected_output.txt` (with Dark-specific override if present)
- **Sufficient runtime**: Benchmarks should run for at least 100ms to minimize startup overhead
