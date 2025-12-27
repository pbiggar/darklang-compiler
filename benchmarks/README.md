# Darklang Benchmarks

Cross-language benchmarking system to measure runtime performance of compiled Darklang code against Rust and Python.

## Prerequisites

Install before running benchmarks:

```bash
# hyperfine (timing benchmarks)
brew install hyperfine  # macOS
sudo apt-get install hyperfine  # Linux

# valgrind (instruction count benchmarks)
sudo apt-get install valgrind  # Linux
brew install valgrind  # macOS (may require extra setup)

# Rust compiler
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Python 3 (usually pre-installed)
python3 --version
```

## Quick Start

```bash
# Run all benchmarks (timing via hyperfine)
./benchmarks/run_benchmarks.sh

# Run a specific benchmark
./benchmarks/run_benchmarks.sh fib

# Run with Cachegrind for deterministic instruction counts
./benchmarks/run_benchmarks.sh --cachegrind
./benchmarks/run_benchmarks.sh --cachegrind fib
```

## Benchmark Modes

### Timing Mode (default)
Uses **hyperfine** to measure wall-clock execution time. Fast but results vary between runs.

### Cachegrind Mode (`--cachegrind`)
Uses **Valgrind Cachegrind** to count instructions. Slower (~50x) but deterministic - same input always produces identical counts. Useful for:
- Detecting performance regressions in CI
- Comparing instruction efficiency between languages
- Tracking optimization improvements over time

## Available Benchmarks

| Benchmark | Description | Tests |
|-----------|-------------|-------|
| `fib` | Fibonacci(35) naive recursive | Function call overhead, double recursion |
| `factorial` | Factorial(20) x 10000 iterations | Arithmetic, tail-like recursion |
| `sum_to_n` | Sum 1 to 10000, repeated 100x | Accumulator pattern, tail recursion |

## Directory Structure

```
benchmarks/
  run_benchmarks.sh          # Main entry point
  README.md                  # This file

  infrastructure/
    build_all.sh             # Compile Dark and Rust implementations
    validate_all.sh          # Verify correctness before benchmarking
    hyperfine_runner.sh      # Run hyperfine timing benchmarks
    cachegrind_runner.sh     # Run Cachegrind instruction counts
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
- **Same algorithm**: Use equivalent algorithms across languages (e.g., naive recursion, not memoization)
- **Output validation**: All implementations must produce identical output
- **Sufficient runtime**: Benchmarks should run for at least 100ms to minimize startup overhead
