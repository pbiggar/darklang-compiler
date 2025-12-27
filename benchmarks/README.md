# Darklang Benchmarks

Cross-language benchmarking system to measure runtime performance of compiled Darklang code against Rust and Python.

## Prerequisites

Install before running benchmarks:

```bash
# hyperfine (benchmarking tool)
brew install hyperfine  # macOS
# or: cargo install hyperfine

# Rust compiler
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Python 3 (usually pre-installed)
python3 --version
```

## Quick Start

```bash
# Run all benchmarks
./benchmarks/run_benchmarks.sh

# Run a specific benchmark
./benchmarks/run_benchmarks.sh fib
```

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
    hyperfine_runner.sh      # Run hyperfine with proper flags
    result_processor.py      # Generate summary reports

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
