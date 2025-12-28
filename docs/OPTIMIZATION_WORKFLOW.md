# Optimization Workflow

Best practices for implementing compiler optimizations.

## Workflow

For each optimization, follow these steps:

### 1. Record Baseline Benchmark

Before making any changes, record the current performance:

```bash
./benchmarks/run_benchmarks.sh
```

This automatically appends results to `benchmarks/HISTORY.md`. Note the commit hash and instruction counts for later comparison.

### 2. Choose Optimization

Select the next optimization to implement. Consider:
- Expected impact (which benchmarks will improve?)
- Implementation complexity
- Risk of introducing bugs

### 3. Add Test Cases

Create test cases that specifically trigger the optimization:
- Tests should fail or be suboptimal without the optimization
- Tests should pass and be optimal with the optimization
- Add tests to `tests/e2e/` following existing patterns

Example test patterns for optimizations:
- **Dead Code Elimination**: `let unused = expensive() in result`
- **Constant Folding**: `1 + 2 + 3` should become `6`
- **Copy Propagation**: `let x = y in x` should use `y` directly

### 4. Implement the Optimization

Implement the optimization in the appropriate pass:
- MIR optimizations: `src/DarkCompiler/passes/3.5_MIR_Optimize.fs`
- LIR optimizations: `src/DarkCompiler/passes/4.5_LIR_Optimize.fs`

Guidelines:
- Make the optimization conservative (correctness over performance)
- Add comments explaining the transformation
- Handle all edge cases

### 5. Run Tests

Verify the optimization doesn't break anything:

```bash
dotnet run --project src/Tests
```

All tests must pass before proceeding.

### 6. Record Improvement Benchmark

Run benchmarks again to measure the improvement:

```bash
./benchmarks/run_benchmarks.sh
```

Compare the new results in `benchmarks/HISTORY.md` with the baseline.

### 7. Commit with Benchmark References

Include before/after benchmark data in the commit message:

```
Enable DCE + constant folding optimizations

Benchmark improvement:
- factorial: 7.35M → 6.92M instructions (-5.8%)
- fib: 1015M → 955M instructions (-5.9%)
- sum_to_n: 39M → 36M instructions (-7.7%)

Baseline: commit abc123
After: commit def456
```

## Directory Structure

```
benchmarks/
├── HISTORY.md           # All benchmark results (append-only)
├── run_benchmarks.sh    # Main benchmark runner
├── problems/            # Benchmark programs
└── results/             # Timestamped result directories

docs/
└── OPTIMIZATION_WORKFLOW.md  # This file

src/DarkCompiler/passes/
├── 3.1_SSA_Construction.fs   # SSA form conversion
├── 3.5_MIR_Optimize.fs       # MIR optimizations (DCE, const fold, etc.)
├── 3.9_SSA_Destruction.fs    # SSA to conventional form
├── 4.5_LIR_Optimize.fs       # LIR optimizations (peephole)
└── ...
```

## Current Optimizations

### Implemented (in 3.5_MIR_Optimize.fs)
- **Dead Code Elimination (DCE)**: Remove unused computations
- **Constant Folding**: Evaluate constant expressions at compile time
- **CFG Simplification**: Remove empty/unreachable blocks

### Planned
- **Copy Propagation**: Replace uses with original definitions
- **Common Subexpression Elimination (CSE)**: Reuse identical computations
- **Strength Reduction**: Replace expensive ops with cheaper equivalents
