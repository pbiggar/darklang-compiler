# Removed Performance Optimizations

This document describes the performance optimization systems that were removed from the compiler to simplify the codebase. These were removed in the commit that added this document (see `git log --oneline docs/removed-optimizations.md`).

Note: the v4 SQLite function cache was removed again. See `docs/compiler-cache.md`
for the historical design and the performance hacks it used.

## Why They Were Removed

The compiler accumulated complexity from four interrelated optimization systems. While these improved compilation speed during development, they added significant complexity:

- **Debugging difficulty**: Caching bugs are notoriously hard to track down
- **Code complexity**: ~1000 lines of caching/parallel infrastructure
- **Interdependencies**: Systems depended on each other (e.g., `ConcurrentDictionary` existed because of parallel execution)
- **Maintenance burden**: Changes to compiler passes required cache invalidation updates

The decision was made to prioritize simplicity over compilation speed.

## What Was Removed

### 1. SQLite Persistent Cache (`Cache.fs`)

**What it was**: A SQLite-backed cache that stored post-register-allocation
`LIR.Function` artifacts across compiler runs.

**How it worked**:
- Used MessagePack serialization for binary encoding of `LIR.Function` values
- Keyed entries by cache version, compiler hash, options hash, function name,
  and a dependency hash derived from post-inlining ANF
- Batched reads/writes via prepared statements and a single transaction per
  scope, with aggressive SQLite pragma tuning for speed
- Preloaded `CacheSnapshot` entries for test runs to reduce repeated I/O

**Files affected**:
- `Cache.fs` - Deleted entirely
- `DarkCompiler.fsproj` - Removed MessagePack and Microsoft.Data.Sqlite package references
- `CompilerLibrary.fs` - Removed cache settings/types and cache-aware compilation
- `Program.fs` - Removed `--cache-key` and `--no-cache` flags
- `AST.fs`, `ANF.fs`, `LIR.fs` - Removed MessagePack serialization attributes
- `TestRunner.fs`, `TestFramework.fs` - Removed cache plumbing and summary output

### 2. Parallelization (`ResultList.fs`)

**What it was**: Infrastructure for parallel compilation of stdlib functions and parallel test execution.

**How it worked**:
- `mapResults` (formerly `mapResultsParallel`): Parallel map over lists collecting `Result` types (now sequential)
- `Parallel.For` in test runner for concurrent test execution
- Lock objects (`stdlibLock`, `compileLock`) for thread safety
- `getOptimalParallelism`: Computed thread pool size based on CPU cores

**Files affected**:
- `ResultList.fs` (renamed from `ParallelUtils.fs`) - Simplified to sequential-only operations
- `TestRunner.fs` - Replaced `Parallel.For` with sequential `for` loop, removed all lock objects
- `ANF_to_MIR.fs`, `MIR_to_LIR.fs` - Now use sequential mapping

### 3. In-Memory Caches (6 `ConcurrentDictionary` caches)

**What they were**: Thread-safe dictionaries caching various compilation stages.

**The caches**:
1. `SpecializationCache` - Cached monomorphized generic function instantiations
2. `CompiledFunctionCache` - Cached fully compiled functions (AST→LIR)
3. `CodegenCache` - Cached ARM64 code generation results
4. `ANFFunctionCache` - Cached AST→ANF conversion results
5. `PreambleCache` - Cached compiled E2E test preambles
6. `LIRCache` - Cached MIR→LIR conversion results

**Files affected**:
- `CompilerLibrary.fs` - Removed all cache type definitions and helper functions
- `StdlibResult` record simplified to remove cache fields
- `AST_to_ANF.fs` - Removed `specializeFunctionCached`, used direct specialization

### 4. Laziness Patterns (`Lazy<T>`)

**What it was**: Deferred computation for stdlib compilation.

**How it worked**:
- `LazyStdlibResult` type wrapped stdlib compilation in `Lazy<T>`
- `cachedModuleRegistry` was a lazy singleton in `Stdlib.fs`
- `prepareStdlibForLazyCompile` set up deferred compilation
- Test preambles used `LazyThreadSafetyMode.ExecutionAndPublication`

**Rationale for laziness**: Stdlib has ~100 functions but most programs use only a few. Lazy compilation avoided compiling unused functions.

**Files affected**:
- `Stdlib.fs` - Removed `cachedModuleRegistry` lazy singleton
- `CompilerLibrary.fs` - Deleted `LazyStdlibResult` type, `prepareStdlibForLazyCompile`, related functions
- `E2ETestRunner.fs` - Removed lazy preamble compilation

## Performance Impact

After removal:
- **Compilation is slower**: Each compiler invocation recompiles stdlib and functions
- **Test suite is slower**: No cache reuse across runs, no parallel execution
- **Memory usage is simpler**: No long-lived caches accumulating entries

The trade-off was deemed acceptable for the current development phase.

## Reinstating Optimizations

If compilation speed becomes a problem again, consider:

1. **Start with the simplest**: In-memory function caches give the most benefit with least complexity
2. **Avoid persistence**: SQLite caching added the most complexity for cache invalidation
3. **Parallelization last**: Only needed once caches are working correctly
4. **Consider alternatives**: Incremental compilation or file-based caching might be simpler than SQLite

## Files Changed Summary

| File | Change |
|------|--------|
| `src/DarkCompiler/Cache.fs` | Deleted |
| `src/DarkCompiler/ResultList.fs` | Simplified to sequential (renamed from `ParallelUtils.fs`) |
| `src/DarkCompiler/CompilerLibrary.fs` | Major simplification |
| `src/DarkCompiler/Stdlib.fs` | Removed lazy singleton |
| `src/DarkCompiler/AST.fs` | Removed MessagePack attributes |
| `src/DarkCompiler/ANF.fs` | Removed MessagePack attributes |
| `src/DarkCompiler/LIR.fs` | Removed MessagePack attributes |
| `src/DarkCompiler/passes/2.4_ANF_Inlining.fs` | Removed dependency hash functions |
| `src/DarkCompiler/passes/3_ANF_to_MIR.fs` | Sequential mapping |
| `src/DarkCompiler/passes/4_MIR_to_LIR.fs` | Sequential mapping |
| `src/DarkCompiler/DarkCompiler.fsproj` | Removed Cache.fs, packages |
| `src/Tests/TestRunner.fs` | Sequential test execution |
| `src/Tests/Runners/E2ETestRunner.fs` | Removed caching, lazy patterns |
| `src/Tests/StdlibTestHarness.fs` | Simplified |
| `src/Tests/test-suite-tooling/TestRunner.fs` | Kept stdlib build and preamble build smoke coverage in the main runner |
| `src/Tests/test-suite-tooling/Runners/E2ETestRunner.fs` | Inlined preamble build setup into sequential E2E execution |
| `src/Tests/test-suite-tooling-tests/StdlibSourceTests.fs` | Kept source-level stdlib invariant coverage |
