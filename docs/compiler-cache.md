# Compiler Cache (Removed)

Status: removed. The compiler no longer uses a compile cache; this document is
kept as a historical record of the v4 SQLite function cache and the
performance hacks we used to keep it fast.

Historical cache file location:

```
~/.cache/dark-compiler/cache.db
```

The cache used to be disableable with `--no-cache` (compiler CLI) or
`./run-tests --no-cache`. Those flags were removed with the cache.

## Goals (Historical)

- Speed up repeat compiles without changing compiler output.
- Cache only the smallest useful artifacts.
- Keep cache keys explicit and easy to reason about.
- Make invalidation obvious and reliable.

## What We Cached (Historical)

Only one artifact type:

1. **Function artifacts**: post-register-allocation `LIR.Function` values
   (including the synthesized `_start` entrypoint).

This kept the cache small and avoided storing large IR graphs or binaries.

## Where the Cache Applied (Historical)

- **Stdlib**: function cache was used when lowering stdlib functions.
- **Preamble**: function cache was used for per-file preamble compilation.
- **User code / test expressions**: function cache was used when lowering user
  functions.

## Design Choices and Assumptions (Historical)

- **SQLite 3**: simple, local, cross-platform, no extra services.
- **Minimal artifacts**: only store LIR functions (no binaries or
  intermediate IR programs).
- **Dependency-based invalidation**: cache keys were derived from the
  function's body, the types it touched, and the signatures of its non-inlined
  callees.
- **Compiler hash**: SHA256 of the compiler binary invalidated the entire cache
  on compiler changes.
- **Post-inlining inputs**: hashes were computed from ANF after inlining, so
  inlining changes invalidated cached functions.
- **Fail fast on hash errors**: if MessagePack couldn't serialize a value used
  for hashing, we crashed rather than produce incorrect keys.
- **Determinism assumption**: given the same typed program and options, passes
  were deterministic. Any non-determinism would have broken cache correctness.
- **Fail-fast I/O**: when caching was enabled, cache read/write errors failed
  compilation so we never silently ignored I/O problems.

## Cache Keying (Historical)

All cache entries included:

- Cache version (`v4`)
- Compiler hash (SHA256 of the compiler binary)

### Function Cache Keys

Each function entry was keyed by:

- **Context hash**: a fixed tag for the function cache (kept to preserve the
  simple schema; all meaningful invalidation lived in the function hash).
- **Options hash**: hash of cacheable compiler options (all optimization and
  runtime flags that affect output; debug dump flags were excluded).
- **Function name**: for clarity and collision avoidance.
- **Function hash**: dependency hash derived from the **post-inlining ANF
  function**, the types it used, and the hashes of its callees.

### Dependency Hash Inputs

Each function's dependency hash was built from:

- **Body hash**: hash of the post-inlining ANF function body plus its typed
  parameters and return type.
- **Type hash**: hash of all types the function touched, including:
  - types in the function signature,
  - explicit types in ANF (TypedAtom/Print/RawGet/RawSlotInit),
  - TempId types from the RC TypeMap,
  - record and sum type definitions referenced by those types.
- **Callee hashes** (post-inlining call graph, so only non-inlined calls remain):
  - **Internal callees**: signature hashes of other functions in the same
    compilation batch (function type plus type dependencies).
  - **External callees**: signature hashes of stdlib/preamble functions
    (function type plus type dependencies).

### Recursion

Recursive functions were hashed independently. Calls to other functions
(including mutual recursion) contributed only signature hashes, so body changes
in one function did not invalidate its callers unless the signature changed.

## Hashing and Serialization (Historical)

- Hashes were SHA256 of MessagePack-serialized values (`Cache.hashData`).
- MessagePack used `FSharpResolver` + `ContractlessStandardResolver` with
  `UntrustedData` security.
- Discriminated unions with named fields used `[<MessagePackObject(false)>]`
  for integer-key (positional) serialization to avoid the known named-field
  constructor mismatch bug in MessagePack's F# resolver.
- Simple string content (source text, empty preamble) used `Cache.hashString`.

## Database Schema (Historical)

Two tables:

`function_cache`
- `cache_version`, `compiler_key`, `options_hash`, `function_name`,
  `function_hash`, `data`, `created_at`
- Primary key: all key fields above.

`compiler_meta`
- `compiler_path`, `compiler_key`, `updated_at`
- Primary key: `compiler_path`.

`created_at`/`updated_at` were informational only.

## Behavior and Invalidation (Historical)

- Cache reads/writes were batched per compilation stage to avoid per-function
  SQLite connection overhead.
- Any cache miss resulted in a normal compile path.
- Cache I/O errors failed compilation unless caching was disabled.
- If the compiler binary changed at the same path, cached entries keyed on the
  old compiler hash were evicted.
- Bump `Cache.cacheVersion` if the schema or keying semantics changed.
- Delete `~/.cache/dark-compiler/cache.db` to fully clear the cache.

## What Was Not Cached (Historical)

- Test results.
- Intermediate IRs (AST/ANF/MIR/LIR programs).
- Final binaries (Mach-O/ELF).
- String/float pools (they are per-program; cached functions were stored in
  LIR before pool resolution).

## Known Limitations (Historical)

- Eviction only happened when a compiler binary changed at a known path; caches
  otherwise grew until manually cleared.
- No cross-compiler sharing (compiler hash was part of the key).

## Performance Hacks We Used (Historical)

These were the explicit speed hacks layered on top of the basic cache design:

- **SQLite pragma tuning**: `journal_mode=WAL`, `synchronous=OFF`,
  `temp_store=MEMORY`, `cache_size=-262144` (256MB), `mmap_size=268435456`
  (256MB), `wal_autocheckpoint=10000`, `journal_size_limit=268435456`,
  `busy_timeout=5000`.
- **Batch reads**: grouped cache lookups by `(cache_version, compiler_key,
  options_hash)` and reused a prepared statement to avoid per-function queries.
- **Batch writes**: `INSERT OR REPLACE` in a single transaction with prepared
  parameters, committing once per batch.
- **Pending write buffers**: compiler stages accumulated cache writes in memory
  and flushed per scope (stdlib, preamble, user program) to avoid chatty I/O.
- **Snapshot preloading**: test runs loaded all entries for a compiler key into
  a `CacheSnapshot` to avoid repeated database reads during the suite.
- **In-memory deserialization cache**: snapshot reads memoized decoded
  `LIR.Function` values to avoid repeated MessagePack decoding.
- **Smallest artifact**: cached only post-regalloc `LIR.Function` values to
  keep serialized blobs small and deserialization fast.
- **High-precision invalidation**: post-inlining dependency hashes avoided
  needless invalidations, keeping hit rates higher without widening cache keys.
- **Binary serialization**: MessagePack with contractless F# resolvers avoided
  slower reflection-heavy formats and kept hash/serialize cost low.
