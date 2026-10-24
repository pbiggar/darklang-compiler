# SQLite in Dark

`Stdlib.Sqlite` uses the embedded Dark engine under
`packages/darklang/stdlib/sqlite/`. Generated programs do not link SQLite or
libSQL or invoke database executables. The host boundary consists of filesystem,
clock, entropy and descriptor/locking primitives. The compiler's hosted-package
cache is a separate subsystem.

The upstream convenience wrappers retain their public API. Public parameters are
strings bound to `@p0`, `@p1`, etc. Private execution accepts case-sensitive named
`@name`, `$name` and `:name` bindings, preserving NULL, signed 64-bit integers,
IEEE doubles, UTF-8 text and binary blobs. Prepared programs contain syntax only;
every execution receives fresh bindings.

## Storage and transactions

The pager reads SQLite 3 pages on demand through an open descriptor. Transactions
cache read pages and keep dirty pages separately. Table B-trees split leaves and
interiors while preserving roots; large records use overflow chains. Deletion
unlinks empty children, collapses roots and reuses freelist pages. Index B-trees
use promoted interior keys, overflow chains and stable roots. Mutations rebuild
affected indexes, reclaiming their old pages inside the transaction.

Rollback-mode commit writes dirty pages at their existing offsets and writes the
header last. Before images support an attempted rollback after an ordinary write
error. WAL mode reads committed snapshots and appends checksum-protected frames,
then publishes SQLite's shared-memory index. SHARED/RESERVED/PENDING database
locks and WAL attachment/reader/writer/checkpoint locks coordinate native SQLite
and independent Dark connections. OFD locks keep descriptors independent within
a process. Linux is exercised; the Darwin OFD ABI is implemented but not
host-tested here.

SQL errors discard pending pages. Ordinary failed WAL appends are truncated back
to the published snapshot before returning an error; failed rollback attempts are
reported explicitly. There is no fsync protocol or persistent rollback journal
for power-loss safety. Committed WAL recovery is implemented, but this engine
does not claim SQLite's crash-durability guarantees.

`__Connection` owns a persistent descriptor attachment, releases transaction locks
between commands and reloads each new snapshot. It supports deferred/immediate
transactions, commit/rollback, typed prepared commands, connection-local
`busy_timeout`, `synchronous=NORMAL|OFF`, `foreign_keys`, and `data_version`.
The version counter notices other connections' commits without counting its own.
A rollback connection whose journal mode changes externally must be reopened;
it returns an error instead of reading or writing through the wrong pager.

`PRAGMA journal_mode=WAL` initializes WAL before the scoped connection opens.
`PRAGMA wal_checkpoint(TRUNCATE)` respects active readers and reports contention.
Backup/restore copies a consistent page snapshot through the destination pager,
including into a live WAL database, preserving its inode. VACUUM rebuilds tables
and indexes into a compact set of dirty pages, then installs them through the
same transaction. Ordinary statements do not serialize or replace the whole file.

## SQL subset

- UTF-8 rowid databases with power-of-two page sizes from 512 to 65536 bytes,
  no reserved page bytes and no auto-vacuum.
- Text/composite primary and unique keys, `INTEGER PRIMARY KEY` aliases,
  SQLite affinities, NOT NULL, defaults and the package schema's references.
- Ordinary, unique, composite, partial and expression indexes, with BINARY ordering.
- Table/index creation and deletion, `ALTER TABLE ... ADD COLUMN`, `sqlite_master`
  / `sqlite_schema` reads and `pragma_table_info(table)` column discovery.
- Multirow VALUES, INSERT SELECT, INSERT OR IGNORE/REPLACE, targeted UPSERT,
  excluded-column references, UPDATE and DELETE with relational expressions.
- Scalar SELECT, DISTINCT, inner/left/cross joins, derived tables, scalar and
  correlated subqueries, EXISTS, scalar/tuple IN and NOT IN, GROUP BY/HAVING,
  COUNT/MAX, UNION/UNION ALL, and materialized/recursive CTEs.
- Column/alias/ordinal ordering, ASC/DESC query sorting and bound/literal LIMIT.
- CASE, COALESCE/IFNULL/NULLIF, comparisons, SQL NULL/boolean logic, arithmetic,
  concatenation, LIKE, length, substr, char, ASCII case conversion, typeof and casts.
- JSON extraction of `$`, `$[index]` and `$.field`; json_each key/value rows.
- ISO date/time inputs and now, datetime, the package schema's strftime format,
  CURRENT_TIMESTAMP and integer second/minute/hour/day modifiers.
- Package operational pragmas and foreign-key checking; scoped and explicit
  transactions, consistent backup/restore and VACUUM.

The supported inline foreign keys are checked by `PRAGMA foreign_key_check`
regardless of the connection's `foreign_keys` setting. Enabling enforcement checks
mutations before commit. Package bulk import uses foreign_keys=OFF and performs
its explicit check afterwards.

This is a restricted implementation, not all of SQLite. Views, triggers,
AUTOINCREMENT, WITHOUT ROWID, arbitrary collations/JSON paths/date formats,
Julian-date inputs and UPDATE of a rowid alias are unsupported. Writes to stores
containing views or triggers are rejected. Catalog SQL is normalized when
rewritten; its original whitespace/comments are not preserved. General numeric
conversion edge cases outside the package workload are not promised.

Queries use table scans and materialize their results. Index maintenance rebuilds
whole affected trees. Records are limited to 64 MiB and recursive output to one
million rows. Incremental index updates, query optimization, bounded caches and
large-store performance remain future work.

## Package-store coverage and verification

The target is interpreter revision
`9ca42515810126f9cdf8ab9704ae44cc7e2e7241`. The unchanged initial schema is in
`test/fixtures/sqlite-package/schema.sql`. The corpus inventories 263 SQL source
entries from LibDB and SCM, including 54 dynamic templates. Tracing and
process-test query implementations are excluded; their shared schema remains.
Regenerate with `python3 scripts/update-sqlite-package-corpus.py INTERPRETER`;
`--check` verifies the pin and source hashes without writing files.

The static corpus prepares 237 concrete statements against native SQLite, which
validates the reference input only. `sqlite_package_templates.py` constructs
source-driven variants for every dynamic entry, including variable content-table
columns, list lengths, search branches, main/branch predicates and CTE fragments.
These are explicit constructions, not instrumentation of the running F# host.

The following regressions run under `dune runtest`. Native SQLite is an oracle,
never a generated program's runtime dependency:

- `sqlite_interop.py`: table pages, splits, overflow, freelist reuse, deletion,
  atomic batches, prepared rebinding and unchanged unrelated pages.
- `sqlite_index_interop.py`: index leaf/interior splits, mixed typed keys,
  overflow, stable roots and reuse at multiple page sizes.
- `sqlite_locking.py`: writer exclusion, committed read visibility, reader/writer
  handoff and independent descriptor locks.
- `sqlite_wal.py`: bidirectional WAL interoperability, snapshots, reader-slot
  sharing, hash-block transitions, restart, committed-frame recovery and ordinary
  append-error rollback.
- `sqlite_operations.py`: held transactions and data_version, configuration,
  checkpoint contention/truncation, backup/restore, foreign keys and compaction.
- `sqlite_package_store.py --audit`: typed read results for concrete queries and
  constructed dynamic read variants, plus unchanged-schema interoperability.
- `sqlite_package_differential.py`: every mutation shape against independent
  native/Dark stores, comparing counts, logical catalogs, typed rows and integrity;
  every source site must have a successful execution.
- `sqlite_package_workflows.py`: SQL-level authoring, commit, branch merge,
  projection replay, atomic failure, full-schema export/restore and migration.

Public API and internal E2E fixtures also run in the host suite. Timestamp
comparisons allow independently sampled clock values within measured intervals;
unordered result comparisons preserve multiplicity. Stored cells and explicit
ordering are otherwise compared exactly.

These workflows exercise opaque serialized bodies and the interpreter's SQL;
they do not execute its F# serializers or CLI. The interpreter repository has not
been switched to this backend. Full compiler build, host suite, `dune runtest`
and the source-ledger check are completion gates. The benchmark gate is not
applicable in ChatGPT Work.
