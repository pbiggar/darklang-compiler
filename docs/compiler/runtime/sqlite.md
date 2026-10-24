# SQLite in Dark

`Stdlib.Sqlite` uses the embedded Dark engine under
`packages/darklang/stdlib/sqlite/`. Its only host boundary is existing filesystem
primitives. Generated programs do not link SQLite or libSQL or run database
executables. The compiler's own hosted-package cache is a separate subsystem.

The original upstream wrappers supply the public API. Three portable `Builtin`
bridges delegate execution, querying and batching to the engine. Public parameters are
strings bound to `@p0`, `@p1`, etc. The private engine accepts named typed cells.
Output cells preserve NULL, signed 64-bit
integers, IEEE doubles, UTF-8 text and binary blobs.

## Storage and transactions

The engine reads SQLite 3 table B-tree pages on demand through an open file
descriptor. A transaction caches read pages and maintains separate dirty pages.
Inserts split leaves and interior nodes, growing the root while preserving its
page number. Large records use SQLite overflow chains. Dropping tables and
deleting overflow records return pages to the SQLite freelist. Deleting ordinary
rows compacts their leaf, unlinks empty children and collapses roots. Underfull
nonempty nodes can accept later inserts.

Commit writes dirty pages at their existing offsets, followed by the database
header. It does not serialize or replace the entire file. A batch shares one
pager and commits only after every statement and parameter row succeeds. SQL
errors discard dirty pages without changing the database. Before images are
retained for attempting rollback after an ordinary commit write error; if that
rollback also fails, the returned error reports both failures. Newly appended
pages may remain beyond the restored logical database size.

There is no fsync protocol, persistent rollback journal, WAL or recovery after
process termination or power loss. Rollback-mode transactions use SQLite's native
SHARED, RESERVED and PENDING byte ranges. OFD locks coordinate independent Dark
connections and native SQLite connections, including connections in one process.
Readers may coexist; writers wait for readers before committing. Native locking
regressions cover writer exclusion, committed read visibility and writer handoff.
Linux is exercised here; the Darwin OFD ABI is implemented but not host-tested.

## Supported subset

- UTF-8 SQLite 3 databases with power-of-two page sizes from 512 to 65536 bytes,
  no reserved page bytes, no auto-vacuum, and rollback-mode file headers.
- Ordinary rowid tables, text/composite primary and unique keys, or an `INTEGER PRIMARY KEY` alias.
- SQLite type affinities, `NOT NULL`, literal/function defaults and catalog references.
- Ordinary, unique, composite, partial and expression indexes with BINARY ordering.
- `CREATE TABLE [IF NOT EXISTS]`, `DROP TABLE [IF EXISTS]`, index creation/deletion,
  `ALTER TABLE ... ADD COLUMN` and `pragma_table_info(table)` column discovery.
- `INSERT [OR IGNORE|REPLACE] INTO`, optional target columns, one or more `VALUES`
  tuples, and targeted `ON CONFLICT ... DO NOTHING|UPDATE` with `excluded` and WHERE.
- `UPDATE ... SET` and `DELETE FROM`, optionally filtered by `WHERE`.
- Single-table `SELECT` with aliases and `DISTINCT`, or scalar `SELECT` without a table; columns, `*`,
  literals, `AS` aliases, comparisons, `IS [NOT]`, `AND`, `OR`, and parentheses.
- `ORDER BY` column/scalar expressions with `ASC`/`DESC`, bound or literal integer
  `LIMIT`, standalone `count(*)`, and casts to integer, real, text or blob.
- SQL comments, escaped single-quoted strings, double-quoted/backtick identifiers,
  blob literals and string parameters.

Records are limited to 64 MiB. The engine scans tables rather than using a query
optimizer. Query results are fully materialized, matching the public API.

The relational evaluator supports inner/left/cross joins, derived tables, scalar
and correlated subqueries, EXISTS, scalar/tuple IN and NOT IN, grouping with COUNT
and MAX, UNION/UNION ALL, and materialized/recursive CTEs. Recursive UNION removes
duplicates across iterations and terminates on cycles. Recursive output has a
one-million-row resource limit. Named and ordinal ordering, aliases and DISTINCT
are handled before LIMIT. JSON table inputs expose key/value reads; wildcard
projection of that restricted virtual source returns an error.

Relational expressions are also available in INSERT VALUES, INSERT SELECT,
UPDATE assignments/predicates and DELETE predicates. INSERT SELECT materializes
its input before applying target writes. The branch/replay statement tests execute
the interpreter's JSON-ID updates, scalar commit lookup, projection copy and
scoped delete without changing their SQL.

Unsupported execution features return errors: views/triggers, operational pragmas, WAL, `AUTOINCREMENT`, `WITHOUT ROWID`, and changing a
rowid alias through `UPDATE`. Reference clauses are preserved in the catalog;
foreign-key enforcement/checking is pending. Writes to stores containing views
or triggers are rejected. This subset does not implement all SQLite numeric-string
conversion edge cases or arbitrary existing SQLite databases. It does not replace
the interpreter's internal store.

## Verification

`test/fixtures/e2e/sqlite.e2e` exercises public API behavior. The pinned upstream
SQLite tests run through their original convenience wrappers. Internal codec
fixtures cover signed rowids, varint boundaries and IEEE representation.

`test/regression/sqlite_interop.py`, run by `dune runtest`, compiles a standalone
Dark executable and exchanges files with Python's SQLite engine. It checks native
`integrity_check`, cross-engine reads and writes, tree splits, overflow storage,
freelist reuse, batch rollback, and unchanged bytes in unrelated pages after an
update. Python SQLite is a test oracle only, never a runtime dependency of the
generated program.

## Interpreter package-store compatibility work

The extension targets interpreter revision
`9ca42515810126f9cdf8ab9704ae44cc7e2e7241`. The unchanged initial schema is in
`test/fixtures/sqlite-package/schema.sql`. `corpus.json` inventories 263 source
SQL entries from LibDB and SCM, including 54 dynamic templates. The extraction
excludes tracing and test-process queries, while retaining their shared tables.
Regenerate with `python3 scripts/update-sqlite-package-corpus.py INTERPRETER`;
`--check` verifies the pin and source hashes without writing files.

The static corpus regression prepares 237 concrete statements against native
SQLite. This verifies the reference inputs, **not Dark engine coverage**.
Dynamic templates still require complete workload captures, including the
variable table/column lists and shared CTE prefixes assembled at runtime.

The engine now has a private typed binding interface (`__executeTyped`) with
case-sensitive `@name`, `$name` and `:name` keys. Public string arguments still
bind as `@p0`, `@p1`, etc. Prepared programs hold syntax without old bindings;
`__transaction` supplies one scoped pager across reads and writes and commits
only after the callback succeeds. Multiple statements are parsed together and
share that transaction. These internal operations do not add public Stdlib API.

The catalog reader accepts rowid tables with text/composite keys, defaults,
references and associated indexes for table-scan SELECTs. Index writes use record-key B-trees with promoted interior keys and overflow
chains. Their roots remain stable; rebuilding an affected index reclaims and
reuses its other pages in the statement transaction. Package-store write and
schema creation and typed mutation tests pass against native SQLite. Reading a
view or firing triggers is not implemented. Existing records shorter than an
added column use its declared default. WAL files remain unsupported.

The scalar extension includes searched/simple CASE, COALESCE/IFNULL/NULLIF,
concatenation, arithmetic, LIKE, length, substr, char, ASCII case conversion,
typeof, JSON extraction of `$`, `$[index]` and `$.field`, json_each key/value rows, datetime and the package
schema's strftime format. UTF-8 processing preserves SQLite text bytes and counts
codepoints. General JSON paths and strftime formats remain unsupported. The required integer
second/minute/hour/day date modifiers are supported, including the package
writer's next-second timestamp and store-health next-day check. Native differential tests cover typed cells and the unchanged
package-store schema in addition to the original page interoperability suite.

Remaining milestones, in dependency order:

1. Complete dynamic SQL workload captures and assertions for each call site.
2. Implement remaining metadata/operational pragmas and foreign-key checks.
   Column additions, column discovery and index recreation are covered.
   Index maintenance currently rebuilds affected trees;
   incremental index deletion/rebalancing remains a performance improvement.
3. Complete differential corpus coverage for relational queries. Focused joins,
   correlated and tuple lookups, grouping, JSON joins and recursive cycle cases pass;
   full interpreter query/workflow parity is still being measured.
4. Complete source-template and workflow coverage for writes. INSERT SELECT,
   expression/subquery writes, multirow VALUES, REPLACE and targeted UPSERT/excluded
   semantics are covered by focused and native interoperability cases.
5. Long-lived connections with SQLite locking, WAL, checkpointing, data_version,
   required pragmas, consistent backup/restore and VACUUM.
6. Interpreter package authoring, branch/merge, projection rebuild, migration and
   export/restore workflow parity. The interpreter store is not switched over.
