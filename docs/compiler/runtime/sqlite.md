# SQLite in Dark

`Stdlib.Sqlite` uses the embedded Dark engine under
`packages/darklang/stdlib/sqlite/`. Its only host boundary is existing filesystem
primitives. Generated programs do not link SQLite or libSQL or run database
executables. The compiler's own hosted-package cache is a separate subsystem.

The original upstream wrappers supply the public API. Three portable `Builtin`
bridges delegate execution, querying and batching to the engine. Parameters are
strings bound to `@p0`, `@p1`, etc.; output cells preserve NULL, signed 64-bit
integers, IEEE doubles, UTF-8 text and binary blobs.

## Storage and transactions

The engine reads SQLite 3 table B-tree pages on demand through an open file
descriptor. A transaction caches read pages and maintains separate dirty pages.
Inserts split leaves and interior nodes, growing the root while preserving its
page number. Large records use SQLite overflow chains. Dropping tables and
deleting overflow records return pages to the SQLite freelist. Deleting ordinary
rows compacts their leaf; underfull nodes remain allocated and can accept later
inserts.

Commit writes dirty pages at their existing offsets, followed by the database
header. It does not serialize or replace the entire file. A batch shares one
pager and commits only after every statement and parameter row succeeds. SQL
errors discard dirty pages without changing the database. Before images are
retained for attempting rollback after an ordinary commit write error; if that
rollback also fails, the returned error reports both failures. Newly appended
pages may remain beyond the restored logical database size.

There is no fsync protocol, persistent rollback journal, WAL or recovery after
process termination or power loss. Exclusively own the database while using this
engine. Calls cooperate through an exclusive filesystem lock; SQLite's native
byte-range locking protocol is not implemented, so that lock does not coordinate
with another SQLite implementation.

## Supported subset

- UTF-8 SQLite 3 databases with power-of-two page sizes from 512 to 65536 bytes,
  no reserved page bytes, no auto-vacuum, and rollback-mode file headers.
- Ordinary rowid tables, optionally with one `INTEGER PRIMARY KEY` alias.
- Column types `INTEGER`, `INT`, `NUMERIC`, `REAL`, `TEXT`, and `BLOB`; `NOT NULL`.
- `CREATE TABLE [IF NOT EXISTS]` and `DROP TABLE [IF EXISTS]`.
- `INSERT [OR IGNORE] INTO`, optional target columns, one `VALUES` tuple.
- `UPDATE ... SET` and `DELETE FROM`, optionally filtered by `WHERE`.
- Single-table `SELECT`, or scalar `SELECT` without a table; columns, `*`,
  literals, `AS` aliases, comparisons, `IS [NOT]`, `AND`, `OR`, and parentheses.
- `ORDER BY` column/scalar expressions with `ASC`/`DESC`, literal nonnegative
  `LIMIT`, standalone `count(*)`, and casts to integer, real, text or blob.
- SQL comments, escaped single-quoted strings, double-quoted/backtick identifiers,
  blob literals and string parameters.

Records are limited to 64 MiB. The engine scans tables rather than using a query
optimizer. Query results are fully materialized, matching the public API.

Unsupported statements and schema features return errors: indexes, views,
triggers, joins, subqueries, recursive queries, JSON functions, defaults,
foreign-key constraints, `AUTOINCREMENT`, `WITHOUT ROWID`, and changing a primary
key through `UPDATE`. A database whose schema includes an unsupported object is
rejected rather than modified with its indexes or triggers ignored. This subset
does not implement all SQLite numeric-string conversion edge cases or arbitrary
existing SQLite databases. It does not replace the interpreter's internal store.

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
references and associated indexes for table-scan SELECTs. Writes to such stores
are still rejected until index/constraint maintenance is implemented. Reading a
view or firing triggers is not implemented. Existing records shorter than an
added column use its declared default. WAL files remain unsupported.

The scalar extension includes searched/simple CASE, COALESCE/IFNULL/NULLIF,
concatenation, arithmetic, LIKE, length, substr, char, ASCII case conversion,
typeof, JSON extraction of `$`, `$[index]` and `$.field`, datetime and the package
schema's strftime format. UTF-8 processing preserves SQLite text bytes and counts
codepoints. General JSON paths, date modifiers and strftime formats remain
unsupported. Native differential tests cover typed cells and the unchanged
package-store schema in addition to the original page interoperability suite.

Remaining milestones, in dependency order:

1. Complete dynamic SQL workload captures and assertions for each call site.
2. Schema creation/migrations and ordinary, unique, composite, partial and
   expression index maintenance; defaults, affinities and foreign keys on writes.
3. Aliases, joins, DISTINCT, subqueries/tuple IN, grouping/aggregates and recursive
   CTE execution needed by dependency and branch queries.
4. Bulk INSERT, INSERT SELECT, REPLACE and UPSERT/excluded semantics.
5. Long-lived connections with SQLite locking, WAL, checkpointing, data_version,
   required pragmas, consistent backup/restore and VACUUM.
6. Interpreter package authoring, branch/merge, projection rebuild, migration and
   export/restore workflow parity. The interpreter store is not switched over.
