#!/usr/bin/env python3
"""Cross-read and modify real SQLite files with the Dark engine and Python's SQLite oracle."""
import json
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile


PROGRAM = '''let valueText (value: Stdlib.Sqlite.Value) : String =
  match value with
  | Null -> "null"
  | Int i -> Stdlib.Int64.toString i
  | Real f -> Stdlib.Float.toString f
  | Text s -> s
  | Bytes b -> Stdlib.Blob.toHex b
let reportExec (result: Stdlib.Result.Result<Int,String>) : String =
  match result with | Ok n -> "Ok " + Stdlib.Int.toString n | Error e -> "Error " + e
let reportBatch (result: Stdlib.Result.Result<List<Int>,String>) : String =
  match result with | Ok ns -> "Ok " + Stdlib.String.join (Stdlib.List.map ns Stdlib.Int.toString) "," | Error e -> "Error " + e
let reportQuery (result: Stdlib.Result.Result<List<Dict<String,Stdlib.Sqlite.Value>>,String>) : String =
  match result with
  | Error e -> "Error " + e
  | Ok rows -> "Ok " + Stdlib.String.join (Stdlib.List.map rows (fun row ->
    Stdlib.String.join (Stdlib.List.map (Stdlib.Dict.toList row) (fun (k,v) -> k + "=" + valueText v)) "|")) ";"
let env (key: String) : String =
  Stdlib.Env.get key |> Stdlib.Option.withDefault ""
let db = env "SQLITE_TEST_DB"
let sql = env "SQLITE_TEST_SQL"
let paramsText = env "SQLITE_TEST_PARAMS"
let params = if paramsText == "" then [] else Stdlib.String.split paramsText "\\n"
let mode = env "SQLITE_TEST_MODE"
if mode == "bulk" then
  let rows = Stdlib.List.map (Stdlib.List.range 1 500) (fun i -> [Stdlib.Int.toString i, "row " + Stdlib.Int.toString i + paramsText])
  Stdlib.printLine (reportBatch (Stdlib.Sqlite.execBatch db [(sql,rows)]))
else if mode == "transaction" || mode == "abort" || mode == "rebind" then
  let result = Stdlib.Sqlite.__Engine.__transaction db (fun state ->
    Stdlib.Sqlite.__Engine.__prepare "INSERT INTO t VALUES ($id,$name)" |> Stdlib.Result.andThen (fun prepared ->
      Stdlib.Sqlite.__Engine.__executePrepared state prepared (Dict { "$id": Stdlib.Sqlite.Value.Int 601L, "$name": Stdlib.Sqlite.Value.Text "pending" })
      |> Stdlib.Result.andThen (fun first ->
        let bindings = if mode == "rebind" then Dict { "$id": Stdlib.Sqlite.Value.Int 602L } else Dict { "$id": Stdlib.Sqlite.Value.Int 602L, "$name": Stdlib.Sqlite.Value.Text "second" }
        Stdlib.Sqlite.__Engine.__executePrepared first.state prepared bindings |> Stdlib.Result.andThen (fun second ->
          Stdlib.Sqlite.__Engine.__prepare "SELECT name FROM t WHERE id=$id" |> Stdlib.Result.andThen (fun query ->
            Stdlib.Sqlite.__Engine.__executePrepared second.state query (Dict { "$id": Stdlib.Sqlite.Value.Int 601L })
            |> Stdlib.Result.andThen (fun queried ->
              if mode == "abort" then Error "deliberate abort"
              else Ok ((queried.state,queried.rows))))))))
  Stdlib.printLine (reportQuery result)
else if mode == "query" then
  Stdlib.printLine (reportQuery (Stdlib.Sqlite.queryP db sql params))
else if mode == "rollback" then
  Stdlib.printLine (reportBatch (Stdlib.Sqlite.execBatch db [(sql,[["1001","first"],["1","duplicate"]])]))
else Stdlib.printLine (reportExec (Stdlib.Sqlite.execP db sql params))
'''


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-sqlite-") as temporary:
        root = Path(temporary)
        source, binary, db = (root / n for n in ("sqlite.dark", "sqlite", "test.db"))
        source.write_text(PROGRAM)
        built = subprocess.run([compiler, "--allow-internal", "-q", str(source), "-o", str(binary)],
                               capture_output=True, text=True, timeout=180)
        assert built.returncode == 0, built.stdout + built.stderr

        def run(sql, params=(), mode="exec", path=db):
            result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=90,
                                    env=dict(os.environ, SQLITE_TEST_DB=str(path), SQLITE_TEST_SQL=sql,
                                             SQLITE_TEST_PARAMS="\n".join(params), SQLITE_TEST_MODE=mode))
            assert result.returncode == 0, (sql, result.returncode, result.stdout, result.stderr)
            return result.stdout.rstrip("\n")

        def ok(sql, params=(), mode="exec", path=db):
            result = run(sql, params, mode, path)
            assert result.startswith("Ok"), (sql, result)
            return result

        ok("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")
        ok("INSERT INTO t VALUES (@p0,@p1)", mode="bulk")
        with sqlite3.connect(db) as connection:
            assert connection.execute("PRAGMA integrity_check").fetchone() == ("ok",)
            assert connection.execute("SELECT count(*) FROM t").fetchone() == (500,)
            assert connection.execute("SELECT name FROM t WHERE id=500").fetchone() == ("row 500",)
            connection.execute("INSERT INTO t VALUES (?,?)", (-123, "SQLite wrote 雪"))
        assert "SQLite wrote 雪" in ok("SELECT name FROM t WHERE id = @p0", ["-123"], "query")
        before = db.read_bytes()
        assert run("INSERT INTO t VALUES (@p0,@p1)", mode="rollback").startswith("Error")
        assert db.read_bytes() == before, "a failed batch changed database pages"
        ok("UPDATE t SET name = @p0 WHERE id = @p1", ["changed", "250"])
        after = db.read_bytes()
        assert len(after) == len(before)
        page_size = int.from_bytes(after[16:18], "big")
        changed = [i for i in range(0,len(before),page_size) if before[i:i+page_size] != after[i:i+page_size]]
        assert len(changed) <= 2, changed  # Header and the modified leaf; unrelated pages stay intact.
        before = db.read_bytes()
        assert run("", mode="abort") == "Error deliberate abort"
        assert db.read_bytes() == before, "a failed transaction changed database pages"
        assert "missing bound parameter: $name" in run("", mode="rebind")
        assert db.read_bytes() == before, "a reused prepared command retained an old binding"
        assert run("", mode="transaction") == "Ok name=pending"
        with sqlite3.connect(db) as connection:
            assert connection.execute("SELECT name FROM t WHERE id IN (601,602) ORDER BY id").fetchall() == [("pending",),("second",)]
            assert connection.execute("PRAGMA integrity_check").fetchone() == ("ok",)
        ok("DELETE FROM t WHERE id > 400")
        with sqlite3.connect(db) as connection:
            assert connection.execute("SELECT name FROM t WHERE id=250").fetchone() == ("changed",)
            assert connection.execute("SELECT count(*) FROM t").fetchone() == (401,)
            assert connection.execute("PRAGMA integrity_check").fetchone() == ("ok",)

        big = "snow 雪 " * 3000
        ok("INSERT INTO t VALUES (@p0,@p1)", ["999", big])
        with sqlite3.connect(db) as connection:
            assert connection.execute("SELECT name FROM t WHERE id=999").fetchone() == (big,)
            assert connection.execute("PRAGMA integrity_check").fetchone() == ("ok",)
        assert big in ok("SELECT name FROM t WHERE id=999", mode="query")
        ok("DELETE FROM t WHERE id=999")
        free_size = db.stat().st_size
        ok("INSERT INTO t VALUES (@p0,@p1)", ["998", big])
        assert db.stat().st_size == free_size, "freed overflow pages were not reused"
        ok("DROP TABLE t")
        with sqlite3.connect(db) as connection:
            assert connection.execute("PRAGMA integrity_check").fetchone() == ("ok",)
            assert connection.execute("SELECT name FROM sqlite_schema").fetchall() == []

        # SQLite-generated small pages force interior splits with a modest row count.
        small = root / "small.db"
        with sqlite3.connect(small) as connection:
            connection.execute("PRAGMA page_size=512")
            connection.execute("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")
        ok("INSERT INTO t VALUES (@p0,@p1)", ["x" * 80], mode="bulk", path=small)
        with sqlite3.connect(small) as connection:
            assert connection.execute("SELECT count(*) FROM t").fetchone() == (500,)
            assert connection.execute("PRAGMA integrity_check").fetchone() == ("ok",)

        # Removing the last row in a non-root leaf must unlink the empty
        # child and shrink the root; SQLite rejects dangling empty children.
        allocated = small.stat().st_size
        ok("DELETE FROM t WHERE id>=2",path=small)
        with sqlite3.connect(small) as connection:
            assert connection.execute("SELECT count(*) FROM t").fetchone()==(1,)
            assert connection.execute("PRAGMA integrity_check").fetchall()==[("ok",)]
        ok("DELETE FROM t",path=small)
        with sqlite3.connect(small) as connection:
            assert connection.execute("PRAGMA integrity_check").fetchall()==[("ok",)]
        ok("INSERT INTO t VALUES (@p0,@p1)",["x"*80],mode="bulk",path=small)
        assert small.stat().st_size==allocated,"deleting a tree did not reuse reclaimed pages"
        # The schema root has 100 fewer bytes. Exercise root collapse where
        # its last child's leaf needs splitting to fit beside that header.
        names=["schema_"+str(i)+"x"*70 for i in range(20)]
        ok(";".join("CREATE TABLE "+name+"(id INTEGER PRIMARY KEY,name TEXT,other TEXT)" for name in names),path=small)
        for name in names:
            ok("DROP TABLE "+name,path=small)
            with sqlite3.connect(small) as connection:
                assert connection.execute("PRAGMA integrity_check").fetchall()==[("ok",)]

        types = root / "types.db"
        ok("CREATE TABLE v (id INTEGER PRIMARY KEY, n INTEGER, f REAL, s TEXT, b BLOB)", path=types)
        ok("INSERT INTO v VALUES (-1, -9223372036854775808, 1.25, 'a''b', X'00FF')", path=types)
        with sqlite3.connect(types) as connection:
            assert connection.execute("SELECT * FROM v").fetchone() == (-1, -9223372036854775808, 1.25, "a'b", b"\0\xff")
            connection.execute("INSERT INTO v VALUES (2,9223372036854775807,?,?,?)", (5e-324, "雪", b"big"*2000))
        assert "9223372036854775807" in ok("SELECT n FROM v WHERE id=2", mode="query", path=types)
        before = types.read_bytes()
        for sql in ["SELECT missing FROM v WHERE id=999", "SELECT n FROM v JOIN v x", "CREATE INDEX nope ON v(missing)"]:
            assert run(sql, path=types).startswith("Error"), sql
            assert types.read_bytes() == before, sql
        before = types.read_bytes()
        assert run("INSERT INTO v VALUES (3,4,5,6,7); INSERT INTO v VALUES (3,8,9,10,11)", path=types).startswith("Error")
        assert types.read_bytes() == before, "a failing multi-statement command committed its prefix"
        ok("INSERT INTO v VALUES (3,4,5,6,7); DELETE FROM v WHERE id=3;", path=types)
        invalid = root / "invalid.db"
        invalid.write_bytes(b"not sqlite")
        assert run("SELECT 1", path=invalid).startswith("Error")
        assert invalid.read_bytes() == b"not sqlite"
    print("Dark SQLite: cross-engine reads/writes, splits, overflow reuse, rollback and page-local updates passed")


if __name__ == "__main__":
    main()
