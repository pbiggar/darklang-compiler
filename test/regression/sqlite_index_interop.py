#!/usr/bin/env python3
"""Rebuild native SQLite indices in Dark; verify native traversal and integrity."""
import os
from pathlib import Path
import random
import sqlite3
import subprocess
import sys
import tempfile

PROGRAM = '''let env (name: String) : String = Stdlib.Env.get name |> Stdlib.Option.withDefault ""
let number (name: String) : Int = Stdlib.Int.parse (env name) |> Stdlib.Result.withDefault 0
let result = Stdlib.Sqlite.__Engine.__transaction (env "SQLITE_TEST_DB") (fun state ->
  Stdlib.Sqlite.__Btree.__scan state (number "SQLITE_TABLE_ROOT") |> Stdlib.Result.andThen (fun (state,entries) ->
    Stdlib.List.fold entries (Ok []) (fun result entry -> result |> Stdlib.Result.andThen (fun keys ->
      Stdlib.Sqlite.__Codec.__decodeRecord entry.payload |> Stdlib.Result.map (fun values -> keys @ [values @ [Stdlib.Sqlite.Value.Int (Stdlib.Sqlite.__Codec.__i64 entry.key)]])))
    |> Stdlib.Result.andThen (fun keys ->
      Stdlib.Sqlite.__Index.__rebuild state (number "SQLITE_INDEX_ROOT") (Stdlib.List.reverse keys)
      |> Stdlib.Result.map (fun state -> (state,Stdlib.List.length keys)))))
match result with | Ok count -> Stdlib.printLine ("Ok " + Stdlib.Int.toString count) | Error e -> Stdlib.printLine ("Error " + e)
'''


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix='dark-sqlite-index-') as temporary:
        root = Path(temporary)
        source, binary = root/'index.dark',root/'index'
        source.write_text(PROGRAM)
        built = subprocess.run([compiler,'--allow-internal','-q',str(source),'-o',str(binary)],capture_output=True,text=True,timeout=180)
        assert built.returncode == 0,built.stdout+built.stderr
        for size in (512,4096,65536):
            db = root/f'index-{size}.db'
            connection = sqlite3.connect(db)
            connection.execute(f'PRAGMA page_size={size}')
            connection.execute('CREATE TABLE t(a,b)')
            connection.execute('CREATE INDEX i ON t(a,b)')
            rows = [(None,i) if i%11==0 else (bytes([i%256])*300,i) if i%13==0 else ('key '+str(i%31)+'雪'+('long'*700 if i%7==0 else 'x'*40),i) for i in range(500)]
            random.Random(12345).shuffle(rows)
            connection.executemany('INSERT INTO t VALUES (?,?)',rows)
            connection.commit()
            table_root = connection.execute("SELECT rootpage FROM sqlite_schema WHERE name='t'").fetchone()[0]
            index_root = connection.execute("SELECT rootpage FROM sqlite_schema WHERE name='i'").fetchone()[0]
            expected = connection.execute('SELECT rowid,a,b FROM t NOT INDEXED ORDER BY a,b,rowid').fetchall()
            connection.close()
            for repeat in range(2):
                result = subprocess.run([str(binary)],capture_output=True,text=True,timeout=180,env=dict(os.environ,
                    SQLITE_TEST_DB=str(db),SQLITE_TABLE_ROOT=str(table_root),SQLITE_INDEX_ROOT=str(index_root)))
                assert result.returncode == 0 and result.stdout.strip()=='Ok 500',(size,result.stdout,result.stderr)
                with sqlite3.connect(db) as connection:
                    assert connection.execute('PRAGMA integrity_check').fetchall()==[('ok',)],(size,connection.execute('PRAGMA integrity_check').fetchall())
                    assert connection.execute('SELECT rowid,a,b FROM t INDEXED BY i ORDER BY a,b,rowid').fetchall()==expected
                    assert connection.execute("SELECT rootpage FROM sqlite_schema WHERE name='i'").fetchone()[0]==index_root
                if repeat==0:
                    first_size=db.stat().st_size
                else:
                    assert db.stat().st_size==first_size,'index rebuild did not reuse freed pages'
    print('Dark SQLite: index interior/leaf splits, mixed keys, overflow and stable-root rebuild passed at 512/4096/65536-byte pages')


if __name__=='__main__':
    main()
