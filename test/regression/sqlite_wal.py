#!/usr/bin/env python3
"""Native SQLite/Dark WAL interoperability; production code never invokes SQLite."""
import os
import contextlib
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
from sqlite_locking import PROGRAM as LOCK_PROGRAM

PROGRAM = LOCK_PROGRAM.replace('Stdlib.Sqlite.scalarInt db "SELECT v FROM t" "v" |> Stdlib.Option.map Stdlib.Int64.toString |> Stdlib.Option.withDefault "missing"', 'match Stdlib.Sqlite.__Engine.__executeTyped db "SELECT v FROM t" (Dict {}) with\n    | Error e -> "Error " + e\n    | Ok result ->\n      match result.rows with\n      | [row] ->\n        match Stdlib.Dict.get row "v" with\n        | Some (Int n) -> Stdlib.Int64.toString n\n        | _ -> "wrong cell"\n      | _ -> "wrong rows"')

def main():
    compiler = str(Path(sys.argv[1]).resolve())
    with (contextlib.nullcontext(os.environ['SQLITE_TEST_KEEP']) if 'SQLITE_TEST_KEEP' in os.environ else tempfile.TemporaryDirectory(prefix='dark-sqlite-wal-')) as temporary:
        root = Path(temporary)
        source,binary,db=(root/n for n in ('wal.dark','wal','test.db'))
        source.write_text(PROGRAM)
        built=subprocess.run([compiler,'--allow-internal','-q',str(source),'-o',str(binary)],capture_output=True,text=True,timeout=240)
        assert built.returncode==0,built.stdout+built.stderr
        def run(sql='',mode='exec'):
            result=subprocess.run([str(binary)],env=dict(os.environ,SQLITE_TEST_DB=str(db),SQLITE_TEST_SQL=sql,SQLITE_TEST_MODE=mode),capture_output=True,text=True,timeout=30)
            assert result.returncode==0,(result.returncode,result.stdout,result.stderr)
            return result.stdout.strip()
        connection=sqlite3.connect(db)
        assert connection.execute('PRAGMA journal_mode=WAL').fetchone()==('wal',)
        connection.execute('PRAGMA wal_autocheckpoint=0')
        connection.execute('CREATE TABLE t(v INTEGER)')
        connection.execute('INSERT INTO t VALUES (1)')
        connection.commit()
        output=run(mode='query'); assert output=='1',output
        connection.execute('BEGIN IMMEDIATE')
        connection.execute('UPDATE t SET v=99')
        assert run(mode='query')=='1'
        assert run('UPDATE t SET v=2')=='Error SQLite: database is locked'
        connection.rollback()
        assert run('UPDATE t SET v=2')=='Ok 1'
        assert connection.execute('SELECT v FROM t').fetchall()==[(2,)]
        reader=sqlite3.connect(db)
        reader.execute('BEGIN')
        assert reader.execute('SELECT v FROM t').fetchall()==[(2,)]
        assert run('UPDATE t SET v=3')=='Ok 1'
        assert reader.execute('SELECT v FROM t').fetchall()==[(2,)],'Dark overwrote a native reader snapshot'
        assert connection.execute('SELECT v FROM t').fetchall()==[(3,)]
        # A native writer must understand Dark's hash entries and frame checksums.
        connection.execute('UPDATE t SET v=4')
        connection.commit()
        assert run(mode='query')=='4'
        assert reader.execute('SELECT v FROM t').fetchall()==[(2,)]
        reader.commit()
        reader.close()
        assert connection.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
        assert connection.execute('PRAGMA wal_checkpoint(TRUNCATE)').fetchall()[0][0]==0
        assert run('UPDATE t SET v=5')=='Ok 1'
        assert connection.execute('SELECT v FROM t').fetchall()==[(5,)]
        connection.close()
        # Terminating without SQLite close leaves committed frames for recovery.
        subprocess.run([sys.executable,'-c','import sqlite3,os,sys;c=sqlite3.connect(sys.argv[1]);c.execute("UPDATE t SET v=6");c.commit();os._exit(0)',str(db)],check=True)
        assert run(mode='query')=='6'
        assert run('UPDATE t SET v=7')=='Ok 1'
        connection=sqlite3.connect(db)
        assert connection.execute('SELECT v FROM t').fetchall()==[(7,)]
        assert connection.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
        connection.close()
    print('Dark SQLite WAL: native frame reads/writes, writer exclusion, reader snapshots, restart and recovery passed')


if __name__=='__main__':
    main()
