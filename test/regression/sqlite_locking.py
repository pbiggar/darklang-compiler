#!/usr/bin/env python3
"""Verify SQLite lock interoperability and independent descriptor ownership."""
import fcntl
import os
from pathlib import Path
import sqlite3
import struct
import subprocess
import sys
import tempfile
import time

PROGRAM = '''let env (name: String) : String = Stdlib.Env.get name |> Stdlib.Option.withDefault ""
let db = env "SQLITE_TEST_DB"
let result =
  if env "SQLITE_TEST_MODE" == "query" then
    Stdlib.Sqlite.scalarInt db "SELECT v FROM t" "v" |> Stdlib.Option.map Stdlib.Int64.toString |> Stdlib.Option.withDefault "missing"
  else if env "SQLITE_TEST_MODE" == "descriptors" then
    let opened = Stdlib.Cli.Posix.openFile db 2 384
    match opened with
    | Error _ -> "open failed"
    | Ok first ->
      let result = Stdlib.Cli.Posix.openFile db 2 384 |> Stdlib.Result.mapError (fun _ -> "open failed") |> Stdlib.Result.andThen (fun second ->
        let a = Stdlib.Sqlite.__Locks.__try first 123456 1 Stdlib.Sqlite.__Locks.__Kind.Exclusive
        let b = Stdlib.Sqlite.__Locks.__try second 123456 1 Stdlib.Sqlite.__Locks.__Kind.Exclusive
        let _ = Stdlib.Cli.Posix.fdClose second
        Stdlib.Cli.Posix.openFile db 2 384 |> Stdlib.Result.mapError (fun _ -> "open failed") |> Stdlib.Result.andThen (fun third ->
          let c = Stdlib.Sqlite.__Locks.__try third 123456 1 Stdlib.Sqlite.__Locks.__Kind.Exclusive
          let _ = Stdlib.Cli.Posix.fdClose first
          let d = Stdlib.Sqlite.__Locks.__try third 123456 1 Stdlib.Sqlite.__Locks.__Kind.Exclusive
          let _ = Stdlib.Cli.Posix.fdClose third
          Ok (a == Ok true && b == Ok false && c == Ok false && d == Ok true)))
      match result with | Ok true -> "owned" | _ -> "ownership failed"
  else
    match Stdlib.Sqlite.exec db (env "SQLITE_TEST_SQL") with
    | Error e -> "Error " + e
    | Ok n -> "Ok " + Stdlib.Int.toString n
Stdlib.printLine result
'''


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix='dark-sqlite-lock-') as temporary:
        root = Path(temporary)
        source,binary,db=(root/n for n in ('locking.dark','locking','test.db'))
        source.write_text(PROGRAM)
        built=subprocess.run([compiler,'--allow-internal','-q',str(source),'-o',str(binary)],capture_output=True,text=True,timeout=180)
        assert built.returncode==0,built.stdout+built.stderr
        connection=sqlite3.connect(db)
        connection.execute('CREATE TABLE t(v INTEGER)')
        connection.execute('INSERT INTO t VALUES (1)')
        connection.commit()
        def env(sql='',mode='exec'):
            return dict(os.environ,SQLITE_TEST_DB=str(db),SQLITE_TEST_SQL=sql,SQLITE_TEST_MODE=mode)
        def run(sql='',mode='exec'):
            result=subprocess.run([str(binary)],env=env(sql,mode),capture_output=True,text=True,timeout=15)
            assert result.returncode==0,(result.returncode,result.stdout,result.stderr)
            return result.stdout.strip()
        assert run(mode='descriptors')=='owned'
        before=db.read_bytes()
        connection.execute('BEGIN IMMEDIATE')
        connection.execute('UPDATE t SET v=99')
        blocked=run('UPDATE t SET v=2')
        assert blocked=='Error SQLite: database is locked',blocked
        assert run(mode='query')=='1','a reader observed another connection\'s uncommitted write'
        connection.rollback()
        assert db.read_bytes()==before,'a rejected writer modified the database'
        assert run('UPDATE t SET v=2')=='Ok 1'
        assert connection.execute('SELECT v FROM t').fetchall()==[(2,)]
        # Closing any descriptor for an inode releases this process's POSIX
        # locks. Keep the probe open until SQLite ends its read transaction.
        probe=db.open('rb')
        connection.execute('BEGIN')
        assert connection.execute('SELECT v FROM t').fetchall()==[(2,)]
        process=subprocess.Popen([str(binary)],env=env('UPDATE t SET v=3'),stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
        try:
            # Wait for the PENDING byte so the test synchronizes with the
            # writer's commit attempt rather than relying on a fixed sleep.
            file=probe
            deadline=time.monotonic()+4
            pending=False
            while time.monotonic()<deadline:
                if sys.platform=='darwin':
                    request,type_offset=struct.pack('qqihh',1073741824,1,0,fcntl.F_RDLCK,os.SEEK_SET),20
                else:
                    request,type_offset=struct.pack('hhqqi',fcntl.F_RDLCK,os.SEEK_SET,1073741824,1,0),0
                lock=fcntl.fcntl(file.fileno(),fcntl.F_GETLK,request)
                if struct.unpack_from('h',lock,type_offset)[0]!=fcntl.F_UNLCK:
                    pending=True
                    break
                if process.poll() is not None:
                    break
                time.sleep(.01)
            assert pending,'writer did not wait for the active SQLite reader'
            assert connection.execute('SELECT v FROM t').fetchall()==[(2,)]
            connection.commit()
            stdout,stderr=process.communicate(timeout=10)
            assert process.returncode==0 and stdout.strip()=='Ok 1',(process.returncode,stdout,stderr)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()
            if connection.in_transaction:
                connection.rollback()
            probe.close()
        assert connection.execute('SELECT v FROM t').fetchall()==[(3,)]
        assert connection.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
        connection.close()
    print('Dark SQLite: native writer exclusion, concurrent readers, pending-writer handoff and independent descriptor locks passed')


if __name__=='__main__':
    main()
