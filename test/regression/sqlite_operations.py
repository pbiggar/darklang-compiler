#!/usr/bin/env python3
"""Connection and package maintenance behavior, checked against native SQLite."""
import os
import contextlib
from pathlib import Path
import selectors
import sqlite3
import subprocess
import sys
import tempfile
import time
from sqlite_package_store import PROGRAM as QUERY_PROGRAM

PREFIX=QUERY_PROGRAM[:QUERY_PROGRAM.index('let params =')]
PROGRAM=PREFIX+'''
let db = env "SQLITE_TEST_DB"
let target = env "SQLITE_TEST_TARGET"
let output (affected: Int) (rows: List<Dict<String,Stdlib.Sqlite.Value>>) : String =
  "Ok " + Stdlib.Int.toString affected + " " + Stdlib.String.join (Stdlib.List.map rows (fun row ->
    Stdlib.String.join (Stdlib.List.map (Stdlib.Dict.toList row) (fun (k,v) -> k + "=" + valueText v)) "|")) ";"
let loop (state: Stdlib.Sqlite.__Connection.__State) : Unit =
  let command = Stdlib.Cli.Stdin.readLine ()
  if command == ":quit" || command == "" then
    let _ = Stdlib.Sqlite.__Connection.__close state
    Stdlib.printLine "Closed"
  else if command == ":backup" || command == ":restore" then
    let result = if command == ":backup" then Stdlib.Sqlite.__Maintenance.__backup db target else Stdlib.Sqlite.__Maintenance.__backup target db
    let _ = Stdlib.printLine (match result with | Ok _ -> "Copied" | Error e -> "Error " + e)
    loop state
  else
    match Stdlib.Sqlite.__Connection.__execute state command (Dict { "@payload": Stdlib.Sqlite.Value.Bytes (Stdlib.Sqlite.__Codec.__zeros 6000) }) with
    | Error e -> let _ = Stdlib.printLine ("Error " + e) in loop state
    | Ok result -> let _ = Stdlib.printLine (output result.affected result.rows) in loop result.connection
if env "SQLITE_TEST_MODE" == "initialize" then
  match Stdlib.Sqlite.__Connection.__executeTyped db "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000; CREATE TABLE t(v INTEGER); INSERT INTO t VALUES (1)" (Dict {}) with
  | Error e -> Stdlib.printLine ("Error " + e)
  | Ok result -> Stdlib.printLine (output result.affected result.rows)
else
  match Stdlib.Sqlite.__Connection.__open db with
  | Error e -> Stdlib.printLine ("Error " + e)
  | Ok state -> let _ = Stdlib.printLine "Opened" in loop state
'''


def main():
    compiler=str(Path(sys.argv[1]).resolve())
    with (contextlib.nullcontext(os.environ["SQLITE_TEST_KEEP"]) if "SQLITE_TEST_KEEP" in os.environ else tempfile.TemporaryDirectory(prefix="dark-sqlite-operations-")) as temporary:
        root=Path(temporary)
        source,binary,db,target=(root/n for n in ('operations.dark','operations','test.db','backup.db'))
        source.write_text(PROGRAM)
        built=subprocess.run([compiler,'--allow-internal','-q',str(source),'-o',str(binary)],capture_output=True,text=True,timeout=240)
        assert built.returncode==0,built.stdout+built.stderr
        env=dict(os.environ,SQLITE_TEST_DB=str(db),SQLITE_TEST_TARGET=str(target),SQLITE_TEST_MODE='initialize')
        initialized=subprocess.run([str(binary)],env=env,capture_output=True,text=True,timeout=30)
        assert initialized.returncode==0 and initialized.stdout.startswith('Ok 1'),(initialized.stdout,initialized.stderr)
        connection=sqlite3.connect(db)
        assert connection.execute('PRAGMA journal_mode').fetchone()==('wal',)
        process=subprocess.Popen([str(binary)],env=dict(env,SQLITE_TEST_MODE='hold'),stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True,bufsize=1)
        selector=selectors.DefaultSelector();selector.register(process.stdout,selectors.EVENT_READ)
        def read():
            assert selector.select(20),'Dark connection did not respond'
            line=process.stdout.readline().rstrip('\n')
            assert line,line
            return line
        def command(sql):
            process.stdin.write(sql+'\n');process.stdin.flush()
            return read()
        def rows(sql):
            result=command(sql)
            assert result.startswith('Ok '),(sql,result)
            raw=result.split(' ',2)[2]
            return [dict(cell.split('=',1) for cell in row.split('|')) for row in raw.split(';')] if raw else []
        def version():
            return int(rows('PRAGMA data_version')[0]['data_version'][1:])
        try:
            assert read()=='Opened'
            first=version();assert version()==first
            assert command('UPDATE t SET v=2')=='Ok 1 '
            assert version()==first,'a connection counted its own commit'
            # BEGIN IMMEDIATE waits for the other writer; its intervening commit
            # must remain visible to data_version even after our own commit.
            waiting_version=version()
            connection.execute('BEGIN IMMEDIATE');connection.execute('UPDATE t SET v=20')
            process.stdin.write('UPDATE t SET v=21\n');process.stdin.flush()
            time.sleep(.2)
            connection.commit()
            assert read()=='Ok 1 '
            assert version()!=waiting_version,'waiting for a writer lost its external commit'

            connection.execute('UPDATE t SET v=3');connection.commit()
            second=version();assert second!=first
            assert rows('SELECT v FROM t')==[{'v':'I3'}]
            assert command('BEGIN')=='Ok -1 '
            assert rows('SELECT v FROM t')==[{'v':'I3'}]
            connection.execute('UPDATE t SET v=4');connection.commit()
            assert rows('SELECT v FROM t')==[{'v':'I3'}],"a held Dark read snapshot changed"
            assert command('COMMIT')=='Ok -1 '
            assert rows('SELECT v FROM t')==[{'v':'I4'}]
            assert version()!=second,'read-transaction end lost an external commit'
            assert command('BEGIN IMMEDIATE')=='Ok -1 '
            assert command('UPDATE t SET v=5')=='Ok 1 '
            assert connection.execute('SELECT v FROM t').fetchall()==[(4,)]
            assert command('ROLLBACK')=='Ok -1 '
            assert rows('SELECT v FROM t')==[{'v':'I4'}]
            assert command(':backup')=='Copied'
            backup=sqlite3.connect(target)
            assert backup.execute('SELECT v FROM t').fetchall()==[(4,)]
            assert backup.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
            backup.close()
            assert command('UPDATE t SET v=6')=='Ok 1 '
            old=version()
            assert command(':restore')=='Copied'
            assert version()!=old
            assert rows('SELECT v FROM t')==[{'v':'I4'}]
            assert connection.execute('SELECT v FROM t').fetchall()==[(4,)]
            reader=sqlite3.connect(db);reader.execute('BEGIN');reader.execute('SELECT * FROM t').fetchall()
            assert command('UPDATE t SET v=7')=='Ok 1 '
            assert rows('PRAGMA busy_timeout=0')==[{'busy_timeout':'I0'}]
            assert rows('PRAGMA wal_checkpoint(TRUNCATE)')[0]['busy']=='I1'
            reader.commit();reader.close()
            assert rows('PRAGMA wal_checkpoint(TRUNCATE)')==[{'busy':'I0','log':'I0','checkpointed':'I0'}]
            assert Path(str(db)+'-wal').stat().st_size==0
            assert connection.execute('SELECT v FROM t').fetchall()==[(7,)]
            assert command('CREATE TABLE parent(id TEXT PRIMARY KEY)')=='Ok -1 '
            assert command('CREATE TABLE child(id INTEGER PRIMARY KEY,p TEXT REFERENCES parent(id))')=='Ok -1 '
            assert command("INSERT INTO child VALUES (1,'absent')")=='Ok 1 '
            assert rows('PRAGMA foreign_key_check')==[{'table':'T6368696C64','rowid':'I1','parent':'T706172656E74','fkid':'I0'}]
            assert command('PRAGMA foreign_keys=ON')=='Ok -1 '
            assert rows('SELECT count(*) AS n FROM child')==[{'n':'I1'}]
            assert command("INSERT INTO parent VALUES ('absent')")=='Ok 1 '
            assert rows('PRAGMA foreign_key_check')==[]
            assert command('PRAGMA foreign_keys=ON')=='Ok -1 '
            assert command("INSERT INTO child VALUES (2,'other')").startswith('Error SQLite: FOREIGN KEY')
            assert rows('SELECT count(*) AS n FROM child')==[{'n':'I1'}]
            assert command('PRAGMA foreign_keys=OFF')=='Ok -1 '
            assert command('CREATE TABLE spare(v BLOB)')=='Ok -1 '
            for _ in range(30):
                assert command("INSERT INTO spare VALUES (@payload)")=='Ok 1 '
            assert command('DELETE FROM spare')=='Ok 30 '
            before=int(rows('SELECT count(*) AS n FROM sqlite_schema')[0]['n'][1:])
            assert command('VACUUM')=='Ok -1 '
            assert int(rows('SELECT count(*) AS n FROM sqlite_schema')[0]['n'][1:])==before
            assert rows('SELECT v FROM t')==[{'v':'I7'}]
            assert connection.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
            assert rows('PRAGMA wal_checkpoint(TRUNCATE)')[0]['busy']=='I0'
            assert db.stat().st_size<20*4096,'VACUUM did not compact freed pages'
            assert command(':quit')=='Closed'
            process.wait(timeout=10);assert process.returncode==0,process.stderr.read()
        finally:
            if process.poll() is None:
                process.kill();process.communicate()
            selector.close();connection.close()
    print('Dark SQLite operations: configuration, held transactions/data_version, WAL checkpoint, backup/restore, foreign keys and VACUUM passed')


if __name__=='__main__':
    main()
