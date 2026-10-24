#!/usr/bin/env python3
"""Exchange the interpreter's unchanged indexed package schema with the Dark reader."""
import json
import os
from pathlib import Path
import sqlite3
import struct
import subprocess
import sys
import tempfile

PROGRAM = '''let env (key: String) : String = Stdlib.Env.get key |> Stdlib.Option.withDefault ""
let valueText (value: Stdlib.Sqlite.Value) : String =
  match value with
  | Null -> "N"
  | Int i -> "I" + Stdlib.Int64.toString i
  | Real f -> "R" + Stdlib.UInt64.toString (Stdlib.Float.toBits f)
  | Text s -> "T" + Stdlib.Blob.toHex (Stdlib.Blob.fromString s)
  | Bytes b -> "B" + Stdlib.Blob.toHex b
let decode (text: String) : Stdlib.Sqlite.Value =
  let parts = Stdlib.String.splitFirst text ":"
  match parts with
  | Some (("T",hex)) -> Stdlib.Sqlite.Value.Text (Stdlib.Blob.fromHex hex |> Stdlib.Result.andThen Stdlib.Blob.toString |> Stdlib.Result.withDefault "")
  | Some (("B",hex)) -> Stdlib.Sqlite.Value.Bytes (Stdlib.Blob.fromHex hex |> Stdlib.Result.withDefault Stdlib.Blob.empty)
  | Some (("I",integer)) -> Stdlib.Sqlite.Value.Int (Stdlib.Int64.parse integer |> Stdlib.Result.withDefault 0L)
  | Some (("R",real)) -> Stdlib.Sqlite.Value.Real (Stdlib.Float.parse real |> Stdlib.Result.withDefault 0.0)
  | _ -> Stdlib.Sqlite.Value.Null
let params = env "SQLITE_TEST_PARAMS"
let bindings = if params == "" then Dict {} else
  Stdlib.Dict.fromListOverwritingDuplicates (Stdlib.List.map (Stdlib.String.split params "\\n") (fun line ->
    match Stdlib.String.splitFirst line "=" with
    | Some ((name,value)) -> (name,decode value)
    | None -> (line,Stdlib.Sqlite.Value.Null)))
match Stdlib.Sqlite.__Engine.__executeTyped (env "SQLITE_TEST_DB") (env "SQLITE_TEST_SQL") bindings with
| Error e -> Stdlib.printLine ("Error " + e)
| Ok result -> Stdlib.printLine ("Ok " + Stdlib.String.join (Stdlib.List.map result.rows (fun row ->
  Stdlib.String.join (Stdlib.List.map (Stdlib.Dict.toList row) (fun (k,v) -> k + "=" + valueText v)) "|")) ";")
'''


def encoded(value):
    if value is None:
        return 'N'
    if isinstance(value, int):
        return 'I' + str(value)
    if isinstance(value, float):
        return 'R' + str(struct.unpack('>Q', struct.pack('>d', value))[0])
    if isinstance(value, bytes):
        return 'B' + value.hex().upper()
    return 'T' + value.encode().hex().upper()


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    fixtures = Path(sys.argv[2]).resolve()
    with tempfile.TemporaryDirectory(prefix='dark-package-sqlite-') as temporary:
        root = Path(temporary)
        source, binary, db = (root / name for name in ('query.dark', 'query', 'packages.db'))
        source.write_text(PROGRAM)
        built = subprocess.run([compiler, '--allow-internal', '-q', str(source), '-o', str(binary)],
                               capture_output=True, text=True, timeout=180)
        assert built.returncode == 0, built.stdout + built.stderr
        connection = sqlite3.connect(db)
        connection.executescript((fixtures / 'schema.sql').read_text())
        connection.execute("INSERT INTO package_types(hash,pt_def,rt_def) VALUES (?,?,?)", ('type-a', b'pt\x00', b'rt\xff' * 4000))
        connection.execute("INSERT INTO package_values(hash,pt_def,rt_dval,value_type) VALUES (?,?,?,?)", ('value-a', b'pt', b'value\x00', b'type'))
        connection.execute("INSERT INTO package_values(hash,pt_def) VALUES (?,?)", ('value-null', b'pt'))
        connection.execute("INSERT INTO package_functions(hash,pt_def,rt_instrs,debug_symbols) VALUES (?,?,?,?)", ('fn-a', b'pt', b'instructions\xff', b'debug\x00'))
        connection.execute("INSERT INTO package_blobs(hash,length,bytes) VALUES (?,?,?)", ('blob-a', 4, b'\x00\x80\xff\x01'))
        connection.execute("INSERT INTO locations(location_id,item_hash,owner,modules,name,item_type) VALUES (?,?,?,?,?,?)", ('location-a','fn-a','Darklang','Example','a','fn'))
        # Added columns have virtual defaults in old record payloads. Include one
        # record with decomposed text: SQL must preserve it rather than normalize.
        connection.execute("INSERT INTO config_v0(key,value) VALUES (?,?)", ('unicode', 'e\u0301雪'))
        connection.execute("ALTER TABLE config_v0 ADD COLUMN compatibility_probe TEXT DEFAULT 'old-row-default'")
        connection.commit()

        def check(sql, bindings=None):
            bindings = bindings or {}
            cursor = connection.execute(sql, {k[1:]: v for k, v in bindings.items()})
            names = [column[0] for column in cursor.description]
            expected = [dict(zip(names, map(encoded, row))) for row in cursor]
            params = '\n'.join(k + '=' + ('N:' if v is None else ('B:' + v.hex() if isinstance(v,bytes) else 'T:' + v.encode().hex() if isinstance(v,str) else 'I:' + str(v) if isinstance(v,int) else 'R:' + repr(v))) for k,v in bindings.items())
            result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=90,
                                    env=dict(os.environ, SQLITE_TEST_DB=str(db), SQLITE_TEST_SQL=sql, SQLITE_TEST_PARAMS=params))
            assert result.returncode == 0, (sql, result.stdout, result.stderr)
            output = result.stdout.rstrip('\n')
            assert output.startswith('Ok '), (sql,output)
            actual = [dict(cell.split('=',1) for cell in row.split('|')) for row in output[3:].split(';')] if output[3:] else []
            assert actual == expected, (sql, actual, expected)

        corpus = json.loads((fixtures / 'corpus.json').read_text())
        reads = [entry for entry in corpus['entries'] if entry['source'].endswith('/RuntimeTypes.fs') and entry['kind']=='statement' and entry['sql'].lstrip().upper().startswith('SELECT')]
        for entry in reads:
            sql = entry['sql']
            # These are the actual source queries, without rewriting @hash or
            # replacing typed BLOB comparisons with string casts.
            for hash_ in ['type-a','value-a','value-null','fn-a','blob-a','missing']:
                check(sql, {'@hash':hash_, '@valueType':b'type', '@value_type':b'type'})
        check('SELECT compatibility_probe FROM config_v0 WHERE key=@key', {'@key':'unicode'})
        check('SELECT value, length(value) AS n, substr(value,2,1) AS mark FROM config_v0 WHERE key=@key', {'@key':'unicode'})
        check("SELECT coalesce(NULL,'fallback') AS c, CASE WHEN 1=2 THEN 'bad' ELSE 'yes' END AS k, 'a' || char(0) || 'b' AS text")
        check("SELECT json_extract('[9007199254740993,null,true,\"snow 雪\"]','$[0]') AS n, json_extract('[null]','$[0]') AS nil")
        check("SELECT substr(X'00010203',-2,2) AS tail, length(X'00FF') AS n, 'Æ' LIKE 'æ' AS unicode, 'Ab雪' LIKE 'a_%' AS ascii")
        check('SELECT @blob AS b, @int AS n, @real AS r, @nil AS nil', {'@blob':b'\x00\xff','@int':42,'@real':1.25,'@nil':None})
        # Indexes must remain untouched until their write maintenance is implemented.
        before = db.read_bytes()
        rejected = subprocess.run([str(binary)],capture_output=True,text=True,timeout=90,
                                  env=dict(os.environ,SQLITE_TEST_DB=str(db),SQLITE_TEST_SQL="DELETE FROM package_types",SQLITE_TEST_PARAMS=''))
        assert rejected.returncode == 0 and rejected.stdout.startswith('Error '), rejected
        assert db.read_bytes() == before
        assert connection.execute('PRAGMA integrity_check').fetchone() == ('ok',)
        connection.close()
    print(f'Dark SQLite: {len(reads)} interpreter runtime-load query templates, typed cells and unchanged package schema passed')


if __name__ == '__main__':
    main()
