#!/usr/bin/env python3
"""Exchange the interpreter's unchanged indexed package schema with the Dark reader."""
import json
import datetime
import os
import re
from pathlib import Path
import sqlite3
import struct
import shutil
import subprocess
import sys
import tempfile
from sqlite_package_templates import concrete_templates

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
| Ok result -> if env "SQLITE_TEST_MODE" == "exec" then Stdlib.printLine ("Ok " + Stdlib.Int.toString result.affected) else Stdlib.printLine ("Ok " + Stdlib.String.join (Stdlib.List.map result.rows (fun row ->
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


def has_order(sql):
    depth, previous = 0, ''
    tokens = re.findall(r"'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\"|--[^\n]*|/\*.*?\*/|[()]|[A-Za-z_][A-Za-z0-9_]*",sql,re.S)
    for token in tokens:
        if token=='(':
            depth += 1
        elif token==')':
            depth -= 1
        elif depth==0 and token[0].isalpha():
            if previous=='ORDER' and token.upper()=='BY':
                return True
            previous=token.upper()
    return False


def sample_bindings(entry):
    names = re.findall(r'[@:$]([A-Za-z_][A-Za-z_0-9]*)',entry['sql'])
    bindings = {}
    for name in names:
        lower = name.lower()
        value = {'owner':'Darklang','modules':'Example','name':'a','item_type':'fn','hash':'fn-a','item_hash':'fn-a','itemhash':'fn-a','value_type':b'type','valuetype':b'type','p0':'branch-test','p1':'00000000-0000-0000-0000-000000000001','branch':'parent-test','c':'effective','searchtext':'a','directchildpattern':'Example.%','fqname':'Darklang.Example'}.get(lower,'0')
        if re.fullmatch(r'(h|hash|target_hash_)\d+',lower):value='fn-a'
        if re.fullmatch(r'id\d+',lower):value='dyn-branch'
        if re.fullmatch(r'k\d+',lower):value='unicode'
        if re.fullmatch(r'tok\d+',lower):value='a'
        if re.fullmatch(r'(owner|modules|name|kind)\d+',lower):value={'owner':'Darklang','modules':'Example','name':'a','kind':'fn'}[re.sub(r'\d+$','',lower)]
        if re.fullmatch(r'key\d+',lower):value='Darklang\x00Example\x00a'+('\x00item\x00' if entry['source'].endswith('/Inserts.fs') and entry['line']==130 else '')
        if lower.startswith('target_hash_kind_'):value='fn'
        bindings['@'+name] = value
    # Python binds by name; the Dark API preserves the prefix too.
    bindings = {prefix+name:bindings['@'+name] for prefix,name in re.findall(r'([@:$])([A-Za-z_][A-Za-z_0-9]*)',entry['sql'])}
    for prefix,name in re.findall(r'json_each\(\s*([@:$])([A-Za-z_][A-Za-z_0-9]*)\s*\)',entry['sql'],re.I):bindings[prefix+name]='["fn-a","type-a"]'
    if re.search(r"json_extract\(j\.value,\s*'\$\[0\]'",entry['sql']):
        fourth='fn' if entry['source'].endswith('/propagation.dark') else 'fn-a'
        bindings['@p0']=json.dumps([['Darklang','Example','a',fourth]])
    for limit_name in re.findall(r'\bLIMIT\s+([@:$][A-Za-z_][A-Za-z_0-9]*)',entry['sql'],re.I):bindings[limit_name]=3
    return bindings


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    fixtures = Path(sys.argv[2]).resolve()
    with tempfile.TemporaryDirectory(prefix='dark-package-sqlite-') as temporary:
        root = Path(temporary)
        source, binary, db = (root / name for name in ('query.dark', 'query', 'packages.db'))
        source.write_text(PROGRAM)
        if 'SQLITE_TEST_DRIVER' in os.environ:binary=Path(os.environ['SQLITE_TEST_DRIVER']).resolve()
        else:
            built = subprocess.run([compiler, '--allow-internal', '-q', str(source), '-o', str(binary)],
                                   capture_output=True, text=True, timeout=240)
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
        connection.execute("INSERT INTO package_ops(id,op_blob,origin_ts,applied) VALUES ('audit-op',X'00','2020-01-01T00:00:00.000Z',1)")
        connection.commit()

        def run(sql, bindings=None, mode='query', path=db):
            bindings = bindings or {}
            params = '\n'.join(k + '=' + ('N:' if v is None else ('B:' + v.hex() if isinstance(v,bytes) else 'T:' + v.encode().hex() if isinstance(v,str) else 'I:' + str(v) if isinstance(v,int) else 'R:' + repr(v))) for k,v in bindings.items())
            result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=90,
                                    env=dict(os.environ, SQLITE_TEST_DB=str(path), SQLITE_TEST_SQL=sql, SQLITE_TEST_PARAMS=params, SQLITE_TEST_MODE=mode))
            assert result.returncode == 0, (sql, result.stdout, result.stderr)
            return result.stdout.rstrip('\n')

        def check(sql, bindings=None, ordered=True):
            bindings = bindings or {}
            cursor = connection.execute(sql, {k[1:]: v for k, v in bindings.items()})
            names = [column[0] for column in cursor.description]
            expected = [dict(zip(names, map(encoded, row))) for row in cursor]
            output = run(sql, bindings)
            assert output.startswith('Ok '), (sql,output)
            actual = [dict(cell.split('=',1) for cell in row.split('|')) for row in output[3:].split(';')] if output[3:] else []
            if re.fullmatch(r"SELECT\s+datetime\('now'\)\s+AS\s+now",sql.strip(),re.I):
                after = connection.execute(sql).fetchone()[0]
                assert len(actual)==1 and set(actual[0])=={'now'}
                stamp = lambda encoded:datetime.datetime.strptime(bytes.fromhex(encoded[1:]).decode(),'%Y-%m-%d %H:%M:%S')
                observed, before, after = stamp(actual[0]['now']),stamp(expected[0]['now']),datetime.datetime.strptime(after,'%Y-%m-%d %H:%M:%S')
                assert min(before,after)<=observed<=max(before,after),(sql,actual,expected,after)
                return
            if not ordered:
                actual.sort(key=lambda row:json.dumps(row,sort_keys=True))
                expected.sort(key=lambda row:json.dumps(row,sort_keys=True))
            assert actual == expected, (sql, actual, expected)

        def execute(sql, bindings=None, count=1):
            output = run(sql,bindings,'exec')
            assert output == f'Ok {count}', (sql,output)
            try:
                integrity = connection.execute('PRAGMA integrity_check').fetchall()
                assert integrity == [('ok',)], integrity
            except (sqlite3.DatabaseError, AssertionError):
                destination = Path.cwd()/'TestResults/sqlite-package-failed.db'
                destination.parent.mkdir(exist_ok=True)
                shutil.copyfile(db,destination)
                raise

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
        execute('INSERT INTO package_types(hash,pt_def,rt_def) VALUES ($hash,$pt,$rt)', {'$hash':'type-b','$pt':b'pt-b','$rt':b'rt-b'*5000})
        assert connection.execute("SELECT rt_def FROM package_types WHERE hash='type-b'").fetchone()==(b'rt-b'*5000,)
        assert connection.execute("SELECT created_at,description FROM package_types WHERE hash='type-b'").fetchone()[1]==''
        execute('INSERT OR IGNORE INTO package_types(hash,pt_def,rt_def) VALUES ($hash,$pt,$rt)', {'$hash':'type-b','$pt':b'other','$rt':b'other'},count=0)
        execute('UPDATE package_values SET value_type=@type WHERE hash=@hash', {'@hash':'value-a','@type':b'changed'})
        assert connection.execute('SELECT hash FROM package_values INDEXED BY idx_package_values_type WHERE value_type=?',(b'changed',)).fetchall()==[('value-a',)]
        execute("INSERT INTO package_ops(id,op_blob) VALUES ('op-a',X'00FF'),('op-b',X'01')",count=2)
        execute("UPDATE package_ops SET applied=1 WHERE id='op-a'")
        assert connection.execute('SELECT id FROM package_ops INDEXED BY idx_package_ops_applied WHERE applied=0').fetchall()==[('op-b',)]
        execute("INSERT INTO package_dependencies VALUES ('fn-a','type-a','type',NULL,NULL,NULL)")
        execute("INSERT OR IGNORE INTO package_dependencies VALUES ('fn-a','type-a','type','','','')",count=0)
        before = db.read_bytes()
        duplicate = run("INSERT INTO package_dependencies VALUES ('fn-a','type-a','type','','','')",mode='exec')
        assert duplicate.startswith('Error '),duplicate
        assert db.read_bytes()==before,'unique expression index failure changed the file'
        if '--audit' in sys.argv:
            connection.execute("INSERT INTO branches(id,name,parent_id) VALUES ('branch-test','work','parent-test'),('parent-test','parent','00000000-0000-0000-0000-000000000001')")
            connection.execute("INSERT INTO commits(hash,message,origin_ts,parent) VALUES ('dyn-commit','main','2020-01-01T00:00:01.000Z',''),('dyn-branch-commit','branch','2020-01-01T00:00:02.000Z','dyn-commit')")
            connection.execute("INSERT INTO package_ops(id,op_blob,applied,effective,origin_ts,commit_hash,created_at) VALUES ('dyn-main',X'0100',1,1,'2020-01-01T00:00:01.000Z','dyn-commit','2020-01-01 00:00:00'),('dyn-branch',X'0200',0,0,'2020-01-01T00:00:02.000Z','dyn-branch-commit','2020-01-01 00:00:01'),('dyn-draft',X'0300',0,1,'2020-01-01T00:00:03.000Z',NULL,'2020-01-01 00:00:02')")
            connection.execute("INSERT INTO op_branches(op_id,branch_id,source) VALUES ('dyn-branch','parent-test','op')")
            connection.execute("INSERT INTO package_dependencies VALUES ('caller-a','old-hash','fn','Darklang','Example','a'),('caller-b','caller-a','fn',NULL,NULL,NULL),('caller-a','caller-b','fn',NULL,NULL,NULL)")
            connection.commit()
            failures, passed = [], 0
            read_cases = [entry for entry in corpus['entries'] if entry['kind']=='statement'] + concrete_templates(corpus)
            for entry in read_cases:
                if not re.match(r'^(SELECT|WITH)\b',entry['sql'],re.I):
                    continue
                bindings=sample_bindings(entry)
                try:
                    check(entry['sql'],bindings,ordered=has_order(entry['sql']))
                except (AssertionError,sqlite3.Error) as error:
                    failures.append({'source':entry['source'],'line':entry['line'],'sql':entry['sql'],'error':str(error)[:2000]})
                else:
                    passed += 1
                if (passed+len(failures))%50==0:print(f'Read cases: {passed} passed, {len(failures)} failed',flush=True)
            report = {'revision':corpus['interpreter_revision'],'passed':passed,'failed':len(failures),'failures':failures,'qualification':'Concrete reads plus source-driven variants of all 54 dynamic templates; read data and binding samples, not full F# host execution'}
            destination=Path.cwd()/'TestResults/sqlite-package-query-audit.json'
            destination.parent.mkdir(exist_ok=True)
            destination.write_text(json.dumps(report,indent=2,ensure_ascii=False)+'\n')
            print(f"Source query audit: {passed} passed, {len(failures)} failed; {destination}",flush=True)
            assert not failures,failures[:3]
            connection.execute("DELETE FROM package_dependencies WHERE item_hash IN ('caller-a','caller-b')")
            connection.execute("DELETE FROM op_branches WHERE op_id='dyn-branch'")
            connection.execute("DELETE FROM package_ops WHERE id IN ('dyn-main','dyn-branch','dyn-draft')")
            connection.execute("DELETE FROM commits WHERE hash IN ('dyn-commit','dyn-branch-commit')")
            connection.execute("DELETE FROM branches WHERE id IN ('branch-test','parent-test')")
            connection.commit()
        # Run the interpreter's literal UPSERTs, retaining their $/@ bindings,
        # qualified predicates and excluded-column references.
        def source_sql(path, line):
            return next(entry['sql'] for entry in corpus['entries'] if entry['source'].endswith('/'+path) and entry['line']==line)
        config_sql = source_sql('Config.fs',39)
        execute(config_sql,{'@key':'setting','@value':'old'})
        execute(config_sql,{'@key':'setting','@value':'new'})
        assert connection.execute("SELECT value FROM config_v0 WHERE key='setting'").fetchone()==('new',)
        op_sql = source_sql('Inserts.fs',221)
        op_params = {'@id':'op-a','@op_blob':b'received','@applied':1,'@origin_ts':'2026-10-24T00:00:00.000Z','@commit_hash':None}
        execute(op_sql,op_params,count=0)
        execute("UPDATE package_ops SET effective=0 WHERE id='op-a'")
        execute(op_sql,op_params)
        assert connection.execute("SELECT effective,applied,op_blob,origin_ts,commit_hash FROM package_ops WHERE id='op-a'").fetchone()==(1,0,b'\x00\xff',op_params['@origin_ts'],None)
        docs_sql = source_sql('Docs.fs',613)
        doc_params = {'$owner':'Darklang','$modules':'Example','$name':'a','$kind':'item','$within':'','$text':'first','$ts':'1'}
        execute(docs_sql,doc_params)
        doc_params.update({'$text':'second','$ts':'2'})
        execute(docs_sql,doc_params)
        assert connection.execute('SELECT text,origin_ts FROM location_docs').fetchall()==[('second','2')]
        # The actual branch/replay writes use JSON membership, a scalar
        # subquery, INSERT SELECT, then a scoped DELETE.
        execute("INSERT INTO op_branches(op_id,branch_id,source) VALUES ('op-b','branch-test','authored')")
        execute("UPDATE package_ops SET effective=0 WHERE id='op-b'")
        playback = {'$ids':'["op-b"]','$b':'branch-test','$e':'op-a','$p':'parent-test'}
        execute(source_sql('PackageOpPlayback.fs',772),playback)
        execute("UPDATE package_ops SET commit_hash='commit-a' WHERE id='op-a'")
        execute(source_sql('PackageOpPlayback.fs',788),playback)
        assert connection.execute("SELECT commit_hash,effective FROM package_ops WHERE id='op-b'").fetchone()==('commit-a',1)
        execute(source_sql('PackageOpPlayback.fs',804),playback)
        execute(source_sql('PackageOpPlayback.fs',804),playback,count=0)
        execute(source_sql('PackageOpPlayback.fs',818),playback)
        assert connection.execute("SELECT branch_id,source FROM op_branches WHERE op_id='op-b'").fetchall()==[('parent-test','authored')]
        next_rowid = connection.execute('SELECT max(rowid)+1 FROM config_v0').fetchone()[0]
        execute("INSERT OR REPLACE INTO config_v0(key,value) VALUES ('setting','replaced')")
        assert connection.execute("SELECT rowid,value FROM config_v0 WHERE key='setting'").fetchone()==(next_rowid,'replaced')
        execute("DELETE FROM package_types WHERE hash='type-b'")
        # The migration SQL discovers columns through the eponymous pragma table.
        check("SELECT cid,name,type,\"notnull\",dflt_value,pk FROM pragma_table_info('config_v0') ORDER BY cid")
        execute("ALTER TABLE config_v0 ADD COLUMN engine_probe INTEGER NOT NULL DEFAULT '3'",count=-1)
        check('SELECT engine_probe,typeof(engine_probe) AS storage FROM config_v0 ORDER BY key')
        check("SELECT 1 AS n FROM pragma_table_info('config_v0') WHERE name=@c",{'@c':'engine_probe'})
        before=db.read_bytes()
        assert run('ALTER TABLE config_v0 ADD COLUMN bad TEXT NOT NULL',mode='exec').startswith('Error ')
        assert db.read_bytes()==before,'failed migration changed pages'
        execute('DROP INDEX idx_package_values_type',count=-1)
        execute('CREATE INDEX idx_package_values_type ON package_values(value_type)',count=-1)
        check('SELECT key,value FROM json_each(@list) ORDER BY key',{'@list':'[9007199254740993,null,true,"snow 雪"]'})
        check("SELECT key,json_extract(value,'$[0]') AS a FROM json_each(@list) ORDER BY key",{'@list':'[["hash-a"],["hash-b"]]'})
        created = root/'dark-created.db'
        output = run((fixtures/'schema.sql').read_text(),mode='exec',path=created)
        assert output=='Ok 4',output  # Four account seed rows; DDL contributes no count.
        with sqlite3.connect(created) as native:
            assert native.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
            expected = connection.execute("SELECT type,name,tbl_name FROM sqlite_schema WHERE type IN ('table','index') ORDER BY name").fetchall()
            # ALTER above changes columns, not catalog object identity.
            assert native.execute("SELECT type,name,tbl_name FROM sqlite_schema WHERE type IN ('table','index') ORDER BY name").fetchall()==expected
            assert native.execute('SELECT count(*) FROM accounts_v0').fetchone()==(4,)
        connection.close()
    print(f'Dark SQLite: {len(reads)} interpreter runtime-load query templates, typed cells, schema creation and maintained package indices passed')


if __name__ == '__main__':
    main()
