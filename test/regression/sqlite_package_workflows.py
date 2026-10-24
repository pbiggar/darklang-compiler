#!/usr/bin/env python3
"""SQL-level package workflows using pinned interpreter statements and typed bodies.

Opaque serialized bodies exercise SQL/storage, not the F# serializer or CLI host.
"""
import datetime
import json
import os
import re
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
from sqlite_package_corpus import statements
from sqlite_package_differential import snapshot,equivalent,difference
from sqlite_package_store import PROGRAM,encoded
from sqlite_package_templates import concrete_templates,PROJECTIONS


def main():
    compiler,fixtures=map(lambda p:Path(p).resolve(),sys.argv[1:3])
    corpus=json.loads((fixtures/'corpus.json').read_text())
    def source(file,line):return next(e['sql'] for e in corpus['entries'] if Path(e['source']).name==file and e['line']==line)
    def content(table):return next(e['sql'] for e in concrete_templates(corpus) if Path(e['source']).name=='PackageOpPlayback.fs' and e['line']==168 and e['sql'].startswith('INSERT INTO '+table+' '))
    with tempfile.TemporaryDirectory(prefix='dark-package-workflows-') as temporary:
        root=Path(temporary);code,binary,native_path,dark_path,export=(root/n for n in ['workflow.dark','workflow','native.db','dark.db','export.db'])
        program=PROGRAM.replace('Stdlib.Sqlite.__Engine.__executeTyped','Stdlib.Sqlite.__Connection.__executeTyped')
        index=program.index('match Stdlib.Sqlite.__Connection.__executeTyped')
        program=program[:index]+'''if env "SQLITE_TEST_MODE" == "backup" then
  match Stdlib.Sqlite.__Maintenance.__backup (env "SQLITE_TEST_DB") (env "SQLITE_TEST_TARGET") with
  | Error error -> Stdlib.printLine ("Error " + error)
  | Ok _ -> Stdlib.printLine "Copied"
else
'''+''.join('  '+line+'\n' for line in program[index:].splitlines())
        code.write_text(program)
        built=subprocess.run([str(compiler),'--allow-internal','-q',str(code),'-o',str(binary)],capture_output=True,text=True,timeout=240)
        assert built.returncode==0,built.stdout+built.stderr
        native=sqlite3.connect(native_path);native.executescript((fixtures/'schema.sql').read_text())
        native.execute('PRAGMA journal_mode=WAL');native.execute('PRAGMA wal_autocheckpoint=0')
        # Create the second unchanged schema through Dark, then keep a native
        # observer attached to its WAL throughout authoring and replay.
        def run(sql='',params=None,path=dark_path,mode='exec',target=''):
            params=params or {}
            text='\n'.join(k+'='+('N:' if v is None else 'B:'+v.hex() if isinstance(v,bytes) else 'T:'+v.encode().hex() if isinstance(v,str) else 'I:'+str(v)) for k,v in params.items())
            result=subprocess.run([str(binary)],capture_output=True,text=True,timeout=120,env=dict(os.environ,SQLITE_TEST_DB=str(path),SQLITE_TEST_SQL=sql,SQLITE_TEST_PARAMS=text,SQLITE_TEST_MODE=mode,SQLITE_TEST_TARGET=str(target)))
            assert result.returncode==0,(result.returncode,result.stdout,result.stderr)
            return result.stdout.rstrip('\n')
        assert run((fixtures/'schema.sql').read_text())=='Ok 4'
        observer=sqlite3.connect(dark_path);observer.execute('PRAGMA journal_mode=WAL');observer.execute('PRAGMA wal_autocheckpoint=0')
        # Seed account timestamps are independent clock reads. Normalize only this
        # fixture setup; every subsequent write is compared without rewriting it.
        native.execute("UPDATE accounts_v0 SET created_at='2020-01-01 00:00:00'");native.commit()
        assert run("UPDATE accounts_v0 SET created_at='2020-01-01 00:00:00'")=='Ok 4'
        count=0
        clock_pairs=set()
        def step(sql,params=None):
            nonlocal count
            params=params or {}
            required={prefix+name for prefix,name in re.findall(r'([@:$])([A-Za-z_][A-Za-z_0-9]*)',sql)}
            assert required<=params.keys(),(sql,required-params.keys())
            before=datetime.datetime.now(datetime.UTC).replace(tzinfo=None)
            affected=-1
            native.execute('BEGIN IMMEDIATE')
            try:
                for statement in statements(sql):
                    cursor=native.execute(statement,{k[1:]:v for k,v in params.items()})
                    if cursor.rowcount>=0:affected=max(0,affected)+cursor.rowcount
                    cursor.fetchall()
                native.commit()
            except Exception:native.rollback();raise
            output=run(sql,params)
            assert output=='Ok '+str(affected),(sql,output,affected)
            after=datetime.datetime.now(datetime.UTC).replace(tzinfo=None)
            expected,actual=snapshot(native),snapshot(observer)
            assert equivalent(actual,expected,before,after,clock_pairs),difference(actual,expected)
            assert observer.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
            count+=1
            if count%5==0:print(f"Package workflow steps: {count} passed",flush=True)
        def op(id,applied=1):
            step(source('Inserts.fs',221),{'@id':id,'@op_blob':b'\x00'+id.encode()+b'\xff'*6000,'@applied':applied,'@origin_ts':'2020-01-01T00:00:01.000Z','@commit_hash':None})
        def store(hash):
            step(content('package_functions'),{'$hash':hash,'$pt_def':b'pt\x00'+hash.encode(),'$rt_instrs':b'instructions\xff'*1000,'$debug_symbols':b'debug\x00','$description':'function description'})
        def bind(location,hash,opid,previous=None):
            base={'$owner':'Darklang','$modules':'Example','$name':'a','$item_type':'fn'}
            step(source('PackageOpPlayback.fs',514),base)
            step(source('PackageOpPlayback.fs',529),dict(base,**{'$location_id':location,'$item_hash':hash,'$origin_ts':'2020-01-01T00:00:01.000Z','$source':'op','$op_id':opid,'$previous':previous}))
        op('author-a');store('fn-a');bind('loc-a','fn-a','author-a')
        step(source('Config.fs',39),{'@key':'schema-stamp','@value':'v1'})
        step(source('Docs.fs',613),{'$owner':'Darklang','$modules':'Example','$name':'a','$kind':'item','$within':'','$text':'first doc','$ts':'1'})
        step(source('Docs.fs',613),{'$owner':'Darklang','$modules':'Example','$name':'a','$kind':'item','$within':'','$text':'updated doc','$ts':'2'})
        step(source('Inserts.fs',710),{'@hash':'commit-a','@message':'author function','@origin_ts':'1'})
        step(source('Inserts.fs',720),{'@hash':'commit-a'})
        step(source('Branches.fs',43),{'@id':'branch-work','@name':'work','@parent':'00000000-0000-0000-0000-000000000001'})
        step(source('Branches.fs',250),{'@id':'branch-op','@op_blob':b'branch\x00\xff','@origin_ts':'2'})
        step(source('Branches.fs',257),{'@op_id':'branch-op','@branch_id':'branch-work','@source':'op'})
        store('fn-b')
        assert observer.execute('SELECT item_hash FROM locations WHERE unlisted_at IS NULL').fetchall()==[('fn-a',)]
        assert observer.execute("SELECT id FROM package_ops WHERE effective=0").fetchall()==[('branch-op',)]
        step(source('branches.dark',775),{'@p0':'branch-work'})
        bind('loc-b','fn-b','branch-op','fn-a')
        step(source('branches.dark',796)+';'+source('branches.dark',797),{'@p0':'branch-work'})
        assert observer.execute('SELECT item_hash FROM locations WHERE unlisted_at IS NULL').fetchall()==[('fn-b',)]
        assert observer.execute('SELECT count(*) FROM op_branches').fetchone()==(0,)
        step(source('Inserts.fs',710),{'@hash':'commit-b','@message':'merge branch','@origin_ts':'2'})
        step(source('Inserts.fs',720),{'@hash':'commit-b'})
        # A failing second statement must not publish the first one's dirty pages.
        before=snapshot(observer)
        failed=run("UPDATE config_v0 SET value='should roll back'; INSERT INTO package_functions(hash,pt_def,rt_instrs) VALUES ('fn-a',X'00',X'00')")
        assert failed.startswith('Error '),failed
        assert snapshot(observer)==before
        logical=lambda c:c.execute('SELECT location_id,item_hash,op_id,previous,unlisted_at IS NULL FROM locations ORDER BY location_id').fetchall()
        projection_before=logical(observer)
        clear=';'.join('DELETE FROM '+table for table in PROJECTIONS)+';UPDATE package_ops SET applied=0'
        step(clear)
        store('fn-a');bind('loc-a','fn-a','author-a');store('fn-b');bind('loc-b','fn-b','branch-op','fn-a')
        step('UPDATE package_ops SET applied=1')
        assert logical(observer)==projection_before,'projection replay changed logical bindings'
        assert run(path=dark_path,mode='backup',target=export)=='Copied'
        exported=sqlite3.connect(export)
        assert exported.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
        assert logical(exported)==projection_before
        exported.close()
        assert run(source('Seed.fs',92),path=export).startswith('Ok ')
        assert run('VACUUM',path=export)=='Ok -1'
        assert run(path=export,mode='backup',target=dark_path)=='Copied'
        assert observer.execute('SELECT count(*) FROM locations').fetchone()==(0,)
        assert observer.execute('SELECT count(*) FROM package_ops WHERE applied=0').fetchone()==(2,)
        assert observer.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
        # Migration discovery/addition and rebuilding an expression index survive
        # restoration while observers still hold their original database inode.
        assert run(source('Config.fs',39),{'@key':'restored-probe','@value':'v'})=='Ok 1'
        assert run("ALTER TABLE config_v0 ADD COLUMN restored INTEGER DEFAULT '3'")=='Ok -1'
        assert run("SELECT restored,typeof(restored) AS kind FROM config_v0",mode='query')=='Ok kind=T696E7465676572|restored=I3'
        assert run('DROP INDEX idx_package_ops_applied;CREATE INDEX idx_package_ops_applied ON package_ops(applied) WHERE applied=0')=='Ok -1'
        assert observer.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
        observer.close();native.close()
    print(f'Package SQL workflows: {count} differential authoring/commit/branch/merge/replay steps, atomic failure, full-schema export/restore and migration passed')


if __name__=='__main__':main()
