#!/usr/bin/env python3
"""Execute every package mutation shape against independent native/Dark stores."""
import datetime
import json
import os
from pathlib import Path
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from sqlite_package_corpus import statements
from sqlite_package_store import PROGRAM
from sqlite_package_templates import concrete_templates

STAMP='2020-01-01 00:00:00'


def fixture(path,schema):
    connection=sqlite3.connect(path)
    connection.executescript(schema)
    tables=[row[0] for row in connection.execute("SELECT name FROM sqlite_schema WHERE type='table'")]
    for table in tables:
        if connection.execute(f'SELECT count(*) FROM "{table}"').fetchone()[0]:continue
        values={}
        for _,name,type_,notnull,default,primary in connection.execute(f'PRAGMA table_info("{table}")'):
            if default is not None:continue
            if not notnull and not primary:continue
            value={'hash':'fn-a','id':'op-a','op_id':'op-a','branch_id':'branch-test','parent_id':'parent-test','item_hash':'fn-a','depends_on_hash':'fn-a','depends_on_item_type':'fn','owner':'Darklang','modules':'Example','name':'a','account_id':'00000000-0000-0000-0000-000000000001','origin_ts':STAMP,'execution_date':STAMP,'created_at':STAMP,'status':'pending','candidates':'[]','kind':'item','item_type':'fn','item_kind':'fn'}.get(name,'seed')
            if 'BLOB' in type_.upper():value=b'\x00\xffpayload'
            if 'INT' in type_.upper():value=1
            values[name]=value
        connection.execute('INSERT INTO "'+table+'" ('+','.join('"'+name+'"' for name in values)+') VALUES ('+','.join('?' for _ in values)+')',tuple(values.values()))
    # Several eligible rows, with ties and NULLs, make membership and scoped
    # mutations observable instead of merely validating an empty-table path.
    connection.execute("UPDATE branches SET id='branch-test',parent_id='parent-test',name='work'")
    connection.execute("INSERT INTO branches(id,parent_id,name) VALUES ('parent-test','00000000-0000-0000-0000-000000000001','parent')")
    connection.execute("UPDATE package_ops SET applied=0,effective=1,commit_hash=NULL,created_at=?,origin_ts=?",(STAMP,STAMP))
    connection.execute("INSERT INTO package_ops(id,op_blob,applied,effective,commit_hash,created_at,origin_ts) VALUES ('op-b',X'AB',0,0,'fn-a',?,?)",(STAMP,STAMP))
    connection.execute("INSERT OR IGNORE INTO op_branches(op_id,branch_id,source) VALUES ('op-b','branch-test','op')")
    connection.execute("UPDATE locations SET unlisted_at=NULL,op_id='op-a',item_hash='fn-a',origin_ts=?,created_at=?,source='op'",(STAMP,STAMP))
    connection.commit()
    assert connection.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
    assert connection.execute('PRAGMA foreign_key_check').fetchall()==[]
    connection.close()


def bindings(sql,fresh=False):
    result={}
    for prefix,name in re.findall(r'([@:$])([A-Za-z_][A-Za-z_0-9]*)',sql):
        lower=name.lower()
        value={'hash':'fn-a','id':'op-a','op':'op-a','op_id':'op-a','e':'op-a','b':'branch-test','p':'parent-test','parent':'parent-test','branch':'branch-test','branch_id':'branch-test','owner':'Darklang','modules':'Example','name':'a','item_hash':'fn-a','itemhash':'fn-a','item_type':'fn','item_kind':'fn','kind':'item','within':'','source':'op','status':'resolved','origin_ts':STAMP,'ts':STAMP,'at':STAMP,'created_at':STAMP,'op_blob':b'\x00\xffnew','blob':b'\x00\xffnew','bytes':b'\x00\xffnew','pt_def':b'\x00\xffpt','rt_def':b'rt','rt_instrs':b'instructions','rt_dval':b'value','value_type':b'type','debug_symbols':b'debug','annotation_blob':b'annotation','length':5,'applied':1,'candidates':'[]','ids':'["op-a","op-b"]','key':'setting','value':'new','description':'new description','commit_hash':None,'commit':'fn-a','trait_hash':'fn-a','p0':'op-a','p1':'branch-test','p2':'Darklang','p3':'Example','p4':'a'}.get(lower,'new')
        if re.fullmatch(r'(hash|kind|owner|modules|name)_\d+',lower):value={'hash':'fn-a','kind':'fn','owner':'Darklang','modules':'Example','name':'a'}[lower.rsplit('_',1)[0]]
        if fresh and lower in ['hash','id','op_id','p0']:value='fresh'
        result[prefix+name]=value
    for prefix,name in re.findall(r'json_each\(\s*([@:$])([A-Za-z_][A-Za-z_0-9]*)\s*\)',sql,re.I):result[prefix+name]='["op-a","op-b"]'
    return result


def canonical_sql(sql):
    if sql is None:return None
    tokens=re.findall(r"X'(?:''|[^'])*'|'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\"|`[^`]*`|--[^\n]*|/\*.*?\*/|[A-Za-z_][A-Za-z_0-9]*|[0-9]+|>=|<=|<>|!=|\|\||[^\s]",sql,re.S)
    tokens=[token if token.startswith(("'",'"','`')) else token.upper() for token in tokens if not token.startswith(('--','/*'))]
    for index in range(min(5,len(tokens)-2)):
        if tokens[index:index+3]==['IF','NOT','EXISTS']:
            del tokens[index:index+3]
            break
    return tokens


def snapshot(connection):
    catalog=connection.execute('SELECT type,name,tbl_name,sql FROM sqlite_schema ORDER BY type,name').fetchall()
    catalog=[(kind,name,table,canonical_sql(sql)) for kind,name,table,sql in catalog]
    rows={}
    for type_,name,_,_ in catalog:
        if type_=='table':rows[name]=connection.execute('SELECT rowid,* FROM "'+name+'" ORDER BY rowid').fetchall()
    return catalog,rows


def timestamp(value):
    if not isinstance(value,str):return None
    try:return datetime.datetime.fromisoformat(value.removesuffix('Z')).replace(tzinfo=None)
    except ValueError:return None


def equivalent(actual,expected,before,after,clock_pairs=None):
    if isinstance(actual,(tuple,list)) and isinstance(expected,(tuple,list)):
        return len(actual)==len(expected) and all(equivalent(a,b,before,after,clock_pairs) for a,b in zip(actual,expected))
    if isinstance(actual,dict) and isinstance(expected,dict):
        return actual.keys()==expected.keys() and all(equivalent(actual[k],expected[k],before,after,clock_pairs) for k in actual)
    if actual==expected:return True
    if clock_pairs is not None and isinstance(actual,str) and isinstance(expected,str) and (actual,expected) in clock_pairs:return True
    a,b=timestamp(actual),timestamp(expected)
    if a is None or b is None:return False
    # CURRENT_TIMESTAMP, datetime(now), and default millisecond strftime may
    # legitimately cross a clock tick between the two processes.
    lower=before-datetime.timedelta(seconds=1)
    valid=lower<=a<=after and lower<=b<=after and len(actual)==len(expected)
    if valid and clock_pairs is not None:clock_pairs.add((actual,expected))
    return valid


def difference(actual,expected,path='database'):
    if isinstance(actual,(tuple,list)) and isinstance(expected,(tuple,list)) and len(actual)==len(expected):
        for index,(a,b) in enumerate(zip(actual,expected)):
            if a!=b:return difference(a,b,path+'['+str(index)+']')
    if isinstance(actual,dict) and isinstance(expected,dict) and actual.keys()==expected.keys():
        for key in actual:
            if actual[key]!=expected[key]:return difference(actual[key],expected[key],path+'.'+key)
    return (path,actual,expected)

def main():
    compiler,fixtures=map(lambda p:Path(p).resolve(),sys.argv[1:3])
    corpus=json.loads((fixtures/'corpus.json').read_text())
    with tempfile.TemporaryDirectory(prefix='dark-package-differential-') as temporary:
        root=Path(temporary);source,binary,seed,native_db,dark_db=(root/n for n in ['query.dark','query','seed.db','native.db','dark.db'])
        source.write_text(PROGRAM.replace('Stdlib.Sqlite.__Engine.__executeTyped','Stdlib.Sqlite.__Connection.__executeTyped'))
        if 'SQLITE_TEST_DRIVER' in os.environ:binary=Path(os.environ['SQLITE_TEST_DRIVER']).resolve()
        else:
            compiled=subprocess.run([str(compiler),'--allow-internal','-q',str(source),'-o',str(binary)],capture_output=True,text=True,timeout=240)
            assert compiled.returncode==0,compiled.stdout+compiled.stderr
        fixture(seed,(fixtures/'schema.sql').read_text())
        cases=[e for e in corpus['entries'] if e['kind']=='statement']+concrete_templates(corpus)
        cases=[e for e in cases if not re.match(r'^(SELECT|WITH)\b',e['sql'],re.I)]
        failures=[];passed=0;successful=set()
        for entry in cases:
            # PRAGMA counters/configuration and maintenance have held-connection
            # tests. Compare their resulting database here, not arbitrary local
            # counter values or a multi-command execute-reader's final rowset.
            for fresh in (False,True):
                for db in (native_db,dark_db):
                    for suffix in ('','-wal','-shm'):
                        Path(str(db)+suffix).unlink(missing_ok=True)
                    shutil.copyfile(seed,db)
                params=bindings(entry['sql'],fresh)
                before=datetime.datetime.now(datetime.UTC).replace(tzinfo=None)
                native=sqlite3.connect(native_db)
                try:
                    affected=-1
                    for sql in statements(entry['sql']):
                        cursor=native.execute(sql,{k[1:]:v for k,v in params.items()})
                        if cursor.rowcount>=0:affected=max(0,affected)+cursor.rowcount
                        cursor.fetchall()
                    native.commit();expected=snapshot(native);native_error=None
                except sqlite3.Error as error:
                    native.rollback();native_error=str(error);expected=snapshot(native)
                params_text='\n'.join(k+'='+('N:' if v is None else 'B:'+v.hex() if isinstance(v,bytes) else 'T:'+v.encode().hex() if isinstance(v,str) else 'I:'+str(v)) for k,v in params.items())
                result=subprocess.run([str(binary)],capture_output=True,text=True,timeout=60,env=dict(os.environ,SQLITE_TEST_DB=str(dark_db),SQLITE_TEST_SQL=entry['sql'],SQLITE_TEST_PARAMS=params_text,SQLITE_TEST_MODE='exec'))
                after=datetime.datetime.now(datetime.UTC).replace(tzinfo=None)
                output=result.stdout.rstrip('\n')
                try:
                    assert result.returncode==0,(result.returncode,output,result.stderr)
                    if native_error is not None:
                        assert output.startswith('Error '),(output,native_error)
                    else:
                        assert output=='Ok '+str(affected),(output,affected)
                        successful.add((entry['source'],entry['line']))
                    dark=sqlite3.connect(dark_db)
                    try:
                        assert dark.execute('PRAGMA integrity_check').fetchall()==[('ok',)]
                        actual=snapshot(dark)
                        assert equivalent(actual,expected,before,after),difference(actual,expected)
                    finally:dark.close()
                except (AssertionError,sqlite3.Error) as error:
                    failures.append(dict(source=entry['source'],line=entry['line'],variant=entry.get('variant'),fresh=fresh,sql=entry['sql'],error=str(error)[:3000]))
                else:passed+=1
                if failures and len(failures)<=3:print(str(failures[-1])[:600],flush=True)
                if (passed+len(failures))%50==0:print(f"Mutation cases: {passed} passed, {len(failures)} failed",flush=True)
                native.close()
        missing=[(e['source'],e['line']) for e in cases if (e['source'],e['line']) not in successful]
        report=dict(revision=corpus['interpreter_revision'],passed=passed,failed=len(failures),never_succeeded=sorted(set(missing)),failures=failures)
        destination=Path.cwd()/'TestResults/sqlite-package-mutation-audit.json';destination.parent.mkdir(exist_ok=True);destination.write_text(json.dumps(report,indent=2)+'\n')
        print(f'Package mutation differential: {passed} passed, {len(failures)} failed; {len(set(missing))} source sites without a successful case',flush=True)
        assert not failures,failures[:3]
        assert not missing,sorted(set(missing))


if __name__=='__main__':main()
