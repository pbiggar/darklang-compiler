"""Concrete variants of every dynamic SQL source entry at the pinned revision.

These are source-driven constructions, not a claim of instrumenting the F# host.
The inventory assertion prevents an unclassified template from silently passing.
"""
import itertools
from pathlib import Path

CONTENT={
 'package_types':(['pt_def','rt_def','description'],[],'type'),
 'package_values':(['pt_def','description'],['rt_dval','value_type'],'value'),
 'package_functions':(['pt_def','rt_instrs','debug_symbols','description'],[],'fn'),
 'package_traits':(['pt_def','description'],[],'trait'),
 'package_trait_impls':(['pt_def','trait_hash','description','origin_ts'],[],'impl'),
 'package_tests':(['pt_def','description'],[],'test'),
}
DRAFT='effective = 1 AND commit_hash IS NULL AND id NOT IN (SELECT op_id FROM op_branches)'
MAIN='effective = 1 AND id NOT IN (SELECT op_id FROM op_branches)'
CHAIN='WITH RECURSIVE chain(bid) AS (SELECT @p0 UNION SELECT b.parent_id FROM branches b JOIN chain c ON b.id=c.bid WHERE b.parent_id != @p1)'
VISIBLE=['id NOT IN (SELECT op_id FROM op_branches)',f'(id NOT IN (SELECT op_id FROM op_branches) OR id IN (SELECT op_id FROM op_branches WHERE branch_id IN ({CHAIN} SELECT bid FROM chain)))']
OWNED=['p.effective = 1 AND p.id NOT IN (SELECT op_id FROM op_branches)','p.id IN (SELECT op_id FROM op_branches WHERE branch_id=@p0)']
DRAFTS=['p.commit_hash IS NULL AND p.effective=1 AND p.id NOT IN (SELECT op_id FROM op_branches)','p.commit_hash IS NULL AND p.id IN (SELECT op_id FROM op_branches WHERE branch_id=@p0)']
PARENTS=['p.commit_hash IS NOT NULL AND p.effective=1 AND p.id NOT IN (SELECT op_id FROM op_branches)',f'p.commit_hash IS NOT NULL AND (p.id IN (SELECT op_id FROM op_branches WHERE branch_id IN ({CHAIN} SELECT bid FROM chain)) OR (p.effective=1 AND p.id NOT IN (SELECT op_id FROM op_branches)))']
PURGE=['locations',*CONTENT,'package_ops','package_dependencies','deprecations','location_docs','conflicts','op_branches','branch_name_bases','seed_ops']
PROJECTIONS=[*CONTENT,'locations','package_dependencies','deprecations']


def variants(entry):
    file,line,sql=Path(entry['source']).name,entry['line'],entry['sql']
    key=(file,line)
    def replace(**values):
        result=sql
        for name,value in values.items():result=result.replace('{'+name+'}',value)
        return result
    if key in [('Branches.fs',291),('Branches.fs',315)]:return [replace(clause=', '.join('@id'+str(i) for i in range(n))) for n in (1,3)]
    if key==('Config.fs',28):return [replace(names=', '.join('@k'+str(i) for i in range(n))) for n in (1,3)]
    if key in [('Docs.fs',343),('Inserts.fs',62),('Inserts.fs',130)]:return [replace(keyClause=', '.join('@key'+str(i) for i in range(n))) for n in (1,3)]
    if key in [('Docs.fs',446),('PackageOpPlayback.fs',105)]:return [replace(table=t) for t in CONTENT]
    if key in [('Inserts.fs',437),('Inserts.fs',439)]:return [replace(draftWhere=DRAFT)]
    if key==('Inserts.fs',458):return [replace(mainWhere=MAIN,keepUnreadable=keep) for keep in (''," AND id NOT IN ('unreadable','other')")]
    if key==('Inserts.fs',497):return [replace(where=predicate) for predicate in (DRAFT,MAIN)]
    if key==('PackageOpPlayback.fs',60):return [sql+' '+','.join('($item_hash,'+','.join('$'+c+'_'+str(i) for c in ['hash','kind','owner','modules','name'])+')' for i in range(n)) for n in (1,3)]
    if key in [('PackageOpPlayback.fs',168),('PackageOpPlayback.fs',189)]:
        result=[]
        for table,(columns,nulls,_) in CONTENT.items():
            result.append(replace(table=table,insertColumns=','.join(['hash',*columns,*nulls]),insertValues=','.join(['$hash',*('$'+c for c in columns),*('NULL' for _ in nulls)]),assignments=','.join(c+'=$'+c for c in columns)))
        return result
    if key in [('ProgramTypes.fs',28),('ProgramTypes.fs',77),('ProgramTypes.fs',110)]:return [replace(itemType=kind) for _,_,kind in CONTENT.values()]
    if key==('ProgramTypes.fs',58):return [replace(table=t,lookupColumn='hash') for t in CONTENT]
    if key==('ProgramTypes.fs',258):
        return [replace(submoduleCondition=p) for p in ["((modules LIKE '%' || @searchText || '%') OR (owner || '.' || modules LIKE '%' || @searchText || '%'))","((modules LIKE @directChildPattern AND modules LIKE '%' || @searchText || '%') OR (owner || '.' || modules LIKE @directChildPattern AND owner || '.' || modules LIKE '%' || @searchText || '%'))"]]
    if key==('ProgramTypes.fs',344):
        locations=['1=1',"((l.modules=@modules) OR (l.owner || '.' || l.modules=@fqname))","((l.modules=@modules) OR (l.owner || '.' || l.modules=@fqname) OR (l.modules LIKE @modules || '.%') OR (l.owner || '.' || l.modules LIKE @fqname || '.%'))"]
        names=["l.name=@searchText","((l.owner || '.' || l.modules || '.' || l.name)=@searchText OR (l.owner || '.' || l.modules || '.' || l.name) LIKE '%.' || @searchText)","(l.owner || '.' || l.modules || '.' || l.name) LIKE '%' || @searchText || '%'","(l.name LIKE '%' || @searchText || '%' OR c.description LIKE '%' || @searchText || '%')","l.name LIKE '%' || @searchText || '%'"]
        names += [' AND '.join("((l.owner || '.' || l.modules || '.' || l.name) LIKE '%%' || @tok"+str(i)+" || '%%' OR c.description LIKE '%%' || @tok"+str(i)+" || '%%')" for i in range(n)) for n in (2,3)]
        return [replace(joinColumn='hash')+f" FROM locations l JOIN {table} c ON l.item_hash=c.hash WHERE l.unlisted_at IS NULL AND l.item_type='{kind}' AND ({location}) AND {name} ORDER BY l.owner,l.modules,l.name" for table,(_,_,kind) in CONTENT.items() for location,name in itertools.product(locations,names)]
    if key==('Purge.fs',71):return [replace(table=t) for t in PURGE]
    if key in [('Queries.fs',73),('Queries.fs',432)]:return [replace(hashInClause=clause) for clause in ('@h0','@h0,@h1,@h2')]
    if key==('Queries.fs',169):return [replace(locTuples=','.join('(@kind'+str(i)+',@owner'+str(i)+',@modules'+str(i)+',@name'+str(i)+')' for i in range(n)),hashFallbackClause=fallback) for n in (1,3) for fallback in ('0', '(pd.depends_on_item_type,pd.depends_on_hash) IN ((@target_hash_kind_0,@target_hash_0),(@target_hash_kind_1,@target_hash_1))')]
    if key==('Queries.fs',224):return [replace(inClause=clause) for clause in ('@h0','@h0,@h1,@h2')]
    if key==('Queries.fs',269):return [replace(**{'Inserts.draftWhere':DRAFT})]
    if key==('Queries.fs',348):return [replace(**{'Inserts.mainWhere':MAIN})]
    if key==('Releases.fs',34):return [replace(table=t) for t in ['package_ops','locations','system_migrations_v0']]
    if key==('Releases.fs',62):return [replace(table='config_v0',column='migration_probe',declaration=d) for d in ["TEXT DEFAULT ''",'INTEGER NOT NULL DEFAULT 0','BLOB NULL']]
    if key==('Seed.fs',278):return [replace(quoted=clause) for clause in ("'op-a'","'op-a','op-b'")]
    if key==('Seed.fs',400):return [replace(t=t) for t in PROJECTIONS]
    if key==('conflicts.dark',432):return [sql+clause+')' for clause in ("'a'","'a','other'")]
    if key==('packageOpsBindings.dark',201):return [sql+' SELECT bid FROM chain']
    if key==('packageOpsBindings.dark',215):return [CHAIN+' '+sql]
    if key==('packageOpsCommits.dark',15):return [sql+' '+str(n) for n in (0,1,100)]
    if key==('packageOpsCommits.dark',73):return ['SELECT id FROM package_ops WHERE '+VISIBLE[1]]
    if key==('packageOpsCommits.dark',104):return [sql+' '+p for p in VISIBLE]
    if key==('packageOpsCommits.dark',113):return [sql+' '+p+' ORDER BY rowid DESC LIMIT 5' for p in VISIBLE]
    if key==('packageOpsCommits.dark',180):return [sql+' '+p+' ORDER BY id' for p in VISIBLE]
    if key==('packageOpsCommits.dark',203):return [replace(inList=clause) for clause in ("'op-a'","'op-a','op-b'")]
    if key==('packageOpsCommits.dark',241):return [sql+' (applied=1 OR id IN (SELECT op_id FROM op_branches))']
    if key==('packageOpsCommits.dark',252):return [sql+' (applied=1 OR id IN (SELECT op_id FROM op_branches))']
    if key==('packageOpsCommits.dark',273):return [sql+' '+p+')' for p in OWNED]
    if key==('packageOpsCommits.dark',297):return [sql+' '+p+') ORDER BY c.rowid DESC LIMIT @p'+str(i) for i,p in enumerate(OWNED)]
    if key==('packageOpsCommits.dark',331):return ['SELECT p.id FROM package_ops p WHERE '+PARENTS[1]]
    if key==('packageOpsCommits.dark',339):return [sql+' '+p+') ORDER BY c.origin_ts DESC,c.rowid DESC LIMIT 1' for p in PARENTS]
    if key in [('packageOpsDraft.dark',34),('packageOpsDraft.dark',68),('packageOpsDraft.dark',88)]:return [sql+' '+p+(' ORDER BY p.id' if line==68 else ' ORDER BY p.origin_ts,p.rowid' if line==88 else '') for p in DRAFTS]
    if key==('packageOpsDraft.dark',396):return [sql+' @p'+str(i)+' WHERE id IN (SELECT p.id FROM package_ops p WHERE '+p+')' for i,p in enumerate(DRAFTS)]
    if key==('packageOpsDraft.dark',449):return [sql+' @p'+str(i)+' WHERE id=@p'+str(i+1)+' AND id IN (SELECT p.id FROM package_ops p WHERE '+p+')' for i,p in enumerate(DRAFTS)]
    if key==('packageOpsRepoints.dark',74):return [sql+clause+')' for clause in ("'fn-a'","'fn-a','type-a'")]
    if key==('partialCommit.dark',96):return [replace(quoted=clause) for clause in ("'fn-a'","'fn-a','type-a'")]
    raise AssertionError(('unclassified SQL template',key,sql))


def concrete_templates(corpus):
    result=[]
    for entry in corpus['entries']:
        if entry['kind']=='statement':continue
        statements=variants(entry)
        assert statements,entry
        result.extend(dict(entry,sql=sql,variant=i) for i,sql in enumerate(statements))
    assert len({(e['source'],e['line']) for e in result})==54
    return result
