"""Shared local ProgramTypes inputs for complete package resolver observations."""
import json


def observation_input(source):
    cases=[]
    missing=object()
    def add(op, value=missing, **fields):
        if value is not missing: fields['json']=json.dumps(value,ensure_ascii=False,separators=(', ', ': '))
        cases.append(dict(op=op,**fields))
    def union(name,*fields): return {name:list(fields)}
    def expression(name,*fields): return union(name,17,*fields)
    def location(name='name'): return dict(owner='Owner',modules=['M','N'],name=name)
    def resolved(loc=True): return dict(originalName=['Original','name'],resolved=union('Ok',dict(name=union('Package',union('Hash','hash')),location=union('Some',location()) if loc else union('None'))))
    primitive_types=[union(name) for name in ['TUnit','TBool','TInt8','TUInt8','TInt16','TUInt16','TInt32','TUInt32','TInt64','TUInt64','TInt128','TUInt128','TInt','TFloat','TChar','TString','TDateTime','TUuid','TBlob']]
    types=primitive_types+[union('TVariable',source),union('TList',primitive_types[0]),union('TStream',primitive_types[1]),union('TDB',primitive_types[2]),union('TDict',primitive_types[0],primitive_types[1]),union('TTuple',primitive_types[0],primitive_types[1],[primitive_types[2]]),union('TFn',primitive_types[:2],primitive_types[2]),union('TCustomType',resolved(),[]),union('TCustomType',resolved(False),primitive_types[:2])]
    for typ in types:
        add('renderType',typ)
        add('renderType',union('TList',typ))
    for name in ['TUnit','TBool','TList','TStream','TDB','TDict','TTuple','TFn','TCustomType','unknown']:
        for fields in [[],[None],[1,2,3]]: add('renderType',union(name,*fields))
    lets=[expression('LPUnit'),expression('LPWildcard'),expression('LPVariable',source)]
    lets+=[expression('LPTuple',lets[0],lets[1],lets[:])]
    for value in lets+[{},None,union('LPVariable',1),expression('unknown')]: add('renderLetPattern',value)
    scalar_names=['Int8','UInt8','Int16','UInt16','Int32','UInt32','Int64','UInt64','Int128','UInt128','Int']
    patterns=[expression('MPVariable',source),expression('MPUnit'),expression('MPBool',True),expression('MPBool',False),expression('MPString',source),expression('MPChar',source),expression('MPChar',"'\\\n")]+[expression('MP'+name, -(2**127) if name.startswith('Int') else 2**128-1) for name in scalar_names]
    patterns += [expression('MPList',patterns[:]),expression('MPListCons',patterns[0],patterns[1]),expression('MPTuple',patterns[0],patterns[1],patterns[2:4]),expression('MPEnum',source,[]),expression('MPEnum',source,patterns[:2]),expression('MPOr',patterns[:2])]
    for pattern in patterns+[{},None,expression('unknown'),expression('MPBool',1)]: add('renderMatchPattern',pattern)
    infixes=[union('BinOp',union('BinOpAnd')),union('BinOp',union('BinOpOr'))]+[union('InfixFnCall',union(name,999)) for name in ['ArithmeticPlus','ArithmeticMinus','ArithmeticMultiply','ArithmeticDivide','ArithmeticModulo','ArithmeticPower','ComparisonGreaterThan','ComparisonGreaterThanOrEqual','ComparisonLessThan','ComparisonLessThanOrEqual','ComparisonEquals','ComparisonNotEquals','StringConcat']]
    for value in infixes+[union('BinOp',union('other')),union('InfixFnCall',union('other')),union('unknown'),{},None]: add('infixText',value)
    for value in [location(),dict(owner='Owner',modules=[],name=source),{},dict(owner=1,modules=['A'],name='B'),dict(owner='Owner',modules=1,name='B')]: add('locationName',value)
    names=[resolved(),resolved(False),dict(originalName=['A',source],resolved=union('Error','failed')),dict(originalName=['A','B'],resolved=union('Ok',dict(location=union('Other')))),{},dict(originalName=[],resolved=union('Other')),dict(originalName=[],resolved=union('Ok',{}))]
    for value in names: add('resolvedName',value)
    for value in [union('Hash',source),union('Hash',1),union('Other'),{},[],None]: add('parseHashJson',value)
    base=expression('EInt64',42)
    expressions=[expression('EUnit'),expression('EBool',True),expression('EBool',False)]+[expression('E'+name,-(2**127) if name.startswith('Int') else 2**128-1) for name in scalar_names]
    expressions += [expression('EFloat',union('Negative'),'0','0'),expression('EFloat',union('Positive'),'123','5'),expression('EChar',source),expression('EString',[union('StringText',source),union('StringInterpolation',base)]),expression('EVariable',source),expression('EArg',0),expression('EArg',1),expression('EArg',-1),expression('EArg',2147483648),expression('EArg',1.0),expression('ESelf'),expression('EFnName',resolved()),expression('EValue',resolved(False)),expression('EList',[base,base]),expression('ETuple',base,base,[base]),expression('EDict',[[base,base]]),expression('ELet',lets[3],base,base),expression('EIf',base,base,union('Some',base)),expression('EIf',base,base,union('None')),expression('EApply',expression('EFnName',resolved()),types[:2],[base,base]),expression('EApply',expression('EVariable','f'),[],[]),expression('ELambda',lets[:],base),expression('ERecord',resolved(),types[:2],[[source,base]]),expression('ERecordFieldAccess',base,source),expression('ERecordUpdate',base,[[source,base]]),expression('EEnum',resolved(),types[:2],source,[]),expression('EEnum',resolved(),[],source,[base]),expression('EMatch',base,[dict(pat=patterns[0],whenCondition=union('None'),rhs=base),dict(pat=patterns[1],whenCondition=union('Some',base),rhs=base)]),expression('EStatement',base,base)]
    expressions += [expression('EInfix',value,base,base) for value in infixes]
    pipes=[expression('EPipeVariable',source,[]),expression('EPipeVariable',source,[base]),expression('EPipeLambda',lets[:],base),expression('EPipeInfix',infixes[0],base),expression('EPipeFnCall',resolved(),types[:2],[base]),expression('EPipeEnum',resolved(),source,[]),expression('EPipeEnum',resolved(),source,[base])]
    expressions += [expression('EPipe',base,pipes),expression('EPipe',base,[])]
    for value in expressions:
        add('renderExpr',value,params=[source,'second'],self='Owner.M.self')
        add('renderExpr',expression('EList',[value]),params=[source,'second'],self='Owner.M.self')
    errors=[{},None,expression('unknown'),expression('EArg','bad'),expression('EBool',1),expression('EDict',[[base]]),expression('ERecord',resolved(),[],[[1]]),expression('ERecordUpdate',base,[[1]]),expression('EIf',base,base,union('Other')),expression('EMatch',base,[{}]),expression('EMatch',base,[dict(pat=patterns[0],whenCondition=union('Other'),rhs=base)]),expression('EString',[union('Other')]),expression('EPipe',base,[expression('Other')])]
    for value in errors: add('renderExpr',value,params=[],self='Owner.M.self')
    for kind in ['PackageType','PackageValue','PackageFunction']:
        for value in [dict(entity={},location=location()),dict(entity={},location={}),{},dict(entity={})]: add('parseLocatedEntity',value,kind=kind,hash='hash')
    dependency=dict(name=union('Package',union('Hash','hash')),location=union('Some',location()))
    dependencies=[dependency,dict(a=dependency,b=[dependency,dict(name=union('Package',union('Hash','other')),location=union('Some',location('other')))]),dict(name=union('Package',union('Hash',1)),location=union('Some',location())),{},[]]
    for value in dependencies: add('dependencyRefs',value)
    entities=[]
    for value in expressions:
        entities.append(('PackageValue',dict(body=value)))
        entities.append(('PackageFunction',dict(parameters=[dict(name=source,typ=types[0]),dict(name='second',typ=types[1])],typeParams=['a'],returnType=types[2],body=value)))
    for typ in types: entities.append(('PackageType',dict(declaration=dict(typeParams=['a'],definition=union('Alias',typ)))))
    entities += [('PackageType',dict(declaration=dict(typeParams=[],definition=union('Record',[dict(name=source,typ=types[0])])))),('PackageType',dict(declaration=dict(typeParams=[],definition=union('Enum',[dict(name='None',fields=[]),dict(name='Some',fields=[dict(typ=types[0]),dict(typ=types[1])])]))))]
    for kind,entity in entities: add('renderEntity',dict(entity=entity),kind=kind,hash='hash',location='Owner.M.name')
    for kind in ['PackageType','PackageValue','PackageFunction']:
        for entity in [{},dict(parameters=[{}],typeParams=[],returnType=types[0],body=base),dict(declaration={}),dict(declaration=dict(typeParams=[],definition=union('Record',[{}]))),dict(declaration=dict(typeParams=[],definition=union('Enum',[{}]))),dict(declaration=dict(typeParams=[],definition=union('Enum',[dict(name='Some',fields=[{}])]))),dict(declaration=dict(typeParams=[],definition=union('Other'))),dict(body=expression('EBool',1))]: add('renderEntity',dict(entity=entity),kind=kind,hash='hash',location='Owner.M.name')
    for loc in ['name','','Owner.M.name']: add('renderEntity',dict(entity=dict(body=base)),kind='PackageValue',hash='hash',location=loc)
    for known in [[],['A.B','M.f'],['A.B.C']]: add('candidatePrefixes',names=['A','A.B.C',source,'M.f','A.B.C.D'],known=known)
    add('defaults')
    add('cache',text=source)
    # No HTTP calls: exercise the same ByteArrayContent decoding boundary locally.
    for charset in [None,'utf-8','"utf-8"','"iso_646.irv:1991"','"iso_8859-1:1987"','utf8','unicode','utf-16be','utf-32','utf-32be','ascii','iso-8859-1','latin1','windows-1252','utf-7','invalid','""'] + ['ansi_x3.4-1968', 'ansi_x3.4-1986', 'ascii', 'cp367', 'cp819', 'csascii', 'csisolatin1', 'csunicode11utf7', 'ibm367', 'ibm819', 'iso-10646-ucs-2', 'iso-8859-1', 'iso-ir-100', 'iso-ir-6', 'iso646-us', 'iso8859-1', 'iso_646.irv:1991', 'iso_8859-1', 'iso_8859-1:1987', 'l1', 'latin1', 'ucs-2', 'unicode', 'unicode-1-1-utf-7', 'unicode-1-1-utf-8', 'unicode-2-0-utf-7', 'unicode-2-0-utf-8', 'unicodefffe', 'us', 'us-ascii', 'utf-16', 'utf-16be', 'utf-16le', 'utf-32', 'utf-32be', 'utf-32le', 'utf-7', 'utf-8', 'x-unicode-1-1-utf-7', 'x-unicode-1-1-utf-8', 'x-unicode-2-0-utf-7', 'x-unicode-2-0-utf-8']:
        for data in [b'',b'hello',bytes([0x61,0xff,0x62]),bytes.fromhex('fffe4100'),bytes.fromhex('0000feff00000041')]: add('content',charset=charset,bytes=data.hex())
    return json.dumps(cases,ensure_ascii=False,separators=(',',':'))
